use crate::{errors::Error, project::Project, sway, translate::*};
use convert_case::Case;
use num_bigint::BigUint;
use num_traits::Zero;
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_storage_name(
    _project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    name: &str,
) -> String {
    if !translated_definition.storage_fields_names.contains_key(name) {
        let mut new_name = translate_naming_convention(name, Case::Snake);

        let count = translated_definition.storage_fields_name_counts.entry(new_name.clone()).or_insert(0);
        *count += 1;

        if *count > 1 {
            new_name = format!("{new_name}_{}", *count);
        }

        translated_definition.storage_fields_names.insert(name.into(), new_name);
    }

    translated_definition.storage_fields_names.get(name).unwrap().clone()
}

#[inline]
pub fn translate_state_variable(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    variable_definition: &solidity::VariableDefinition,
) -> Result<(), Error> {
    // Collect information about the variable from its attributes
    let is_public = variable_definition.attrs.iter().any(|x| matches!(x, solidity::VariableAttribute::Visibility(solidity::Visibility::External(_) | solidity::Visibility::Public(_))));
    let is_constant = variable_definition.attrs.iter().any(|x| matches!(x, solidity::VariableAttribute::Constant(_)));
    let is_immutable = variable_definition.attrs.iter().any(|x| matches!(x, solidity::VariableAttribute::Immutable(_)));

    // If the state variable is not constant or immutable, it is a storage field
    let storage_namespace = if !is_constant && !is_immutable {
        Some(translate_naming_convention(&translated_definition.name, Case::Snake))
    } else {
        None
    };

    let is_storage = storage_namespace.is_some();

    // If the state variable is immutable and not a constant, it is a configurable field
    let is_configurable = is_immutable && !is_constant;

    // Translate the variable's naming convention
    let old_name = variable_definition.name.as_ref().unwrap().name.clone();
    let new_name = if is_constant || is_immutable {
        translate_naming_convention(old_name.as_str(), Case::Constant)
    } else {
        translate_storage_name(project, translated_definition, old_name.as_str())
    };

    // Translate the variable's type name
    let mut variable_type_name = translate_type_name(project, translated_definition, &variable_definition.ty, is_storage, false);
    let mut abi_type_name = None;

    match &variable_definition.ty {
        solidity::Expression::MemberAccess(_, container, member) => {
            match container.as_ref() {
                solidity::Expression::Variable(name) => {
                    if let Some(external_definition) = project.translated_definitions.iter().find(|d| d.name == name.name) {
                        if let Some(external_struct) = external_definition.structs.iter().find(|s| s.borrow().name == member.name) {
                            translated_definition.ensure_struct_included(project, external_struct);
                        }
                    }
                }

                _ => {}
            }
        }
        _ => {}
    }

    // Check if the variable's type is an ABI
    if let sway::TypeName::Identifier { name, generic_parameters: None } = &variable_type_name {
        // Check if type is a contract that hasn't been defined yet
        if project.find_definition_with_abi(name.as_str()).is_none() && translated_definition.contract_names.iter().any(|n| n == name) {
            project.translate(Some(name), &translated_definition.path).unwrap();
        }

        if let Some(external_definition) = project.find_definition_with_abi(name.as_str()) {
            for entry in external_definition.uses.iter() {
                if !translated_definition.uses.contains(entry) {
                    translated_definition.uses.push(entry.clone());
                }
            }
            abi_type_name = Some(variable_type_name.clone());

            variable_type_name = sway::TypeName::Identifier {
                name: "Identity".into(),
                generic_parameters: None,
            };
        }
    }

    // Translate the variable's initial value
    let value_scope = Rc::new(RefCell::new(TranslationScope {
        parent: Some(translated_definition.toplevel_scope.clone()),
        ..Default::default()
    }));

    let value = match &variable_type_name {
        sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
            // Create deferred initializations for types that can't be initialized with a value
            ("StorageString", None) | ("StorageVec", Some(_)) => {
                if let Some(x) = variable_definition.initializer.as_ref() {
                    let value = translate_expression(project, translated_definition, &value_scope, x)?;

                    translated_definition.deferred_initializations.push(DeferredInitialization {
                        name: new_name.clone(),
                        is_storage,
                        is_constant,
                        is_configurable,
                        value,
                    });
                }

                sway::Expression::from(sway::Constructor {
                    type_name: sway::TypeName::Identifier {
                        name: name.clone(),
                        generic_parameters: None,
                    },
                    fields: vec![],
                })
            }

            // HACK: Check for Identity storage fields that have an abi cast for their initializer
            ("Identity", None) => {
                let initializer = variable_definition.initializer.as_ref()
                    .map(|x| {
                        let mut value = translate_expression(project, translated_definition, &value_scope, x);
                        
                        if let Ok(sway::Expression::Commented(comment, expression)) = &value {
                            if let sway::Expression::FunctionCall(function_call) = expression.as_ref() {
                                if let Some(identifier) = function_call.function.as_identifier() {
                                    if identifier == "abi" && function_call.parameters.len() == 2 {
                                        value = Ok(sway::Expression::Commented(comment.clone(), Box::new(function_call.parameters[1].clone())));
                                    }
                                }
                            }
                        }

                        value
                    })
                    .transpose()?;
                
                create_value_expression(translated_definition, value_scope.clone(), &variable_type_name, initializer.as_ref())
            }

            (name, generic_parameters) => {
                let initializer = variable_definition.initializer.as_ref()
                    .map(|x| translate_expression(project, translated_definition, &value_scope, x))
                    .transpose()?;

                // HACK: Add to mapping names for toplevel structs in storage that contain storage mappings
                if generic_parameters.is_none() {
                    if let Some(struct_definition) = translated_definition.structs.iter().find(|s| s.borrow().name == name).cloned() {
                        for field in struct_definition.borrow().fields.iter() {
                            let Some(option_type) = field.type_name.option_type() else { continue };
                            let Some(storage_key_type) = option_type.storage_key_type() else { continue };
                            let Some(_) = storage_key_type.storage_map_type() else { continue };

                            let struct_name = translate_naming_convention(struct_definition.borrow().name.as_str(), Case::Snake);

                            if !translated_definition.mapping_names.iter().any(|(n, _)| *n == struct_name) {
                                translated_definition.mapping_names.push((struct_name.clone(), vec![]));
                            }

                            let mapping_names = translated_definition.mapping_names.iter_mut().find(|m| m.0 == struct_name).unwrap();
                            mapping_names.1.push(field.name.clone());

                            let instance_field_name = format!("{struct_name}_instance_count");
                            let mapping_field_name = format!("{struct_name}_{}s", field.name);

                            let storage = translated_definition.get_storage_namespace();

                            if let Some(field) = storage.fields.iter().find(|f| f.name == instance_field_name) {
                                assert!(field.type_name.is_u64(), "Instance count field already exists : {field:#?}");
                            } else {
                                storage.fields.push(
                                    sway::StorageField { 
                                        name: instance_field_name,
                                        type_name: sway::TypeName::Identifier { 
                                            name: "u64".into(),
                                            generic_parameters: None 
                                        },
                                        value: sway::Expression::Literal(sway::Literal::DecInt(BigUint::zero(), None))
                                    }
                                );
                            }

                            if let Some(field) = storage.fields.iter().find(|f| f.name == mapping_field_name) {
                                if let Some((k, v)) = field.type_name.storage_map_type() {
                                    assert!(k.is_u64() && v.is_compatible_with(&storage_key_type), "Instance mapping field already exists : {field:#?}");
                                } else {
                                    panic!("Instance mapping field already exists : {field:#?}");
                                }
                            } else {
                                storage.fields.push(
                                    sway::StorageField { 
                                        name: mapping_field_name,
                                        type_name: sway::TypeName::Identifier { 
                                            name: "StorageMap".into(),
                                            generic_parameters: Some(sway::GenericParameterList { 
                                                entries: vec![
                                                    sway::GenericParameter { 
                                                        type_name: sway::TypeName::Identifier { 
                                                            name: "u64".into(),
                                                            generic_parameters: None,
                                                        },
                                                        implements: None,
                                                    },
                                                    sway::GenericParameter { 
                                                        type_name: storage_key_type.clone(),
                                                        implements: None,
                                                    }
                                                ] 
                                            })
                                        },
                                        value: sway::Expression::from(sway::Constructor { 
                                            type_name: sway::TypeName::Identifier { 
                                                name: "StorageMap".into(),
                                                generic_parameters: None,
                                            },
                                            fields: vec![] 
                                        })
                                    }
                                );
                            }
                        }
                    }
                }

                create_value_expression(translated_definition, value_scope.clone(), &variable_type_name, initializer.as_ref())
            }
        }

        sway::TypeName::StringSlice if is_constant || is_configurable => {
            let initializer = translate_expression(project, translated_definition, &value_scope, variable_definition.initializer.as_ref().unwrap())?;
            
            let sway::Expression::Literal(sway::Literal::String(value)) = initializer else { panic!("Expected a string literal") };
        
            variable_type_name = sway::TypeName::StringArray { length: value.len() };
            
            sway::Expression::create_function_calls(None, &[
                ("__to_str_array", Some((None, vec![sway::Expression::from(sway::Literal::String(value))]))),
            ])
        }
        
        _ => if let Some(x) = variable_definition.initializer.as_ref() {
            let value = translate_expression(project, translated_definition, &value_scope, x)?;
            create_value_expression(translated_definition, value_scope.clone(), &variable_type_name, Some(&value))
        } else {
            create_value_expression(translated_definition, value_scope.clone(), &variable_type_name, None)
        },
    };

    // Handle constant variable definitions
    if is_constant {
        let scope = Rc::new(RefCell::new(TranslationScope {
            parent: Some(translated_definition.toplevel_scope.clone()),
            ..Default::default()
        }));

        // Evaluate the value ahead of time in order to generate an appropriate constant value expression
        let value = evaluate_expression(translated_definition, scope, &variable_type_name, &value);

        translated_definition.constants.push(sway::Constant {
            is_public,
            name: new_name.clone(),
            type_name: variable_type_name.clone(),
            value: Some(value),
        });
    }
    // Handle immutable variable definitions
    else if is_immutable {
        //
        // TODO: we need to check if the value is supplied to the constructor and remove it from there
        //

        translated_definition.get_configurable().fields.push(sway::ConfigurableField {
            name: new_name.clone(),
            type_name: variable_type_name.clone(),
            value,
        });
    }
    // Handle regular state variable definitions
    else if storage_namespace.is_some() {
        translated_definition.get_storage_namespace().fields.push(sway::StorageField {
            name: new_name.clone(),
            type_name: variable_type_name.clone(),
            value,
        });
    }
    
    // Add the storage variable for function scopes
    translated_definition.toplevel_scope.borrow_mut().variables.push(Rc::new(RefCell::new(TranslatedVariable {
        old_name: old_name.clone(),
        new_name: new_name.clone(),
        type_name: if is_storage {
            sway::TypeName::Identifier { 
                name: "StorageKey".to_string(),
                generic_parameters: Some(sway::GenericParameterList { 
                    entries: vec![
                        sway::GenericParameter { 
                            type_name: variable_type_name.clone(),
                            implements: None 
                        }
                    ] 
                })
            }
        } else {
            variable_type_name.clone()
        },
        abi_type_name,
        storage_namespace,
        is_configurable,
        is_constant,
        ..Default::default()
    })));

    // Generate a getter function if the storage field is public
    if !is_public {
        return Ok(());
    }

    // Generate parameters and return type for the public getter function
    let mut parameters = vec![];
    let mut return_type = translate_return_type_name(project, translated_definition, &variable_type_name);

    if let Some((inner_parameters, inner_return_type)) = variable_type_name.getter_function_parameters_and_return_type() {
        parameters = inner_parameters;
        return_type = inner_return_type;
    }

    // Create the function declaration for the abi
    let mut sway_function = sway::Function {
        attributes: if is_storage {
            Some(sway::AttributeList {
                attributes: vec![
                    sway::Attribute {
                        name: "storage".into(),
                        parameters: Some(vec![
                            "read".into(),
                        ]),
                    },
                ],
            })
        } else {
            None
        },
        is_public: false,
        old_name: old_name.clone(),
        name: new_name.clone(),
        generic_parameters: None,
        parameters: sway::ParameterList {
            entries: parameters.iter().map(|(p, _)| p.clone()).collect(),
        },
        return_type: Some(return_type),
        body: None,
    };

    if let Some(abi) = translated_definition.abi.as_mut() {
        // Only add the function to the abi if it doesn't already exist
        if !abi.functions.contains(&sway_function) {
            abi.functions.push(sway_function.clone());
        }
    } else {
        // Add the function to the abi
        translated_definition.get_abi().functions.push(sway_function.clone());
    }

    // Add the toplevel function to the scope
    translated_definition.toplevel_scope.borrow_mut().functions.push(Rc::new(RefCell::new(TranslatedFunction {
        old_name: old_name.clone(),
        new_name: new_name.clone(),
        attributes: Some(sway::AttributeList {
            attributes: vec![
                sway::Attribute {
                    name: "storage".into(),
                    parameters: Some(vec![
                        "read".into(),
                    ]),
                },
            ],
        }),
        constructor_calls: vec![],
        modifiers: vec![],
        type_name: sway::TypeName::Function {
            generic_parameters: None,
            parameters: sway_function.parameters.clone(),
            return_type: sway_function.return_type.as_ref().map(|x| Box::new(x.clone())),
        },
    })));

    let namespace_name = translated_definition.get_storage_namespace_name();

    // Create the body for the toplevel function
    sway_function.body = Some(sway::Block {
        statements: vec![],
        final_expr: Some(if is_storage {
            let mut expression = sway::Expression::from(sway::MemberAccess {
                expression: sway::Expression::create_identifier(format!("storage::{namespace_name}")),
                member: new_name.clone(),
            });

            for (parameter, needs_unwrap) in parameters.iter() {
                expression = sway::Expression::create_function_calls(Some(expression), &[
                    ("get", Some((None, vec![sway::Expression::create_identifier(parameter.name.clone())]))),
                ]);

                if *needs_unwrap {
                    expression = sway::Expression::create_function_calls(Some(expression), &[
                        ("unwrap", Some((None, vec![]))),
                    ]);
                }
            }
            
            sway::Expression::create_function_calls(Some(expression), &[("read", Some((None, vec![])))])
        } else if is_constant || is_immutable {
            sway::Expression::create_identifier(new_name.clone())
        } else {
            todo!("Handle getter function for non-storage variables: {} - {variable_definition:#?}", variable_definition.to_string())
        }),
    });

    // Add the toplevel function
    translated_definition.functions.push(sway_function.clone());

    // Create the body for the contract impl's function wrapper
    sway_function.body = Some(sway::Block {
        statements: vec![],
        final_expr: Some(sway::Expression::create_function_calls(None, &[
            (format!("::{}", sway_function.name).as_str(), Some((None, vec![]))),
        ])),
    });

    // Create the function wrapper item for the contract impl block
    let impl_item = sway::ImplItem::Function(sway_function);

    if let Some(contract_impl) = translated_definition.find_contract_impl_mut() {
        // Only add the function wrapper to the contract impl if it doesn't already exist
        if !contract_impl.items.contains(&impl_item) {
            contract_impl.items.push(impl_item);
        }
    } else {
        // Add the function wrapper to the contract impl
        translated_definition.get_contract_impl().items.push(impl_item);
    }

    Ok(())
}

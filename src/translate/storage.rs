use crate::{error::Error, ir, project::Project, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_state_variable(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    contract_name: Option<&str>,
    variable_definition: &solidity::VariableDefinition,
) -> Result<
    (
        Vec<ir::DeferredInitialization>,
        Vec<(String, Vec<String>)>,
        Option<(sway::Function, sway::Function, sway::Function)>,
    ),
    Error,
> {
    // println!(
    //     "Translating state variable {} at: {}",
    //     variable_definition,
    //     project.loc_to_file_location_string(module.clone(), &variable_definition.loc)
    // );

    let value_scope = Rc::new(RefCell::new(ir::Scope::new(contract_name, None)));

    // Collect information about the variable from its attributes
    let is_public = variable_definition.attrs.iter().any(|x| {
        matches!(
            x,
            solidity::VariableAttribute::Visibility(
                solidity::Visibility::External(_) | solidity::Visibility::Public(_)
            )
        )
    });

    let is_constant = variable_definition
        .attrs
        .iter()
        .any(|x| matches!(x, solidity::VariableAttribute::Constant(_)));

    let is_immutable = variable_definition
        .attrs
        .iter()
        .any(|x| matches!(x, solidity::VariableAttribute::Immutable(_)));

    // If the state variable is immutable and not a constant, it is a configurable field
    let is_configurable = is_immutable && !is_constant;

    // If the state variable is not constant or immutable, it is a storage field
    let storage_namespace = if !is_constant && !is_immutable {
        Some(translate_naming_convention(
            contract_name
                .map(|s| s.to_string())
                .unwrap_or_else(|| module.borrow().name.clone())
                .as_str(),
            Case::Snake,
        ))
    } else {
        None
    };

    let is_storage = storage_namespace.is_some();

    // Translate the variable's naming convention
    let old_name = variable_definition.name.as_ref().unwrap().name.clone();

    let mut new_name = if is_constant || is_immutable {
        translate_naming_convention(old_name.as_str(), Case::Constant)
    } else {
        translate_naming_convention(old_name.as_str(), Case::Snake)
    };

    if is_constant {
        let mut module = module.borrow_mut();
        // Increase the constant name count
        let count = module
            .constant_name_counts
            .entry(new_name.clone())
            .or_insert(0);
        *count += 1;

        // Append the constant name count to the end of the constant name if there is more than 1
        if *count > 1 {
            new_name = format!("{new_name}_{}", *count);
        }
    }

    // Translate the variable's type name
    // let storage_location = if is_storage {
    //     Some(solidity::StorageLocation::Storage(Default::default()))
    // } else {
    //     None
    // };

    let mut variable_type_name = translate_type_name(
        project,
        module.clone(),
        value_scope.clone(),
        &variable_definition.ty,
        if is_storage {
            Some(solidity::StorageLocation::Storage(Default::default()))
        } else {
            None
        }
        .as_ref(),
    );

    // Storage fields should not be wrapped in a `StorageKey<T>`
    if is_storage {
        if let Some(storage_key_type) = variable_type_name.storage_key_type() {
            variable_type_name = storage_key_type;
        }
    }

    // Translate the variable's initial value
    let mut deferred_initializations = vec![];
    let mut mapping_names = vec![];
    let mut value = None;

    // HACK: Check for Identity storage fields that have an abi cast for their initializer
    if value.is_none() && variable_type_name.is_identity() {
        let initializer = variable_definition
            .initializer
            .as_ref()
            .map(|x| {
                let mut value =
                    translate_expression(project, module.clone(), value_scope.clone(), x);

                if let Ok(sway::Expression::Commented(comment, expression)) = &value {
                    if let sway::Expression::FunctionCall(function_call) = expression.as_ref() {
                        if let Some(identifier) = function_call.function.as_identifier() {
                            if identifier == "abi" && function_call.parameters.len() == 2 {
                                value = Ok(sway::Expression::Commented(
                                    comment.clone(),
                                    Box::new(function_call.parameters[1].clone()),
                                ));
                            }
                        }
                    }
                }

                value
            })
            .transpose()?;

        value = Some(create_value_expression(
            project,
            module.clone(),
            value_scope.clone(),
            &variable_type_name,
            initializer.as_ref(),
        ));
    }

    // Create deferred initializations for types that can't be initialized with a value
    if value.is_none()
        && (variable_type_name.is_storage_string() || variable_type_name.is_storage_vec())
    {
        if let Some(x) = variable_definition.initializer.as_ref() {
            let value = translate_expression(project, module.clone(), value_scope.clone(), x)?;

            deferred_initializations.push(ir::DeferredInitialization {
                name: new_name.clone(),
                is_storage,
                is_constant,
                is_configurable,
                value,
            });
        }

        value = Some(sway::Expression::from(sway::Constructor {
            type_name: sway::TypeName::Identifier {
                name: if variable_type_name.is_storage_string() {
                    "StorageString".into()
                } else {
                    "StorageVec".into()
                },
                generic_parameters: None,
            },
            fields: vec![],
        }));
    }

    if value.is_none() && ((is_constant || is_configurable) && variable_type_name.is_string_slice())
    {
        let initializer = translate_expression(
            project,
            module.clone(),
            value_scope.clone(),
            variable_definition.initializer.as_ref().unwrap(),
        )?;

        let sway::Expression::Literal(sway::Literal::String(string)) = initializer else {
            panic!("Expected a string literal")
        };

        variable_type_name = sway::TypeName::StringArray {
            length: string.len(),
        };

        value = Some(sway::Expression::create_function_calls(
            None,
            &[(
                "__to_str_array",
                Some((
                    None,
                    vec![sway::Expression::from(sway::Literal::String(string))],
                )),
            )],
        ));
    }

    if value.is_none() {
        // HACK: Add to mapping names for toplevel structs in storage that contain storage mappings
        if let Some(struct_definition) =
            project.find_struct(module.clone(), &variable_type_name.to_string())
        {
            for field in struct_definition.borrow().fields.iter() {
                let Some(option_type) = field.type_name.option_type() else {
                    continue;
                };
                let Some(storage_key_type) = option_type.storage_key_type() else {
                    continue;
                };
                let Some(_) = storage_key_type.storage_map_type() else {
                    continue;
                };

                let struct_name = translate_naming_convention(
                    struct_definition.borrow().name.as_str(),
                    Case::Snake,
                );

                if !mapping_names.iter().any(|(n, _)| *n == struct_name) {
                    mapping_names.push((struct_name.clone(), vec![]));
                }

                let mapping_names = mapping_names
                    .iter_mut()
                    .find(|m| m.0 == struct_name)
                    .unwrap();
                mapping_names.1.push(field.name.clone());

                let instance_field_name = format!("{struct_name}_instance_count");
                let mapping_field_name = format!("{struct_name}_{}s", field.name);

                let mut module = module.borrow_mut();
                let storage = module.get_storage_namespace(value_scope.clone()).unwrap();

                if let Some(field) = storage
                    .borrow()
                    .fields
                    .iter()
                    .find(|f| f.name == instance_field_name)
                {
                    assert!(
                        field.type_name.is_u64(),
                        "Instance count field already exists: {field:#?}"
                    );
                } else {
                    storage.borrow_mut().fields.push(sway::StorageField {
                        old_name: String::new(),
                        name: instance_field_name,
                        type_name: sway::TypeName::Identifier {
                            name: "u64".into(),
                            generic_parameters: None,
                        },
                        value: sway::Expression::Literal(sway::Literal::DecInt(
                            BigUint::zero(),
                            None,
                        )),
                    });
                }

                if let Some(field) = storage
                    .borrow()
                    .fields
                    .iter()
                    .find(|f| f.name == mapping_field_name)
                {
                    if let Some((k, v)) = field.type_name.storage_map_type() {
                        assert!(
                            k.is_u64() && v.is_compatible_with(&storage_key_type),
                            "Instance mapping field already exists: {field:#?}"
                        );
                    } else {
                        panic!("Instance mapping field already exists: {field:#?}");
                    }
                } else {
                    storage.borrow_mut().fields.push(sway::StorageField {
                        old_name: String::new(),
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
                                    },
                                ],
                            }),
                        },
                        value: sway::Expression::from(sway::Constructor {
                            type_name: sway::TypeName::Identifier {
                                name: "StorageMap".into(),
                                generic_parameters: None,
                            },
                            fields: vec![],
                        }),
                    });
                }
            }
        }

        value = Some(if let Some(x) = variable_definition.initializer.as_ref() {
            let value = translate_expression(project, module.clone(), value_scope.clone(), x)?;
            create_value_expression(
                project,
                module.clone(),
                value_scope.clone(),
                &variable_type_name,
                Some(&value),
            )
        } else {
            create_value_expression(
                project,
                module.clone(),
                value_scope.clone(),
                &variable_type_name,
                None,
            )
        });
    }

    let value = value.unwrap();

    // Handle constant variable definitions
    if is_constant {
        let scope = Rc::new(RefCell::new(ir::Scope::new(contract_name, None)));

        // Evaluate the value ahead of time in order to generate an appropriate constant value expression
        let value = evaluate_expression(
            project,
            module.clone(),
            scope.clone(),
            &variable_type_name,
            &value,
        );

        module.borrow_mut().constants.push(sway::Constant {
            is_public,
            old_name: old_name.clone(),
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

        module
            .borrow_mut()
            .get_configurable()
            .fields
            .push(sway::ConfigurableField {
                old_name: old_name.clone(),
                name: new_name.clone(),
                type_name: variable_type_name.clone(),
                value,
            });
    }
    // Handle regular state variable definitions
    else if storage_namespace.is_some() {
        module
            .borrow_mut()
            .get_storage_namespace(value_scope.clone())
            .unwrap()
            .borrow_mut()
            .fields
            .push(sway::StorageField {
                old_name: old_name.clone(),
                name: new_name.clone(),
                type_name: variable_type_name.clone(),
                value,
            });
    }

    // Generate a getter function if the storage field is public
    if !is_public {
        return Ok((deferred_initializations, mapping_names, None));
    }

    // Generate parameters and return type for the public getter function
    let mut parameters = vec![];
    let mut return_type = get_return_type_name(project, module.clone(), &variable_type_name);

    if let Some((inner_parameters, inner_return_type)) =
        variable_type_name.getter_function_parameters_and_return_type()
    {
        parameters = inner_parameters;
        return_type = inner_return_type;
    }

    let function_name = translate_function_name(
        project,
        module.clone(),
        contract_name,
        Some(old_name.as_str()),
        &Default::default(), // TODO: generate solidity parameter list for implicit getter functions...
        &solidity::FunctionTy::Function,
    );

    // Create the function declaration for the abi
    let abi_function = sway::Function {
        attributes: if is_storage {
            Some(sway::AttributeList {
                attributes: vec![sway::Attribute {
                    name: "storage".into(),
                    parameters: Some(vec!["read".into()]),
                }],
            })
        } else {
            None
        },
        is_public: false,
        old_name: old_name.clone(),
        name: function_name.abi_fn_name.clone(),
        generic_parameters: None,
        parameters: sway::ParameterList {
            entries: parameters.iter().map(|(p, _)| p.clone()).collect(),
        },
        return_type: Some(return_type),
        body: None,
    };

    let namespace_name = translate_naming_convention(contract_name.unwrap(), Case::Snake);

    // Create the the toplevel function
    let mut toplevel_function = abi_function.clone();
    toplevel_function.name = function_name.top_level_fn_name;

    toplevel_function.body = Some(sway::Block {
        statements: vec![],
        final_expr: Some(if is_storage {
            let mut expression = sway::Expression::from(sway::MemberAccess {
                expression: sway::Expression::create_identifier(format!(
                    "storage::{namespace_name}"
                )),
                member: new_name.clone(),
            });

            for (parameter, needs_unwrap) in parameters.iter() {
                expression = sway::Expression::create_function_calls(
                    Some(expression),
                    &[(
                        "get",
                        Some((
                            None,
                            vec![sway::Expression::create_identifier(parameter.name.clone())],
                        )),
                    )],
                );

                if *needs_unwrap {
                    expression = sway::Expression::create_function_calls(
                        Some(expression),
                        &[("unwrap", Some((None, vec![])))],
                    );
                }
            }

            sway::Expression::create_function_calls(
                Some(expression),
                &[("read", Some((None, vec![])))],
            )
        } else if is_constant || is_immutable {
            sway::Expression::create_identifier(new_name.clone())
        } else {
            todo!(
                "Handle getter function for non-storage variables: {} - {variable_definition:#?}",
                variable_definition.to_string()
            )
        }),
    });

    // Create the contract impl's function wrapper
    let mut impl_function = abi_function.clone();

    impl_function.body = Some(sway::Block {
        statements: vec![],
        final_expr: Some(sway::Expression::create_function_calls(
            None,
            &[(
                format!("::{}", toplevel_function.name).as_str(),
                Some((None, vec![])),
            )],
        )),
    });

    Ok((
        deferred_initializations,
        mapping_names,
        Some((abi_function, toplevel_function, impl_function)),
    ))
}

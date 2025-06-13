use crate::{error::Error, ir, project::Project, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_state_variable(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    contract_name: Option<&str>,
    variable_definition: &solidity::VariableDefinition,
) -> Result<(Vec<ir::DeferredInitialization>, Vec<(String, Vec<String>)>), Error> {
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

    // If the state variable is immutable and not a constant, it is a configurable field
    let is_configurable = is_immutable && !is_constant;

    // Translate the variable's naming convention
    let old_name = variable_definition.name.as_ref().unwrap().name.clone();

    let new_name = if is_constant || is_immutable {
        translate_naming_convention(old_name.as_str(), Case::Constant)
    } else {
        translate_naming_convention(old_name.as_str(), Case::Snake)
    };

    // Translate the variable's type name
    let mut variable_type_name = translate_type_name(
        project,
        module.clone(),
        value_scope.clone(),
        &variable_definition.ty,
        is_storage,
        false,
    );

    // Check if the variable's type is an ABI
    let mut abi_type_name = None;

    if let sway::TypeName::Identifier {
        name,
        generic_parameters: None,
    } = &variable_type_name
    {
        if let Some(contract) = project.find_contract(module.clone(), name.as_str()) {
            // TODO:
            // for entry in contract.borrow().uses.iter() {
            //     if !module.uses.contains(entry) {
            //         module.uses.push(entry.clone());
            //     }
            // }

            abi_type_name = Some(variable_type_name.clone());

            variable_type_name = sway::TypeName::Identifier {
                name: "Identity".into(),
                generic_parameters: None,
            };
        }
    }

    // Translate the variable's initial value

    let mut deferred_initializations = vec![];
    let mut mapping_names = vec![];

    let value = match &variable_type_name {
        sway::TypeName::Identifier {
            name,
            generic_parameters,
        } => match (name.as_str(), generic_parameters.as_ref()) {
            // Create deferred initializations for types that can't be initialized with a value
            ("StorageString", None) | ("StorageVec", Some(_)) => {
                if let Some(x) = variable_definition.initializer.as_ref() {
                    let value =
                        translate_expression(project, module.clone(), value_scope.clone(), x)?;

                    deferred_initializations.push(ir::DeferredInitialization {
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
                let initializer = variable_definition
                    .initializer
                    .as_ref()
                    .map(|x| {
                        let mut value =
                            translate_expression(project, module.clone(), value_scope.clone(), x);

                        if let Ok(sway::Expression::Commented(comment, expression)) = &value {
                            if let sway::Expression::FunctionCall(function_call) =
                                expression.as_ref()
                            {
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

                create_value_expression(
                    project,
                    module.clone(),
                    value_scope.clone(),
                    &variable_type_name,
                    initializer.as_ref(),
                )
            }

            (name, generic_parameters) => {
                let initializer = variable_definition
                    .initializer
                    .as_ref()
                    .map(|x| translate_expression(project, module.clone(), value_scope.clone(), x))
                    .transpose()?;

                // HACK: Add to mapping names for toplevel structs in storage that contain storage mappings
                if generic_parameters.is_none() {
                    let struct_defintion = {
                        let module = module.borrow();
                        module
                            .structs
                            .iter()
                            .find(|s| s.signature.to_string() == name)
                            .cloned()
                    };

                    if let Some(struct_definition) = &struct_defintion {
                        let struct_definition = struct_definition.implementation.as_ref().unwrap();

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
                            let storage =
                                module.get_storage_namespace(value_scope.clone()).unwrap();

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
                                    abi_type_name: abi_type_name.clone(),
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
                                    abi_type_name: abi_type_name.clone(),
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
                }

                create_value_expression(
                    project,
                    module.clone(),
                    value_scope.clone(),
                    &variable_type_name,
                    initializer.as_ref(),
                )
            }
        },

        sway::TypeName::StringSlice if is_constant || is_configurable => {
            let initializer = translate_expression(
                project,
                module.clone(),
                value_scope.clone(),
                variable_definition.initializer.as_ref().unwrap(),
            )?;

            let sway::Expression::Literal(sway::Literal::String(value)) = initializer else {
                panic!("Expected a string literal")
            };

            variable_type_name = sway::TypeName::StringArray {
                length: value.len(),
            };

            sway::Expression::create_function_calls(
                None,
                &[(
                    "__to_str_array",
                    Some((
                        None,
                        vec![sway::Expression::from(sway::Literal::String(value))],
                    )),
                )],
            )
        }

        _ => {
            if let Some(x) = variable_definition.initializer.as_ref() {
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
            }
        }
    };

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
            old_name,
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
                old_name,
                name: new_name.clone(),
                type_name: variable_type_name.clone(),
                abi_type_name,
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
                abi_type_name,
                type_name: variable_type_name.clone(),
                value,
            });
    }

    Ok((deferred_initializations, mapping_names))
}

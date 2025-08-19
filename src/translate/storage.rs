use crate::{error::Error, ir, project::Project, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[derive(Clone, Debug)]
pub struct StateVariableInfo {
    pub is_public: bool,
    pub is_storage: bool,
    pub is_immutable: bool,
    pub is_constant: bool,
    pub is_configurable: bool,
    pub old_name: String,
    pub new_name: String,
    pub type_name: sway::TypeName,
    pub deferred_initializations: Vec<ir::DeferredInitialization>,
    pub mapping_names: Vec<(String, Vec<String>)>,
}

#[inline]
pub fn translate_state_variable(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    contract_name: Option<&str>,
    variable_definition: &solidity::VariableDefinition,
) -> Result<StateVariableInfo, Error> {
    // println!(
    //     "Translating state variable {} at: {}",
    //     variable_definition,
    //     project.loc_to_file_location_string(module.clone(), &variable_definition.loc)
    // );

    let scope = Rc::new(RefCell::new(ir::Scope::new(contract_name, None, None)));

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
    let mut variable_type_name = translate_type_name(
        project,
        module.clone(),
        scope.clone(),
        &variable_definition.ty,
        if is_storage {
            Some(solidity::StorageLocation::Storage(Default::default()))
        } else {
            None
        }
        .as_ref(),
    );

    // Storage fields should not be wrapped in a `StorageKey<T>`
    if is_storage && let Some(storage_key_type) = variable_type_name.storage_key_type() {
        variable_type_name = storage_key_type;
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
                let mut value = translate_expression(project, module.clone(), scope.clone(), x);

                if let Ok(sway::Expression::Commented(comment, expression)) = &value
                    && let sway::Expression::FunctionCall(function_call) = expression.as_ref()
                    && let Some(identifier) = function_call.function.as_identifier()
                    && identifier == "abi"
                    && function_call.parameters.len() == 2
                {
                    value = Ok(sway::Expression::Commented(
                        comment.clone(),
                        Box::new(function_call.parameters[1].clone()),
                    ));
                }

                value
            })
            .transpose()?;

        value = Some(create_value_expression(
            project,
            module.clone(),
            scope.clone(),
            &variable_type_name,
            initializer.as_ref(),
        ));
    }

    // Create deferred initializations for types that can't be initialized with a value
    if value.is_none()
        && (variable_type_name.is_storage_string()
            || variable_type_name.is_storage_vec()
            || variable_type_name.is_storage_map()
            || variable_type_name.is_storage_bytes())
    {
        if let Some(x) = variable_definition.initializer.as_ref() {
            let value = translate_expression(project, module.clone(), scope.clone(), x)?;

            deferred_initializations.push(ir::DeferredInitialization {
                expression: sway::Expression::create_identifier("storage_struct")
                    .with_member(&new_name),
                is_storage,
                is_constant,
                is_configurable,
                value,
            });

            ensure_constructor_called_fields_exist(project, module.clone(), scope.clone());
        }

        let sway::TypeName::Identifier { name, .. } = variable_type_name.clone() else {
            unreachable!()
        };

        value = Some(sway::Expression::from(sway::Constructor {
            type_name: sway::TypeName::create_identifier(name.as_str()),
            fields: vec![],
        }));
    }

    if value.is_none() && ((is_constant || is_configurable) && variable_type_name.is_string_slice())
    {
        let initializer = translate_expression(
            project,
            module.clone(),
            scope.clone(),
            variable_definition.initializer.as_ref().unwrap(),
        )?;

        let sway::Expression::Literal(sway::Literal::String(string)) = initializer else {
            panic!("Expected a string literal")
        };

        variable_type_name = sway::TypeName::StringArray {
            length: string.len(),
        };

        value = Some(sway::Expression::create_function_call(
            "__to_str_array",
            None,
            vec![sway::Expression::from(sway::Literal::String(string))],
        ));
    }

    if value.is_none() {
        // HACK: Add to mapping names for structs in storage that contain fields of type `Option<StorageKey<T>>`
        if let Some(struct_definition) = project.find_struct(
            module.clone(),
            scope.clone(),
            &variable_type_name.to_string(),
        ) {
            let struct_definition = struct_definition.borrow();

            let fields = if struct_definition.memory.name == variable_type_name.to_string() {
                struct_definition.memory.fields.as_slice()
            } else if struct_definition.storage.name == variable_type_name.to_string() {
                struct_definition.storage.fields.as_slice()
            } else {
                todo!()
            };

            let mut field_initializations = vec![];

            let struct_field_name =
                translate_naming_convention(&variable_type_name.to_string(), Case::Snake);

            let instance_field_name = format!("{struct_field_name}_instance_count");

            for field in fields.iter() {
                let Some(option_type) = field.type_name.option_type() else {
                    continue;
                };

                let Some(storage_key_type) = option_type.storage_key_type() else {
                    continue;
                };

                if !mapping_names.iter().any(|(n, _)| *n == struct_field_name) {
                    mapping_names.push((struct_field_name.clone(), vec![]));
                }

                let mapping_names = mapping_names
                    .iter_mut()
                    .find(|m| m.0 == struct_field_name)
                    .unwrap();
                mapping_names.1.push(field.new_name.clone());

                // Ensure the instance count field exists in storage
                module.borrow_mut().create_storage_field(
                    scope.clone(),
                    instance_field_name.as_str(),
                    &sway::TypeName::create_identifier("u64"),
                    &sway::Expression::Literal(sway::Literal::DecInt(BigUint::zero(), None)),
                );

                // Ensure the instance mapping field exists in storage
                let mapping_field_name =
                    format!("{struct_field_name}_{}_instances", field.new_name,);

                module.borrow_mut().create_storage_field(
                    scope.clone(),
                    mapping_field_name.as_str(),
                    &sway::TypeName::create_generic(
                        "StorageMap",
                        vec![
                            sway::TypeName::create_identifier("u64"),
                            storage_key_type.clone(),
                        ],
                    ),
                    &sway::Expression::from(sway::Constructor {
                        type_name: sway::TypeName::create_identifier("StorageMap"),
                        fields: vec![],
                    }),
                );

                field_initializations.push(sway::Statement::from(sway::Expression::from(
                    sway::BinaryExpression {
                        operator: "=".to_string(),
                        lhs: sway::Expression::create_identifier(&new_name)
                            .with_member(&field.new_name),
                        rhs: sway::Expression::create_identifier("storage_struct")
                            .with_member(&mapping_field_name)
                            .with_get_call(sway::Expression::create_identifier("instance_index")),
                    },
                )));
            }

            if !field_initializations.is_empty() {
                field_initializations.insert(
                    0,
                    sway::Statement::from(sway::Let {
                        pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                            is_mutable: true,
                            name: new_name.clone(),
                        }),
                        type_name: None,
                        value: sway::Expression::create_identifier("storage_struct")
                            .with_member(&new_name)
                            .with_read_call(),
                    }),
                );

                field_initializations.insert(
                    0,
                    sway::Statement::from(
                        sway::Expression::create_identifier("storage_struct")
                            .with_member(&instance_field_name)
                            .with_write_call(sway::Expression::from(sway::BinaryExpression {
                                operator: "+".to_string(),
                                lhs: sway::Expression::create_identifier(&instance_field_name),
                                rhs: sway::Expression::from(sway::Literal::DecInt(
                                    1_u64.into(),
                                    None,
                                )),
                            })),
                    ),
                );

                field_initializations.insert(
                    0,
                    sway::Statement::from(sway::Let {
                        pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                            is_mutable: false,
                            name: "instance_index".to_string(),
                        }),
                        type_name: None,
                        value: sway::Expression::create_identifier("storage_struct")
                            .with_member(&instance_field_name)
                            .with_read_call(),
                    }),
                );

                deferred_initializations.push(ir::DeferredInitialization {
                    expression: sway::Expression::create_identifier("storage_struct")
                        .with_member(&new_name),
                    is_storage,
                    is_constant,
                    is_configurable,
                    value: sway::Expression::from(sway::Block {
                        statements: field_initializations,
                        final_expr: Some(sway::Expression::create_identifier(&new_name)),
                    }),
                });
            }
        }

        value = Some(if let Some(x) = variable_definition.initializer.as_ref() {
            let value = translate_expression(project, module.clone(), scope.clone(), x)?;
            create_value_expression(
                project,
                module.clone(),
                scope.clone(),
                &variable_type_name,
                Some(&value),
            )
        } else {
            create_value_expression(
                project,
                module.clone(),
                scope.clone(),
                &variable_type_name,
                None,
            )
        });
    }

    let value = value.unwrap();

    // Handle constant variable definitions
    if is_constant {
        let scope = Rc::new(RefCell::new(ir::Scope::new(contract_name, None, None)));

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
            .get_storage_struct(scope.clone())
            .borrow_mut()
            .storage
            .fields
            .push(sway::StructField {
                is_public: true,
                new_name: new_name.clone(),
                old_name: old_name.clone(),
                type_name: variable_type_name.to_storage_key(),
            });

        module
            .borrow_mut()
            .get_storage_namespace(scope.clone())
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

    Ok(StateVariableInfo {
        is_public,
        is_storage,
        is_immutable,
        is_constant,
        is_configurable,
        old_name,
        new_name,
        type_name: variable_type_name.clone(),
        deferred_initializations,
        mapping_names,
    })
}

pub fn generate_state_variable_getter_functions(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    contract_name: Option<&str>,
    state_variable_info: &StateVariableInfo,
) -> Result<Option<(sway::Function, sway::Function, sway::Function)>, Error> {
    if !state_variable_info.is_public {
        return Ok(None);
    }

    // Generate parameters and return type for the public getter function
    let mut parameters = vec![];
    let mut return_type =
        get_return_type_name(project, module.clone(), &state_variable_info.type_name);

    if let Some((inner_parameters, inner_return_type)) = state_variable_info
        .type_name
        .getter_function_parameters_and_return_type()
    {
        parameters = inner_parameters;
        return_type = inner_return_type;
    }

    let function_name = translate_function_name(
        project,
        module.clone(),
        contract_name,
        Some(state_variable_info.old_name.as_str()),
        &Default::default(), // TODO: generate solidity parameter list for implicit getter functions...
        &solidity::FunctionTy::Function,
    );

    // Create the function declaration for the abi
    let abi_function = sway::Function {
        attributes: if state_variable_info.is_storage {
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
        old_name: state_variable_info.old_name.clone(),
        new_name: function_name.abi_fn_name.clone(),
        generic_parameters: None,
        parameters: sway::ParameterList {
            entries: parameters.iter().map(|(p, _)| p.clone()).collect(),
        },
        storage_struct_parameter: None,
        return_type: Some(return_type.clone()),
        body: None,
    };

    // Create the the toplevel function
    let mut toplevel_function = abi_function.clone();
    toplevel_function.is_public = true;
    toplevel_function.new_name = function_name.top_level_fn_name;

    if let Some(contract_name) = contract_name.as_ref()
        && state_variable_info.is_storage
    {
        toplevel_function.storage_struct_parameter = Some(sway::Parameter {
            is_ref: false,
            is_mut: false,
            name: "storage_struct".to_string(),
            type_name: Some(sway::TypeName::create_identifier(
                format!("{contract_name}Storage").as_str(),
            )),
        });
    }

    module.borrow_mut().functions.push(ir::Item {
        signature: toplevel_function.get_type_name(),
        implementation: None,
    });

    toplevel_function.body = Some(sway::Block {
        statements: vec![],
        final_expr: Some(if state_variable_info.is_storage {
            let mut expression = sway::Expression::from(sway::MemberAccess {
                expression: sway::Expression::create_identifier("storage_struct"),
                member: state_variable_info.new_name.clone(),
            });

            for (parameter, needs_unwrap) in parameters.iter() {
                expression = expression
                    .with_get_call(sway::Expression::create_identifier(parameter.name.as_str()));

                if *needs_unwrap {
                    expression = expression.with_unwrap_call();
                }
            }

            scope
                .borrow_mut()
                .set_function_name(&toplevel_function.new_name);

            scope
                .borrow_mut()
                .set_function_storage_accesses(module.clone(), true, false);

            let value = expression.with_read_call();
            let value_type = get_expression_type(project, module.clone(), scope.clone(), &value)?;

            coerce_expression(
                project,
                module.clone(),
                scope.clone(),
                &value,
                &value_type,
                &return_type,
            )
            .unwrap()
        } else if state_variable_info.is_constant || state_variable_info.is_immutable {
            sway::Expression::create_identifier(state_variable_info.new_name.as_str())
        } else {
            todo!("Handle getter function for non-storage variables")
        }),
    });

    // Create the contract impl's function wrapper
    let mut impl_function = abi_function.clone();

    let mut statements = vec![];
    let mut parameters = vec![];

    for p in impl_function.parameters.entries.iter_mut() {
        parameters.push(sway::Expression::create_identifier(p.name.as_str()));

        // Convert parameters of `str` to `String`, since they are not allowed in abi function signatures
        if let Some(sway::TypeName::StringSlice) = p.type_name {
            module.borrow_mut().ensure_use_declared("std::string::*");

            p.type_name = Some(sway::TypeName::create_identifier("String"));

            statements.push(sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                    is_mutable: true,
                    name: p.name.clone(),
                }),
                type_name: None,
                value: sway::Expression::create_identifier(p.name.as_str()).with_as_str_call(),
            }));
        }
    }

    if state_variable_info.is_storage {
        let contract_name = contract_name.unwrap();
        let contract = project
            .find_contract(module.clone(), contract_name)
            .unwrap();

        let has_storage_struct = contract.borrow().storage_struct.is_some();

        if has_storage_struct && contract.borrow().storage_struct_constructor_fn.is_none() {
            let storage_namespace_name = module
                .borrow_mut()
                .get_storage_namespace_name(scope.clone())
                .unwrap();

            let constructor = sway::Constructor {
                type_name: sway::TypeName::create_identifier(
                    format!("{contract_name}Storage").as_str(),
                ),
                fields: contract
                    .borrow()
                    .storage_struct
                    .as_ref()
                    .unwrap()
                    .borrow()
                    .storage
                    .fields
                    .iter()
                    .map(|f| sway::ConstructorField {
                        name: f.new_name.clone(),
                        value: sway::Expression::from(sway::MemberAccess {
                            expression: sway::Expression::from(sway::PathExpr {
                                root: sway::PathExprRoot::Identifier("storage".to_string()),
                                segments: vec![sway::PathExprSegment {
                                    name: storage_namespace_name.clone(),
                                    generic_parameters: None,
                                }],
                            }),
                            member: f.new_name.clone(),
                        }),
                    })
                    .collect(),
            };

            contract.borrow_mut().storage_struct_constructor_fn = Some(sway::Function {
                attributes: None,
                is_public: true,
                old_name: String::new(),
                new_name: format!(
                    "create_{}_storage_struct",
                    contract_name.to_case(Case::Snake)
                ),
                generic_parameters: None,
                parameters: sway::ParameterList::default(),
                storage_struct_parameter: None,
                return_type: Some(sway::TypeName::create_identifier(
                    format!("{contract_name}Storage").as_str(),
                )),
                body: Some(sway::Block {
                    statements: vec![],
                    final_expr: Some(sway::Expression::from(constructor)),
                }),
            });
        }

        if has_storage_struct {
            statements.push(sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::from(sway::LetIdentifier {
                    is_mutable: false,
                    name: "storage_struct".to_string(),
                }),
                type_name: None,
                value: sway::Expression::create_function_call(
                    contract
                        .borrow()
                        .storage_struct_constructor_fn
                        .as_ref()
                        .unwrap()
                        .new_name
                        .as_str(),
                    None,
                    vec![],
                ),
            }));

            scope
                .borrow_mut()
                .add_variable(Rc::new(RefCell::new(ir::Variable {
                    old_name: state_variable_info.old_name.clone(),
                    new_name: "storage_struct".to_string(),
                    type_name: sway::TypeName::create_identifier(
                        format!("{contract_name}Storage").as_str(),
                    ),
                    statement_index: Some(0),
                    read_count: 0,
                    mutation_count: 0,
                })));

            parameters.push(sway::Expression::create_identifier("storage_struct"));
        }
    }

    impl_function.body = Some(sway::Block {
        statements,
        final_expr: Some(sway::Expression::create_function_call(
            toplevel_function.new_name.as_str(),
            None,
            parameters,
        )),
    });

    Ok(Some((abi_function, toplevel_function, impl_function)))
}

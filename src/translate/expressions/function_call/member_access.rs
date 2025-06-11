use crate::{error::Error, project::Project, sway, translate::*};
use convert_case::Case;
use num_bigint::BigUint;
use num_traits::One;
use solang_parser::{helpers::CodeLocation, pt as solidity};
use std::{cell::RefCell, rc::Rc};

pub fn translate_member_access_function_call(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: Rc<RefCell<TranslationScope>>,
    function: &solidity::Expression,
    args: &[solidity::Expression],
    expression: &solidity::Expression,
    arguments: &[solidity::Expression]
) -> Result<sway::Expression, Error> {
    // timelock.executeTransaction.value(proposal.values[i])
    match function {
        solidity::Expression::MemberAccess(_, container, member) => {
            // timelock.executeTransaction
            // .value

            let mut coins = None;
            let mut gas = None;

            match member.name.as_str() {
                "value" if args.len() == 1 => {
                    coins = Some(translate_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        &args[0],
                    )?)
                }

                "gas" if args.len() == 1 => {
                    gas = Some(translate_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        &args[0],
                    )?)
                }

                _ => todo!("translate member function call: {member}"),
            };

            match container.as_ref() {
                solidity::Expression::MemberAccess(_, container, member) => {
                    let variable = translate_variable_access_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        container,
                    )?
                    .map(|v| v.variable);

                    let mut container = translate_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        container,
                    )?;

                    let type_name = module.borrow_mut().get_expression_type(project, scope.clone(), &container)?;

                    match type_name {
                        sway::TypeName::Undefined => panic!("Undefined type name"),

                        sway::TypeName::Identifier { name, .. } => match name.as_str() {
                            "Identity" => match member.name.as_str() {
                                "call" => {
                                    if arguments.len() != 1 {
                                        panic!("Malformed `address.call` call, expected 1 argument, found {}", arguments.len());
                                    }

                                    let payload = translate_expression(project, module.clone(), scope.clone(), &arguments[0])?;
                                    translate_address_call_expression(project, module.clone(), scope.clone(), &payload, coins, None, gas)
                                }

                                _ => {
                                    let mut name = name.clone();
                                    let external_function_new_name = translate_naming_convention(member.name.as_str(), Case::Snake);

                                    // Check if expression is a variable that had an ABI type
                                    if let Some(variable) = variable.as_ref() {
                                        if variable.is_none() {
                                            panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
                                        }
                                        let variable = variable.as_ref().unwrap();
                                        let variable = variable.borrow();

                                        if let Some(abi_type_name) = variable.abi_type_name.as_ref() {
                                            let abi_type_name = abi_type_name.to_string();

                                            // Turn the expression into an ABI cast:
                                            // abi(T, x.as_contract_id().unwrap().into())
                                            container = sway::Expression::create_function_calls(None, &[
                                                ("abi", Some((None, vec![
                                                    sway::Expression::create_identifier(abi_type_name.clone()),
                                                    sway::Expression::create_function_calls(Some(container), &[
                                                        ("as_contract_id", Some((None, vec![]))),
                                                        ("unwrap", Some((None, vec![]))),
                                                        ("into", Some((None, vec![]))),
                                                    ]),
                                                ]))),
                                            ]);

                                            name = abi_type_name.to_string();
                                        }
                                    }

                                    // Check to see if the type is located in an external ABI
                                    if let Some(external_definition) = project.find_module_with_contract(&name) {
                                        if external_definition.borrow().functions.iter().any(|f| {
                                            let sway::TypeName::Function { new_name, .. } = &f.signature else { unreachable!() };
                                            *new_name == external_function_new_name
                                        }) {
                                            //
                                            // TODO: Ensure a use statement for the ABI is added to the current module
                                            //

                                            let mut fields = vec![];

                                            if let Some(coins) = coins {
                                                fields.push(sway::ConstructorField {
                                                    name: "coins".into(),
                                                    value: coins,
                                                });
                                            }

                                            if let Some(gas) = gas {
                                                fields.push(sway::ConstructorField {
                                                    name: "gas".into(),
                                                    value: gas,
                                                });
                                            }

                                            return Ok(sway::Expression::from(sway::FunctionCallBlock {
                                                function: sway::Expression::create_member_access(container, &[external_function_new_name.as_str()]),
                                                generic_parameters: None,
                                                fields,
                                                parameters: arguments.iter()
                                                    .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
                                                    .collect::<Result<Vec<_>, _>>()?,
                                            }));
                                        }
                                    }

                                    todo!("translate Identity member function call block `{member}` : {} - {container:#?}", sway::TabbedDisplayer(&container))
                                }
                            }

                            _ => todo!("translate {name} member function call block: {} - {container:#?}", sway::TabbedDisplayer(&container))
                        }

                        _ => todo!(),
                    }
                }

                _ => todo!("translate member function call: {member}"),
            }
        }

        _ => todo!("translate function call: {function}"),
    }
}

pub fn translate_function_call_block_member_access(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
    arguments: &[solidity::Expression],
    container: &solidity::Expression,
    block: &solidity::Statement,
    member: &solidity::Identifier,
) -> Result<sway::Expression, Error> {
    let variable = translate_variable_access_expression(
        project,
        module.clone(),
        scope.clone(),
        container,
    )?
    .map(|v| v.variable);

    let mut container = translate_expression(project, module.clone(), scope.clone(), container)?;
    let type_name = module.borrow_mut().get_expression_type(project, scope.clone(), &container)?;

    let solidity::Statement::Args(_, block_args) = block else {
        panic!("Malformed `address.call` call, expected args block, found: {block:#?}");
    };

    let mut coins = None;
    let mut gas = None;

    for block_arg in block_args.iter() {
        match block_arg.name.name.as_str() {
            "value" => {
                coins = Some(translate_expression(
                    project,
                    module.clone(),
                    scope.clone(),
                    &block_arg.expr,
                )?)
            }

            "gas" => {
                gas = Some(translate_expression(
                    project,
                    module.clone(),
                    scope.clone(),
                    &block_arg.expr,
                )?)
            }

            arg => todo!("address.transfer block arg: {arg}"),
        }
    }

    match type_name {
        sway::TypeName::Undefined => panic!("Undefined type name"),

        sway::TypeName::Identifier { name, .. } => match name.as_str() {
            "Identity" => match member.name.as_str() {
                "call" => {
                    if arguments.len() != 1 {
                        panic!("Malformed `address.call` call, expected 1 argument, found {}", arguments.len());
                    }

                    let payload = translate_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        &arguments[0],
                    )?;
                    
                    translate_address_call_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        &payload,
                        coins,
                        None,
                        gas,
                    )
                }

                _ => {
                    let mut name = name.clone();
                    let external_function_new_name = translate_naming_convention(
                        member.name.as_str(),
                        Case::Snake,
                    );

                    // Check if expression is a variable that had an ABI type
                    if let Some(variable) = variable.as_ref() {
                        if variable.is_none() {
                            panic!(
                                "Variable not found: {}",
                                sway::TabbedDisplayer(&expression)
                            );
                        }
                        let variable = variable.as_ref().unwrap();
                        let variable = variable.borrow();

                        if let Some(abi_type_name) = variable.abi_type_name.as_ref() {
                            let abi_type_name = abi_type_name.to_string();

                            // Turn the expression into an ABI cast:
                            // abi(T, x.as_contract_id().unwrap().into())
                            container = sway::Expression::create_function_calls(
                                None,
                                &[(
                                    "abi",
                                    Some((
                                        None,
                                        vec![
                                            sway::Expression::create_identifier(abi_type_name.clone()),
                                            sway::Expression::create_function_calls(
                                                Some(container),
                                                &[
                                                    (
                                                        "as_contract_id",
                                                        Some((None, vec![])),
                                                    ),
                                                    ("unwrap", Some((None, vec![]))),
                                                    ("into", Some((None, vec![]))),
                                                ],
                                            ),
                                        ],
                                    )),
                                )],
                            );

                            name = abi_type_name.to_string();
                        }
                    }

                    // Check to see if the type is located in an external ABI
                    if let Some(external_definition) = project.find_module_with_contract(&name) {
                        if external_definition.borrow()
                            .functions
                            .iter()
                            .any(|f| {
                                let sway::TypeName::Function { new_name, .. } = &f.signature else { unreachable!() };
                                *new_name == external_function_new_name
                            })
                        {
                            //
                            // TODO: Ensure a use statement for the ABI is added to the current module
                            //
                            
                            let mut fields = vec![];

                            if let Some(coins) = coins {
                                fields.push(sway::ConstructorField {
                                    name: "coins".into(),
                                    value: coins,
                                });
                            }

                            if let Some(gas) = gas {
                                fields.push(sway::ConstructorField {
                                    name: "gas".into(),
                                    value: gas,
                                });
                            }

                            return Ok(sway::Expression::from(
                                sway::FunctionCallBlock {
                                    function: sway::Expression::create_member_access(
                                        container,
                                        &[external_function_new_name.as_str()],
                                    ),
                                    generic_parameters: None,
                                    fields,
                                    parameters: arguments
                                        .iter()
                                        .map(|a| {
                                            translate_expression(
                                                project,
                                                module.clone(),
                                                scope.clone(),
                                                a,
                                            )
                                        })
                                        .collect::<Result<Vec<_>, _>>()?,
                                },
                            ));
                        }
                    }

                    todo!("translate Identity member function call block `{member}{}`: {} - {container:#?}", block.to_string(), sway::TabbedDisplayer(&container))
                }
            },

            _ => todo!(
                "translate {name} member function call block: {} - {container:#?}",
                sway::TabbedDisplayer(&container)
            ),
        },

        _ => todo!(),
    }
}

pub fn translate_identity_member_access_function_call(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
    arguments: &[solidity::Expression],
    mut container: sway::Expression,
    member: &solidity::Identifier,
    solidity_container: &solidity::Expression,
    name: String,
    function: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    match member.name.as_str() {
        // to.transfer(amount) => std::asset::transfer(to, asset_id, amount)
        "transfer" if arguments.len() == 1 => {
            let argument = translate_expression(project, module.clone(), scope.clone(), &arguments[0])?;
            let argument_type_name = module.borrow_mut().get_expression_type(project, scope.clone(), &argument)?;
            
            let container_type = sway::TypeName::Identifier { name: "u64".to_string(), generic_parameters: None };
            
            let argument = coerce_expression(&argument, &argument_type_name, &container_type).unwrap();
            
            return Ok(sway::Expression::create_function_calls(None, &[
                ("std::asset::transfer", Some((None, vec![
                    container,
                    sway::Expression::create_function_calls(None, &[("AssetId::default", Some((None, vec![])))]),
                    argument
                    ,
                ]))),
            ]));
        }

        // to.send(amount) => {
        //     std::asset::transfer(to, asset_id, amount);
        //     true
        // }
        "send" if arguments.len() == 1 => {
            let argument = translate_expression(project, module.clone(), scope.clone(), &arguments[0])?;
            let argument_type_name = module.borrow_mut().get_expression_type(project, scope.clone(), &argument)?;

            let u64_type = sway::TypeName::Identifier { name: "u64".to_string(), generic_parameters: None };
            
            let argument = coerce_expression(&argument, &argument_type_name, &u64_type).unwrap();
            
            return Ok(sway::Expression::from(sway::Block {
                statements: vec![
                    sway::Statement::from(sway::Expression::create_function_calls(None, &[
                        ("std::asset::transfer", Some((None, vec![
                            container,
                            sway::Expression::create_function_calls(None, &[("AssetId::default", Some((None, vec![])))]),
                            argument,
                        ]))),
                    ])),
                ],
                final_expr: Some(sway::Expression::from(sway::Literal::Bool(true))),
            }));
        }

        "call" if arguments.len() == 1 => {
            let payload = translate_expression(project, module.clone(), scope.clone(), &arguments[0])?;
            return translate_address_call_expression(project, module.clone(), scope.clone(), &payload, None, None, None);
        }

        "delegatecall" => {
            //
            // TODO: is delegatecall possible?
            //

            return Ok(sway::Expression::create_todo(Some(expression.to_string())));
        }

        "staticcall" => {
            //
            // TODO: is staticcall possible?
            //

            return Ok(sway::Expression::create_todo(Some(expression.to_string())));
        }

        _ => {}
    }

    let mut name = name.clone();
    let new_name_lower = translate_naming_convention(member.name.as_str(), Case::Snake);
    let new_name_upper = translate_naming_convention(member.name.as_str(), Case::Constant);

    //
    // TODO:
    //
    // // Check using directives for Identity-specific function
    // for using_directive in module.using_directives.iter() {
    //     let Some(external_definition) =
    //         project.find_module_with_contract(&using_directive.library_name).cloned()
    //     else {
    //         continue;
    //     };
    //
    //     if let Some(for_type_name) = &using_directive.for_type {
    //         if !type_name.is_identity() && for_type_name != type_name {
    //             // println!(
    //             //     "Using directive type {} is not {}, skipping...",
    //             //     sway::TabbedDisplayer(for_type_name),
    //             //     sway::TabbedDisplayer(&type_name),
    //             // );
    //             continue;
    //         }
    //     }
    //
    //     for f in external_definition.toplevel_scope.borrow().functions.iter() {
    //         let f = f.borrow();
    //
    //         let sway::TypeName::Function { parameters: f_parameters, .. } = &f.type_name else {
    //             panic!("Invalid function type name: {:#?}", f.type_name)
    //         };
    //
    //         if f.old_name != member.name {
    //             continue;
    //         }
    //
    //         let Some(parameter) = f_parameters.entries.first() else { continue };
    //         let Some(parameter_type_name) = parameter.type_name.as_ref() else { continue };
    //
    //         if parameter_type_name == type_name {
    //             let mut parameters = arguments.iter()
    //                 .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
    //                 .collect::<Result<Vec<_>, _>>()?;
    //
    //             parameters.insert(0, container.clone());
    //
    //             return Ok(sway::Expression::create_function_calls(None, &[(f.new_name.as_str(), Some((None, parameters)))]));
    //         }
    //     }
    // }

    let variable = match translate_variable_access_expression(project, module.clone(), scope.clone(), solidity_container)? {
        Some(TranslatedVariableAccess { variable, .. }) => variable,
        None => None,
    };

    // Check if expression is a variable that had an ABI type
    if let Some(variable) = variable.as_ref() {
        let variable = variable.borrow();

        if let Some(abi_type_name) = variable.abi_type_name.as_ref() {
            let abi_type_name = abi_type_name.to_string();

            // Turn the expression into an ABI cast:
            // abi(T, x.as_contract_id().unwrap().into())
            container = sway::Expression::create_function_calls(None, &[
                ("abi", Some((None, vec![
                    sway::Expression::create_identifier(abi_type_name.clone()),
                    sway::Expression::create_function_calls(Some(container), &[
                        ("as_contract_id", Some((None, vec![]))),
                        ("unwrap", Some((None, vec![]))),
                        ("into", Some((None, vec![]))),
                    ]),
                ]))),
            ]);

            name = abi_type_name;
        }
    } else {
        let mut expression = container.clone();
        
        // HACK: remove `.read()`
        if let sway::Expression::FunctionCall(f) = &expression {
            if let sway::Expression::MemberAccess(m) = &f.function {
                if m.member == "read" && f.parameters.is_empty() {
                    expression = m.expression.clone();
                }
            }
        }

        if let sway::Expression::MemberAccess(m) = &expression {
            if let sway::Expression::PathExpr(path_expr) = &m.expression {
                if let sway::PathExprRoot::Identifier(root_ident) =  &path_expr.root {
                    if root_ident == "storage" {
                        let mut storage_namespace: Option<sway::StorageNamespace> = None;
            
                        for segment in path_expr.segments.iter() {
                            let namespace = match storage_namespace {
                                None => {
                                    let module = module.borrow(); 
                                    let storage = module.storage.as_ref().unwrap();
                                    storage.namespaces.iter().find(|s| s.name == segment.name).cloned()
                                },
                                Some(storage_namespace) => storage_namespace.namespaces.iter().find(|s| s.name == segment.name).cloned(),
                            };

                            if namespace.is_none() {
                                storage_namespace = None;
                                break;
                            }

                            storage_namespace = namespace;
                        }

                        if let Some(storage_namespace) = storage_namespace {
                            if let Some(storage_field) = storage_namespace.fields.iter().find(|s| s.name == m.member) {
                                if let Some(abi_type_name) = storage_field.abi_type_name.as_ref() {
                                    let abi_type_name = abi_type_name.to_string();

                                    // Turn the expression into an ABI cast:
                                    // abi(T, x.as_contract_id().unwrap().into())
                                    container = sway::Expression::create_function_calls(None, &[
                                        ("abi", Some((None, vec![
                                            sway::Expression::create_identifier(abi_type_name.clone()),
                                            sway::Expression::create_function_calls(Some(container), &[
                                                ("as_contract_id", Some((None, vec![]))),
                                                ("unwrap", Some((None, vec![]))),
                                                ("into", Some((None, vec![]))),
                                            ]),
                                        ]))),
                                    ]);

                                    name = abi_type_name;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Check to see if the type is located in an external ABI
    if let Some(external_definition) = project.find_module_with_contract(&name) {
        // Check lower case names for regular functions
        if external_definition.borrow().functions.iter().any(|f| {
            let sway::TypeName::Function { new_name, .. } = &f.signature else { unreachable!() };
            *new_name == new_name_lower
        }) {
            //
            // TODO: Ensure a use statement for the ABI is added to the current module
            //

            return Ok(sway::Expression::create_function_calls(Some(container.clone()), &[
                (new_name_lower.as_str(), Some((
                    None,
                    arguments.iter()
                        .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
                        .collect::<Result<Vec<_>, _>>()?,
                ))),
            ]));
        }

        // Check upper case names for constant getter functions
        if external_definition.borrow().functions.iter().any(|f| {
            let sway::TypeName::Function { new_name, .. } = &f.signature else { unreachable!() };
            *new_name == new_name_upper
        }) {
            //
            // TODO: Ensure a use statement for the ABI is added to the current module
            //

            return Ok(sway::Expression::create_function_calls(Some(container.clone()), &[
                (new_name_upper.as_str(), Some((
                    None,
                    arguments.iter()
                        .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
                        .collect::<Result<Vec<_>, _>>()?,
                ))),
            ]));
        }
    }

    todo!(
        "{}translate Identity member function call `{member}`: {} - {container:#?}",
        match project.loc_to_line_and_column(module.clone(), &function.loc()) {
            Some((line, col)) => format!("{}:{}:{}: ", project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy(), line, col),
            None => format!("{}: ", project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy()),
        },
        sway::TabbedDisplayer(&container),
    )
}

pub fn translate_storage_vec_member_access_function_call(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: Rc<RefCell<TranslationScope>>,
    arguments: &[solidity::Expression],
    member: &solidity::Identifier,
    solidity_container: &solidity::Expression,
    container: &sway::Expression,
) -> Result<sway::Expression, Error> {
    let Some(TranslatedVariableAccess {
        mut expression,
        ..
    }) = translate_variable_access_expression(project, module.clone(), scope.clone(), solidity_container)?
    else {
        panic!("Failed to translate storage vec member access function call: {}", solidity_container)
    };

    if let sway::Expression::FunctionCall(f) = &expression {
        if let sway::Expression::MemberAccess(m) = &f.function {
            if m.member == "read" && f.parameters.is_empty() {
                let container_type = module.borrow_mut().get_expression_type(project, scope.clone(), &m.expression)?;
                if container_type.is_storage_key() {
                    expression = m.expression.clone();
                }
            }
        }
    }


    let type_name = module.borrow_mut().get_expression_type(project, scope.clone(), &expression)?;

    match member.name.as_str() {
        "push" | "pop" | "remove" => {}

        _ => todo!("translate StorageVec member function call `{member}`: {} - {container:#?}", sway::TabbedDisplayer(container)),
    }
 
    if let Some(storage_key_type) = type_name.storage_key_type() {
        match storage_key_type {
            sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                ("StorageVec", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                    return Ok(sway::Expression::create_function_calls(Some(expression), &[
                        (member.name.as_str(), Some((
                            None,
                            arguments.iter()
                                .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
                                .collect::<Result<Vec<_>, _>>()?,
                        ))),
                    ]))
                },
                
                _ => {},
            },

            _ => {},
        }
    }

    todo!("translate StorageVec member function call `{member}`: {} - {container:#?}", sway::TabbedDisplayer(container))
}

pub fn translate_vec_member_access_function_call(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: Rc<RefCell<TranslationScope>>,
    arguments: &[solidity::Expression],
    member: &solidity::Identifier,
    solidity_container: &solidity::Expression,
    container: &sway::Expression,
) -> Result<sway::Expression, Error> {
    match member.name.as_str() {
        "push" => {
            let (variable, container_access) = match translate_variable_access_expression(project, module.clone(), scope.clone(), solidity_container)? {
                Some(TranslatedVariableAccess { variable, expression }) => (Some(variable), Some(expression)),
                None => (None, None),
            };

            let (Some(_), Some(container_access)) = (variable, container_access) else {
                panic!("Vec is not a variable");
            };

            Ok(sway::Expression::create_function_calls(Some(container_access), &[
                ("push", Some((
                    None,
                    arguments.iter()
                        .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
                        .collect::<Result<Vec<_>, _>>()?,
                ))),
            ]))
        }

        "pop" => {
            let (variable, container_access) = match translate_variable_access_expression(project, module.clone(), scope.clone(), solidity_container)? {
                Some(TranslatedVariableAccess { variable, expression }) => (Some(variable), Some(expression)),
                None => (None, None),
            };

            let (Some(_), Some(container_access)) = (variable, container_access) else {
                panic!("Vec is not a variable");
            };

            Ok(sway::Expression::create_function_calls(Some(container_access), &[("pop", Some((None, vec![])))]))
        }

        "remove" => {
            let (variable, container_access) = match translate_variable_access_expression(project, module.clone(), scope.clone(), solidity_container)? {
                Some(TranslatedVariableAccess { variable, expression }) => (Some(variable), Some(expression)),
                None => (None, None),
            };

            let (Some(_), Some(container_access)) = (variable, container_access) else {
                panic!("Vec is not a variable");
            };

            Ok(sway::Expression::create_function_calls(Some(container_access), &[
                ("remove", Some((
                    None,
                    arguments.iter()
                        .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
                        .collect::<Result<Vec<_>, _>>()?,
                ))),
            ]))
        }

        _ => todo!("translate Vec member function call `{member}`: {} - {container:#?}", sway::TabbedDisplayer(container))
    }
}

pub fn translate_builtin_abi_member_access_function_call(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: Rc<RefCell<TranslationScope>>,
    member_name: &str,
    arguments: &[solidity::Expression],
    named_arguments: Option<&[solidity::NamedArgument]>,
) -> Result<Option<sway::Expression>, Error> {
    match member_name {
        "decode" => {
            // abi.decode(encodedData, (uint256, bool)) =>
            // let (a, b): (u256, bool) = {
            //     let slice = encoded_data.as_raw_slice();
            //     let mut ptr = slice.ptr();
            //     let a = ptr.read::<u256>();
            //     ptr = ptr.add::<u256>(1);
            //     let b = ptr.read::<bool>();
            //     ptr = ptr.add::<bool>(1);
            //     (a, b)
            // };

            if arguments.len() != 2 {
                return Ok(None);
            }

            let encoded_data =
                translate_expression(project, module.clone(), scope.clone(), &arguments[0])?;

            let parameter_types = match &arguments[1] {
                solidity::Expression::List(_, parameter_types) => parameter_types
                    .iter()
                    .map(|(_, p)| {
                        translate_type_name(
                            project,
                            module.clone(),
                            &p.as_ref().unwrap().ty,
                            false,
                            false,
                        )
                    })
                    .collect::<Vec<_>>(),

                solidity::Expression::Parenthesis(_, expression)
                    if matches!(expression.as_ref(), solidity::Expression::Type(_, _)) =>
                {
                    vec![translate_type_name(
                        project,
                        module.clone(),
                        expression,
                        false,
                        false,
                    )]
                }

                _ => {
                    return Ok(None);
                }
            };

            let parameter_names = ('a'..='z')
                .enumerate()
                .take_while(|(i, _)| *i < parameter_types.len())
                .map(|(_, c)| c.to_string())
                .collect::<Vec<_>>();

            if parameter_types.len() != parameter_names.len() {
                return Ok(None);
            }

            // If we only have 1 parameter to decode, just decode it directly
            if parameter_types.len() == 1 {
                // encoded_data.as_raw_slice().ptr().read::<u256>()
                return Ok(Some(sway::Expression::create_function_calls(
                    Some(encoded_data.clone()),
                    &[
                        ("as_raw_slice", Some((None, vec![]))),
                        ("ptr", Some((None, vec![]))),
                        (
                            "read",
                            Some((
                                Some(sway::GenericParameterList {
                                    entries: vec![sway::GenericParameter {
                                        type_name: parameter_types[0].clone(),
                                        implements: None,
                                    }],
                                }),
                                vec![],
                            )),
                        ),
                    ],
                )));
            }

            let mut block = sway::Block {
                statements: vec![
                    // let mut ptr = encoded_data.as_raw_slice().ptr();
                    sway::Statement::from(sway::Let {
                        pattern: sway::LetPattern::from(sway::LetIdentifier {
                            // This only needs to be mutable if there's multiple parameters to decode
                            is_mutable: parameter_names.len() > 1,
                            name: "ptr".into(),
                        }),
                        type_name: None,
                        value: sway::Expression::create_function_calls(
                            Some(encoded_data.clone()),
                            &[
                                ("as_raw_slice", Some((None, vec![]))),
                                ("ptr", Some((None, vec![]))),
                            ],
                        ),
                    }),
                ],
                final_expr: Some(sway::Expression::Tuple(
                    parameter_names
                        .iter()
                        .map(|p| sway::Expression::create_identifier(p.clone()))
                        .collect(),
                )),
            };

            for (i, (parameter_name, parameter_type)) in parameter_names
                .iter()
                .zip(parameter_types.iter())
                .enumerate()
            {
                // let a = ptr.read::<u256>();
                block.statements.push(sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::from(sway::LetIdentifier {
                        is_mutable: false,
                        name: parameter_name.clone(),
                    }),
                    type_name: None,
                    value: sway::Expression::create_function_calls(
                        None,
                        &[
                            ("ptr", None),
                            (
                                "read",
                                Some((
                                    Some(sway::GenericParameterList {
                                        entries: vec![sway::GenericParameter {
                                            type_name: parameter_type.clone(),
                                            implements: None,
                                        }],
                                    }),
                                    vec![],
                                )),
                            ),
                        ],
                    ),
                }));

                // If we have more parameters to decode, increase the ptr
                if i < parameter_names.len() - 1 {
                    // ptr = ptr.add::<u256>(1);
                    block
                        .statements
                        .push(sway::Statement::from(sway::Expression::from(
                            sway::BinaryExpression {
                                operator: "=".into(),
                                lhs: sway::Expression::create_identifier("ptr".into()),
                                rhs: sway::Expression::create_function_calls(
                                    None,
                                    &[
                                        ("ptr", None),
                                        (
                                            "add",
                                            Some((
                                                Some(sway::GenericParameterList {
                                                    entries: vec![sway::GenericParameter {
                                                        type_name: parameter_type.clone(),
                                                        implements: None,
                                                    }],
                                                }),
                                                vec![sway::Expression::from(
                                                    sway::Literal::DecInt(BigUint::one(), None),
                                                )],
                                            )),
                                        ),
                                    ],
                                ),
                            },
                        )));
                }
            }

            Ok(Some(sway::Expression::from(block)))
        }

        "encode" | "encodePacked" => {
            // abi.encode(a, b, ...) | abi.encodePacked(a, b, ...) => {
            //     let mut bytes = Bytes::new();
            //     bytes.append(Bytes::from(std::codec::encode(a)));
            //     bytes.append(Bytes::from(std::codec::encode(b)));
            //     // ...
            //     bytes
            // }

            // Ensure `std::bytes::Bytes` is imported
            module.borrow_mut().ensure_use_declared("std::bytes::Bytes");

            // Generate a unique variable name
            let variable_name = scope.borrow_mut().generate_unique_variable_name("bytes");

            let parameters = arguments
                .iter()
                .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
                .collect::<Result<Vec<_>, _>>()?;

            // Create the abi encoding block
            let mut block = sway::Block {
                statements: vec![sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::from(sway::LetIdentifier {
                        is_mutable: true,
                        name: variable_name.clone(),
                    }),
                    type_name: None,
                    value: sway::Expression::create_function_calls(
                        None,
                        &[("Bytes::new", Some((None, vec![])))],
                    ),
                })],
                final_expr: Some(sway::Expression::create_identifier(variable_name.clone())),
            };

            // Add the encoding statements to the block
            for parameter in parameters {
                block.statements.push(sway::Statement::from(
                    sway::Expression::create_function_calls(
                        None,
                        &[
                            (variable_name.as_str(), None),
                            (
                                "append",
                                Some((
                                    None,
                                    vec![sway::Expression::create_function_calls(
                                        None,
                                        &[(
                                            "Bytes::from",
                                            Some((
                                                None,
                                                vec![sway::Expression::create_function_calls(
                                                    None,
                                                    &[(
                                                        "std::codec::encode",
                                                        Some((None, vec![
                                                            match module.borrow_mut().get_expression_type(project, scope.clone(), &parameter)? {
                                                                sway::TypeName::Identifier { name, generic_parameters } => {
                                                                    match (name.as_str(), generic_parameters.as_ref()) {
                                                                        ("Identity", None) => {
                                                                            let identity_variant_branch = |name: &str| -> sway::MatchBranch {
                                                                                sway::MatchBranch {
                                                                                    pattern: sway::Expression::create_function_calls(None, &[
                                                                                        (format!("Identity::{name}").as_str(), Some((None, vec![
                                                                                            sway::Expression::create_identifier("x".into()),
                                                                                        ]))),
                                                                                    ]),
                                                                                    value: sway::Expression::create_function_calls(None, &[
                                                                                        ("x", None),
                                                                                        ("bits", Some((None, vec![]))),
                                                                                    ]),
                                                                                }
                                                                            };
                                                    
                                                                            sway::Expression::from(sway::Match {
                                                                                expression: parameter,
                                                                                branches: vec![
                                                                                    identity_variant_branch("Address"),
                                                                                    identity_variant_branch("ContractId"),
                                                                                ],
                                                                            })
                                                                        },
                                                                        
                                                                        ("I8" | "I16" | "I32" | "I64" | "I128" | "I256", None) => {
                                                                            sway::Expression::create_function_calls(Some(parameter), &[
                                                                                ("underlying", Some((None, vec![])))
                                                                            ])
                                                                        }

                                                                        _ => parameter
                                                                    }
                                                                }
                                                                _ => parameter
                                                            }
                                                        ])),
                                                    )],
                                                )],
                                            )),
                                        )],
                                    )],
                                )),
                            ),
                        ],
                    ),
                ));
            }

            Ok(Some(sway::Expression::from(block)))
        }

        "encodeWithSelector" => {
            // abi.encodeWithSelector(selector, ...) => ???

            //
            // TODO: how should this be handled?
            //

            Ok(Some(sway::Expression::create_todo(Some(
                format!(
                    "abi.encodeWithSelector({})",
                    arguments.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", "),
                )
            ))))
        }

        "encodeWithSignature" => {
            // abi.encodeWithSignature(signature, ...) => ???

            //
            // TODO: how should this be handled?
            //

            Ok(Some(sway::Expression::create_todo(Some(
                format!(
                    "abi.encodeWithSignature({})",
                    arguments.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", "),
                )
            ))))
        }

        "encodeCall" => {
            // abi.encodeCall(functionPointer, (...)) => ???

            //
            // TODO: how should this be handled?
            //

            Ok(Some(sway::Expression::create_todo(Some(
                format!(
                    "abi.encodeCall({})",
                    arguments.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", "),
                )
            ))))
        }

        _ => Ok(None),
    }
}

pub fn translate_super_member_access_function_call(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: Rc<RefCell<TranslationScope>>,
    member: &str,
    arguments: &[solidity::Expression],
    named_arguments: Option<&[solidity::NamedArgument]>,
) -> Result<Option<sway::Expression>, Error> {
    let parameters = arguments
        .iter()
        .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
        .collect::<Result<Vec<_>, _>>()?;

    let parameter_types = parameters
        .iter()
        .map(|p| module.borrow_mut().get_expression_type(project, scope.clone(), p))
        .collect::<Result<Vec<_>, _>>()?;

    // TODO
    // for inherit in module.borrow().inherits.clone() {
    //     let Some(inherited_definition) = project
    //         .translated_modules
    //         .iter()
    //         .find(|d| d.name == inherit)
    //         .cloned()
    //     else {
    //         panic!("Failed to find inherited definition for `{inherit}`");
    //     };

    //     // Try to resolve the function call
    //     if let Some(result) = resolve_function_call(
    //         project,
    //         module.clone(),
    //         scope.clone(),
    //         &inherited_definition.toplevel_scope,
    //         member.name.as_str(),
    //         named_arguments,
    //         parameters.clone(),
    //         parameter_types.clone(),
    //     )? {
    //         return Ok(result);
    //     }
    // }

    Ok(None)
}

pub fn translate_this_member_access_function_call(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: Rc<RefCell<TranslationScope>>,
    member: &str,
    arguments: &[solidity::Expression],
    named_arguments: Option<&[solidity::NamedArgument]>,
) -> Result<Option<sway::Expression>, Error> {
    let parameters = arguments
        .iter()
        .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
        .collect::<Result<Vec<_>, _>>()?;

    let parameter_types = parameters
        .iter()
        .map(|p| module.borrow_mut().get_expression_type(project, scope.clone(), p))
        .collect::<Result<Vec<_>, _>>()?;

    if let Some(result) = resolve_function_call(
        project,
        module.clone(),
        scope.clone(),
        module.clone(),
        member,
        named_arguments,
        parameters,
        parameter_types,
    )? {
        return Ok(Some(result));
    }
    
    Ok(None)
}

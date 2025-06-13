use crate::{error::Error, project::Project, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_function_name(
    _project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    function_definition: &solidity::FunctionDefinition,
) -> String {
    // Generate the function signature
    let mut signature = function_definition
        .name
        .as_ref()
        .map(|i| i.name.clone())
        .unwrap_or_default();

    signature.push('(');

    for (i, (_, parameter)) in function_definition.params.iter().enumerate() {
        signature = format!(
            "{signature}{}{}",
            if i > 0 { "," } else { "" },
            parameter.as_ref().unwrap().ty,
        );
    }

    signature.push(')');

    let mut module = module.borrow_mut();

    // Add the translated function name to the function names mapping if we haven't already
    if !module.function_names.contains_key(&signature) {
        let old_name = function_definition
            .name
            .as_ref()
            .map(|i| i.name.clone())
            .unwrap_or_default();

        let mut new_name = translate_naming_convention(old_name.as_str(), Case::Snake);

        // Increase the function name count
        let count = module
            .function_name_counts
            .entry(new_name.clone())
            .or_insert(0);
        *count += 1;

        // Append the function name count to the end of the function name if there is more than 1
        if *count > 1 {
            new_name = format!("{new_name}_{}", *count);
        }

        module.function_names.insert(signature.clone(), new_name);
    }

    module.function_names.get(&signature).unwrap().clone()
}

#[inline]
pub fn translate_function_declaration(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    contract_name: Option<&str>,
    function_definition: &solidity::FunctionDefinition,
) -> Result<ir::Function, Error> {
    let (old_name, mut new_name) = match &function_definition.ty {
        solidity::FunctionTy::Function | solidity::FunctionTy::Modifier => {
            let old_name = function_definition.name.as_ref().unwrap().name.clone();
            let new_name = translate_function_name(project, module.clone(), function_definition);
            (old_name, new_name)
        }
        _ => (String::new(), function_definition.ty.to_string()),
    };

    if let Some(contract_name) = contract_name {
        new_name = format!("{}_{}", contract_name.to_case(Case::Snake), new_name);
    }

    // Create a scope for modifier invocation translations
    let scope = Rc::new(RefCell::new(ir::Scope::new(contract_name, None)));

    // Add the function parameters to the scope
    for (_, p) in function_definition.params.iter() {
        let Some(p) = p else { continue };
        let Some(parameter_identifier) = p.name.as_ref() else {
            continue;
        };

        let old_name = parameter_identifier.name.clone();
        let new_name = translate_naming_convention(old_name.as_str(), Case::Snake);
        let type_name =
            translate_type_name(project, module.clone(), scope.clone(), &p.ty, false, true);

        scope
            .borrow_mut()
            .add_variable(Rc::new(RefCell::new(ir::Variable {
                old_name,
                new_name,
                type_name,
                ..Default::default()
            })));
    }

    let mut constructor_calls = vec![];
    let mut modifiers = vec![];

    // Translate the function's constructor/modifier invocations
    for attr in function_definition.attributes.iter() {
        let solidity::FunctionAttribute::BaseOrModifier(_, base) = attr else {
            continue;
        };

        let old_name = base
            .name
            .identifiers
            .iter()
            .map(|i| i.name.clone())
            .collect::<Vec<_>>()
            .join(".");
        let new_name = translate_naming_convention(old_name.as_str(), Case::Snake);

        let parameters = base
            .args
            .as_ref()
            .map(|args| {
                args.iter()
                    .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
                    .collect::<Result<Vec<_>, _>>()
            })
            .unwrap_or_else(|| Ok(vec![]))?;

        // Check to see if base is a constructor call
        if project.find_contract(old_name.as_str()).is_some() {
            let prefix = translate_naming_convention(old_name.as_str(), Case::Snake);
            let name = format!("{prefix}_constructor");

            constructor_calls.push(sway::FunctionCall {
                function: sway::Expression::create_identifier(name),
                generic_parameters: None,
                parameters,
            });

            continue;
        }

        // Add the base to the modifiers list
        modifiers.push(sway::FunctionCall {
            function: sway::Expression::create_identifier(new_name),
            generic_parameters: None,
            parameters,
        });
    }

    // Translate the functions parameters
    let mut parameters = sway::ParameterList::default();

    for (_, parameter) in function_definition.params.iter() {
        let old_name = parameter
            .as_ref()
            .unwrap()
            .name
            .as_ref()
            .map_or("_".into(), |n| n.name.clone());

        let new_name = translate_naming_convention(old_name.as_str(), Case::Snake);

        let mut type_name = translate_type_name(
            project,
            module.clone(),
            scope.clone(),
            &parameter.as_ref().unwrap().ty,
            false,
            true,
        );

        // Check if the parameter's type is an ABI
        if let sway::TypeName::Identifier {
            name,
            generic_parameters: None,
        } = &type_name
        {
            if project.find_contract(&name).is_some() {
                type_name = sway::TypeName::Identifier {
                    name: "Identity".into(),
                    generic_parameters: None,
                };
            }
        }

        parameters.entries.push(sway::Parameter {
            is_ref: false,
            is_mut: false,
            name: new_name,
            type_name: Some(type_name),
        });
    }

    let is_fallback = matches!(function_definition.ty, solidity::FunctionTy::Fallback);

    // Translate the function
    let translated_function = ir::Function {
        old_name: old_name.clone(),
        new_name: new_name.clone(),
        attributes: if is_fallback {
            Some(sway::AttributeList {
                attributes: vec![sway::Attribute {
                    name: "fallback".into(),
                    parameters: None,
                }],
            })
        } else {
            None
        },
        constructor_calls,
        modifiers,
        type_name: sway::TypeName::Function {
            old_name,
            new_name,
            generic_parameters: None,
            parameters,
            return_type: if function_definition.returns.is_empty() {
                None
            } else {
                Some(Box::new(if function_definition.returns.len() == 1 {
                    let type_name = translate_type_name(
                        project,
                        module.clone(),
                        scope.clone(),
                        &function_definition.returns[0].1.as_ref().unwrap().ty,
                        false,
                        true,
                    );
                    translate_return_type_name(project, module.clone(), &type_name)
                } else {
                    sway::TypeName::Tuple {
                        type_names: function_definition
                            .returns
                            .iter()
                            .map(|(_, p)| {
                                let type_name = translate_type_name(
                                    project,
                                    module.clone(),
                                    scope.clone(),
                                    &p.as_ref().unwrap().ty,
                                    false,
                                    true,
                                );
                                translate_return_type_name(project, module.clone(), &type_name)
                            })
                            .collect(),
                    }
                }))
            },
        },
    };

    Ok(translated_function)
}

#[inline]
pub fn translate_modifier_definition(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    contract_name: Option<&str>,
    function_definition: &solidity::FunctionDefinition,
) -> Result<(), Error> {
    let old_name = function_definition.name.as_ref().unwrap().name.clone();
    let new_name = translate_naming_convention(old_name.as_str(), Case::Snake);

    // println!(
    //     "Translating modifier {}.{} at {}",
    //     module.borrow().name,
    //     function_definition
    //         .name
    //         .as_ref()
    //         .map(|n| n.name.as_str())
    //         .unwrap_or_else(|| new_name.as_str()),
    //     project.loc_to_file_location_string(module.clone(), &function_definition.loc),
    // );

    let mut modifier = ir::Modifier {
        old_name: old_name.clone(),
        new_name: new_name.clone(),
        parameters: sway::ParameterList::default(),
        attributes: None,
        has_underscore: false,
        pre_body: None,
        post_body: None,
    };

    let scope = Rc::new(RefCell::new(ir::Scope::new(contract_name, None)));

    for (_, p) in function_definition.params.iter() {
        let old_name = p
            .as_ref()
            .unwrap()
            .name
            .as_ref()
            .map(|p| p.name.clone())
            .unwrap_or_else(String::new);

        let new_name = if old_name.is_empty() {
            println!("WARNING: found unnamed parameter");
            // TODO: we should generate a unique parameter name
            "_".to_string()
        } else {
            translate_naming_convention(old_name.as_str(), Case::Snake)
        };

        let type_name = translate_type_name(
            project,
            module.clone(),
            scope.clone(),
            &p.as_ref().unwrap().ty,
            false,
            true,
        );

        modifier.parameters.entries.push(sway::Parameter {
            name: new_name.clone(),
            type_name: Some(type_name.clone()),
            ..Default::default()
        });

        scope
            .borrow_mut()
            .add_variable(Rc::new(RefCell::new(ir::Variable {
                old_name,
                new_name,
                type_name,
                ..Default::default()
            })));
    }

    let solidity::Statement::Block { statements, .. } = function_definition.body.as_ref().unwrap()
    else {
        panic!(
            "Invalid modifier body, expected block, found: {:#?}",
            function_definition.body
        );
    };

    let mut current_body: &mut Option<sway::Block> = &mut modifier.pre_body;
    let mut current_scope = Rc::new(RefCell::new(scope.borrow().clone()));

    let mut has_pre_storage_read = false;
    let mut has_pre_storage_write = false;

    let mut has_post_storage_read = false;
    let mut has_post_storage_write = false;

    let mut has_storage_read = &mut has_pre_storage_read;
    let mut has_storage_write = &mut has_pre_storage_write;

    for statement in statements.iter() {
        // If we encounter the underscore statement, every following statement goes into the modifier's post_body block.
        if let solidity::Statement::Expression(
            _,
            solidity::Expression::Variable(solidity::Identifier { name, .. }),
        ) = statement
        {
            if name == "_" {
                modifier.has_underscore = true;

                if let Some(block) = current_body.as_mut() {
                    //
                    // TODO: check if any storage fields were read from or written to
                    //

                    let mut scope = Some(current_scope.clone());

                    while let Some(current_scope) = scope.clone() {
                        // for variable in current_scope.borrow_mut().variables.iter() {
                        //     //
                        //     // TODO: check if variable is a storage key that was read from or written to
                        //     //

                        //     if *has_storage_read && *has_storage_write {
                        //         break;
                        //     }
                        // }

                        if *has_storage_read && *has_storage_write {
                            break;
                        }

                        scope.clone_from(&current_scope.borrow().get_parent())
                    }

                    finalize_block_translation(project, current_scope.clone(), block)?;
                }

                current_body = &mut modifier.post_body;

                let mut new_scope = ir::Scope::new(
                    scope
                        .borrow()
                        .get_contract_name()
                        .as_ref()
                        .map(|s| s.as_str()),
                    scope.borrow().get_parent(),
                );

                for v in scope.borrow().get_variables() {
                    let mut v = v.borrow().clone();
                    v.statement_index = None;
                    new_scope.add_variable(Rc::new(RefCell::new(v)));
                }

                current_scope = Rc::new(RefCell::new(new_scope));

                has_storage_read = &mut has_post_storage_read;
                has_storage_write = &mut has_post_storage_write;

                continue;
            }
        }

        // Create the current body block if it hasn't already been.
        if current_body.is_none() {
            *current_body = Some(sway::Block::default());
        }

        let block = current_body.as_mut().unwrap();

        // Translate the statement
        let sway_statement =
            translate_statement(project, module.clone(), current_scope.clone(), statement)?;

        // Store the index of the sway statement
        let statement_index = block.statements.len();

        // Add the sway statement to the sway block
        block.statements.push(sway_statement);

        // If the sway statement is a variable declaration, keep track of its statement index
        if let Some(sway::Statement::Let(sway_variable)) = block.statements.last() {
            let store_variable_statement_index = |id: &sway::LetIdentifier| {
                let scope = current_scope.borrow_mut();
                let scope_variables = scope.get_variables();
                let scope_entry = scope_variables
                    .iter()
                    .rev()
                    .find(|v| v.borrow().new_name == id.name)
                    .unwrap();
                scope_entry.borrow_mut().statement_index = Some(statement_index);
            };

            match &sway_variable.pattern {
                sway::LetPattern::Identifier(id) => store_variable_statement_index(id),
                sway::LetPattern::Tuple(ids) => ids.iter().for_each(store_variable_statement_index),
            }
        }
    }

    if let Some(block) = current_body.as_mut() {
        finalize_block_translation(project, current_scope.clone(), block)?;
    }

    let create_attributes =
        |has_storage_read: bool, has_storage_write: bool| -> Option<sway::AttributeList> {
            let mut parameters = vec![];

            if has_storage_read {
                parameters.push("read".to_string());
            }

            if has_storage_write {
                parameters.push("write".to_string());
            }

            if parameters.is_empty() {
                None
            } else {
                Some(sway::AttributeList {
                    attributes: vec![sway::Attribute {
                        name: "storage".into(),
                        parameters: Some(parameters),
                    }],
                })
            }
        };

    // Ensure that an underscore statement was encountered while translating the modifier
    if !modifier.has_underscore {
        panic!(
            "Malformed modifier missing underscore statement: {}",
            modifier.old_name
        );
    }

    // Generate toplevel modifier functions
    match (modifier.pre_body.as_ref(), modifier.post_body.as_ref()) {
        (Some(pre_body), Some(post_body)) => {
            let modifier_pre_function_name = format!("{}_pre", modifier.new_name);

            module.borrow_mut().functions.push(ir::Item {
                signature: sway::TypeName::Function {
                    old_name: String::new(),
                    new_name: String::new(),
                    generic_parameters: None,
                    parameters: modifier.parameters.clone(),
                    return_type: None,
                },
                implementation: Some(sway::Function {
                    attributes: create_attributes(has_pre_storage_read, has_pre_storage_write),
                    is_public: false,
                    old_name: String::new(), // TODO
                    name: modifier_pre_function_name.clone(),
                    generic_parameters: None,
                    parameters: modifier.parameters.clone(),
                    return_type: None,
                    body: Some(pre_body.clone()),
                }),
            });

            let modifier_post_function_name = format!("{}_post", modifier.new_name);

            module.borrow_mut().functions.push(ir::Item {
                signature: sway::TypeName::Function {
                    old_name: String::new(),
                    new_name: String::new(),
                    generic_parameters: None,
                    parameters: modifier.parameters.clone(),
                    return_type: None,
                },
                implementation: Some(sway::Function {
                    attributes: create_attributes(has_post_storage_read, has_post_storage_write),
                    is_public: false,
                    old_name: String::new(), // TODO
                    name: modifier_post_function_name.clone(),
                    generic_parameters: None,
                    parameters: modifier.parameters.clone(),
                    return_type: None,
                    body: Some(post_body.clone()),
                }),
            });
        }

        (Some(pre_body), None) => {
            module.borrow_mut().functions.push(ir::Item {
                signature: sway::TypeName::Function {
                    old_name: String::new(),
                    new_name: String::new(),
                    generic_parameters: None,
                    parameters: modifier.parameters.clone(),
                    return_type: None,
                },
                implementation: Some(sway::Function {
                    attributes: create_attributes(has_pre_storage_read, has_pre_storage_write),
                    is_public: false,
                    old_name: modifier.old_name.clone(),
                    name: modifier.new_name.clone(),
                    generic_parameters: None,
                    parameters: modifier.parameters.clone(),
                    return_type: None,
                    body: Some(pre_body.clone()),
                }),
            });
        }

        (None, Some(post_body)) => {
            module.borrow_mut().functions.push(ir::Item {
                signature: sway::TypeName::Function {
                    old_name: String::new(),
                    new_name: String::new(),
                    generic_parameters: None,
                    parameters: modifier.parameters.clone(),
                    return_type: None,
                },
                implementation: Some(sway::Function {
                    attributes: create_attributes(has_post_storage_read, has_post_storage_write),
                    is_public: false,
                    old_name: modifier.old_name.clone(),
                    name: modifier.new_name.clone(),
                    generic_parameters: None,
                    parameters: modifier.parameters.clone(),
                    return_type: None,
                    body: Some(post_body.clone()),
                }),
            });
        }

        (None, None) => {
            //
            // TODO:
            //

            // let path = project
            //     .root_folder
            //     .clone()
            //     .unwrap()
            //     .join(module.borrow().path.clone())
            //     .with_extension("sol");

            // panic!(
            //     "{}: ERROR: Malformed modifier missing pre and post bodies",
            //     project.loc_to_file_location_string(module.clone(), &function_definition.loc),
            // );

            return Ok(());
        }
    }

    // Add the translated modifier to the translated definition
    module.borrow_mut().modifiers.push(modifier);

    Ok(())
}

#[inline]
pub fn translate_function_definition(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    contract_name: Option<&str>,
    function_definition: &solidity::FunctionDefinition,
) -> Result<
    (
        sway::Function,
        Option<sway::Function>,
        Option<sway::ImplItem>,
    ),
    Error,
> {
    assert!(!matches!(
        function_definition.ty,
        solidity::FunctionTy::Modifier
    ));

    // Create the scope for the body of the toplevel function
    let scope = Rc::new(RefCell::new(ir::Scope::new(contract_name, None)));

    // Collect information about the function from its type
    let is_constructor = matches!(function_definition.ty, solidity::FunctionTy::Constructor);
    let is_fallback = matches!(function_definition.ty, solidity::FunctionTy::Fallback);
    let is_receive = matches!(function_definition.ty, solidity::FunctionTy::Receive);

    // Collect information about the function from its attributes
    let mut is_public = function_definition.attributes.iter().any(|x| {
        matches!(
            x,
            solidity::FunctionAttribute::Visibility(
                solidity::Visibility::External(_) | solidity::Visibility::Public(_)
            )
        )
    });
    let is_constant = function_definition.attributes.iter().any(|x| {
        matches!(
            x,
            solidity::FunctionAttribute::Mutability(solidity::Mutability::Constant(_))
        )
    });
    let is_pure = function_definition.attributes.iter().any(|x| {
        matches!(
            x,
            solidity::FunctionAttribute::Mutability(solidity::Mutability::Pure(_))
        )
    });
    let is_view = function_definition.attributes.iter().any(|x| {
        matches!(
            x,
            solidity::FunctionAttribute::Mutability(solidity::Mutability::View(_))
        )
    });
    let is_payable = function_definition.attributes.iter().any(|x| {
        matches!(
            x,
            solidity::FunctionAttribute::Mutability(solidity::Mutability::Payable(_))
        )
    });

    let _is_virtual = function_definition
        .attributes
        .iter()
        .any(|x| matches!(x, solidity::FunctionAttribute::Virtual(_)));

    let is_override = function_definition
        .attributes
        .iter()
        .any(|x| matches!(x, solidity::FunctionAttribute::Override(_, _)));

    // If the function is a constructor, we consider it public and add an initializer requirement
    if is_constructor {
        is_public = true;
    }

    let new_name_2 = if is_constructor {
        "constructor".to_string()
    } else if is_fallback {
        "fallback".to_string()
    } else if is_receive {
        "receive".to_string()
    } else {
        translate_function_name(project, module.clone(), function_definition)
    };

    let (old_name, mut new_name) = if matches!(
        function_definition.ty,
        solidity::FunctionTy::Function | solidity::FunctionTy::Modifier
    ) {
        let old_name = function_definition.name.as_ref().unwrap().name.clone();
        (old_name, new_name_2.clone())
    } else {
        (String::new(), new_name_2.clone())
    };

    if let Some(contract_name) = contract_name {
        new_name = format!("{}_{}", contract_name.to_case(Case::Snake), new_name);
    }

    // println!(
    //     "Translating function {}.{} at {}",
    //     module.borrow().name,
    //     function_definition
    //         .name
    //         .as_ref()
    //         .map(|n| n.name.as_str())
    //         .unwrap_or_else(|| new_name_2.as_str()),
    //     project.loc_to_file_location_string(module.clone(), &function_definition.loc),
    // );

    // Translate the functions parameters
    let mut parameters = sway::ParameterList::default();
    let mut parameter_names = 'a'..='z';

    for (_, parameter) in function_definition.params.iter() {
        let old_name = parameter
            .as_ref()
            .unwrap()
            .name
            .as_ref()
            .map(|n| n.name.clone())
            .unwrap_or(parameter_names.next().unwrap().to_string());

        let new_name = translate_naming_convention(old_name.as_str(), Case::Snake);

        let mut type_name = translate_type_name(
            project,
            module.clone(),
            scope.clone(),
            &parameter.as_ref().unwrap().ty,
            false,
            true,
        );

        // Check if the parameter's type is an ABI and make it an Identity
        if let sway::TypeName::Identifier {
            name,
            generic_parameters: None,
        } = &type_name
        {
            if project.find_contract(&name).is_some() {
                type_name = sway::TypeName::Identifier {
                    name: "Identity".into(),
                    generic_parameters: None,
                };
            }
        }

        parameters.entries.push(sway::Parameter {
            is_ref: false,
            is_mut: false,
            name: new_name,
            type_name: Some(type_name),
        });
    }

    // Create the function declaration
    let mut sway_function = sway::Function {
        attributes: if is_constant || is_pure {
            None
        } else {
            let mut attributes = vec![];

            attributes.push(sway::Attribute {
                name: "storage".into(),
                parameters: Some(if is_view {
                    vec!["read".into()]
                } else {
                    vec!["read".into(), "write".into()]
                }),
            });

            if is_payable && !is_fallback {
                attributes.push(sway::Attribute {
                    name: "payable".into(),
                    parameters: None,
                });
            }

            if is_fallback {
                attributes.push(sway::Attribute {
                    name: "fallback".into(),
                    parameters: None,
                });
            }

            Some(sway::AttributeList { attributes })
        },

        is_public: false,
        old_name: old_name.clone(),
        name: new_name.clone(),
        generic_parameters: None,

        parameters,

        return_type: if function_definition.returns.is_empty() {
            None
        } else {
            Some(if function_definition.returns.len() == 1 {
                let type_name = translate_type_name(
                    project,
                    module.clone(),
                    scope.clone(),
                    &function_definition.returns[0].1.as_ref().unwrap().ty,
                    false,
                    true,
                );
                translate_return_type_name(project, module.clone(), &type_name)
            } else {
                sway::TypeName::Tuple {
                    type_names: function_definition
                        .returns
                        .iter()
                        .map(|(_, p)| {
                            let type_name = translate_type_name(
                                project,
                                module.clone(),
                                scope.clone(),
                                &p.as_ref().unwrap().ty,
                                false,
                                true,
                            );
                            translate_return_type_name(project, module.clone(), &type_name)
                        })
                        .collect(),
                }
            })
        },

        body: None,
    };

    let mut abi_fn = None;

    if is_public {
        sway_function.name = new_name_2.clone();

        let mut abi_function = sway_function.clone();
        let mut use_string = false;

        if !is_fallback {
            for p in abi_function.parameters.entries.iter_mut() {
                if p.type_name == Some(sway::TypeName::StringSlice) {
                    use_string = true;
                    p.type_name = Some(sway::TypeName::Identifier {
                        name: "String".into(),
                        generic_parameters: None,
                    });
                }
            }

            abi_fn = Some(abi_function);
        }

        if use_string {
            module
                .borrow_mut()
                .ensure_use_declared("std::string::String");
        }

        sway_function.name.clone_from(&new_name)
    }

    // Convert the statements in the function's body (if any)
    let Some(solidity::Statement::Block { statements, .. }) = function_definition.body.as_ref()
    else {
        if contract_name.is_some() {
            sway_function.name = new_name_2;
        }

        return Ok((sway_function, None, None));
    };

    // Add the function parameters to the scope
    let mut parameters = vec![];

    for (_, p) in function_definition.params.iter() {
        let old_name = p
            .as_ref()
            .unwrap()
            .name
            .as_ref()
            .map_or("_".into(), |n| n.name.clone());
        let new_name = translate_naming_convention(old_name.as_str(), Case::Snake);
        let mut type_name = translate_type_name(
            project,
            module.clone(),
            scope.clone(),
            &p.as_ref().unwrap().ty,
            false,
            true,
        );
        let mut abi_type_name = None;

        // Check if the parameter's type is an ABI
        if let sway::TypeName::Identifier {
            name,
            generic_parameters: None,
        } = &type_name
        {
            if let Some(_external_definition) = project.find_contract(&name) {
                // TODO:
                // for entry in external_definition.uses.iter() {
                //     if !module.uses.contains(entry) {
                //         module.uses.push(entry.clone());
                //     }
                // }

                abi_type_name = Some(type_name.clone());

                type_name = sway::TypeName::Identifier {
                    name: "Identity".into(),
                    generic_parameters: None,
                };
            }
        }

        let translated_variable = ir::Variable {
            old_name,
            new_name,
            type_name,
            abi_type_name,
            ..Default::default()
        };

        parameters.push(translated_variable.clone());

        scope
            .borrow_mut()
            .add_variable(Rc::new(RefCell::new(translated_variable)));
    }

    // Add the function's named return parameters to the scope
    let mut return_parameters = vec![];

    for (_, return_parameter) in function_definition.returns.iter() {
        let Some(return_parameter) = return_parameter else {
            continue;
        };

        let Some(old_name) = return_parameter.name.as_ref().map(|n| n.name.clone()) else {
            continue;
        };

        let new_name = translate_naming_convention(old_name.as_str(), Case::Snake);

        let mut type_name = translate_type_name(
            project,
            module.clone(),
            scope.clone(),
            &return_parameter.ty,
            false,
            true,
        );

        let mut abi_type_name = None;

        // Check if the parameter's type is an ABI
        if let sway::TypeName::Identifier {
            name,
            generic_parameters: None,
        } = &type_name
        {
            if project.find_contract(&name).is_some() {
                abi_type_name = Some(type_name.clone());

                type_name = sway::TypeName::Identifier {
                    name: "Identity".into(),
                    generic_parameters: None,
                };
            }
        }

        let translated_variable = ir::Variable {
            old_name,
            new_name,
            type_name,
            abi_type_name,
            ..Default::default()
        };

        return_parameters.push(translated_variable.clone());

        scope
            .borrow_mut()
            .add_variable(Rc::new(RefCell::new(translated_variable)));
    }

    // Translate the body for the toplevel function
    let mut function_body = translate_block(
        project,
        module.clone(),
        scope.clone(),
        statements.as_slice(),
    )?;

    if is_constructor {
        let prefix = translate_naming_convention(module.borrow().name.as_str(), Case::Snake);
        let constructor_called_variable_name = format!("{prefix}_constructor_called");

        let storage_namespace = module
            .borrow_mut()
            .get_storage_namespace(scope.clone())
            .unwrap();

        let has_field = storage_namespace
            .borrow()
            .fields
            .iter()
            .any(|s| s.name == constructor_called_variable_name);

        if !has_field {
            // Add the `constructor_called` field to the storage block
            module
                .borrow_mut()
                .get_storage_namespace(scope.clone())
                .unwrap()
                .borrow_mut()
                .fields
                .push(sway::StorageField {
                    old_name: String::new(),
                    name: constructor_called_variable_name.clone(),
                    type_name: sway::TypeName::Identifier {
                        name: "bool".into(),
                        generic_parameters: None,
                    },
                    abi_type_name: None,
                    value: sway::Expression::from(sway::Literal::Bool(false)),
                });

            // Add the `constructor_called` requirement to the beginning of the function
            // require(!storage.initialized.read(), "The Contract constructor has already been called");
            function_body.statements.insert(
                0,
                sway::Statement::from(sway::Expression::create_function_calls(
                    None,
                    &[(
                        "require",
                        Some((
                            None,
                            vec![
                                sway::Expression::from(sway::UnaryExpression {
                                    operator: "!".into(),
                                    expression: sway::Expression::create_function_calls(
                                        None,
                                        &[
                                            (
                                                format!(
                                                    "storage::{}",
                                                    storage_namespace.borrow().name
                                                )
                                                .as_str(),
                                                None,
                                            ),
                                            (constructor_called_variable_name.as_str(), None),
                                            ("read", Some((None, vec![]))),
                                        ],
                                    ),
                                }),
                                sway::Expression::from(sway::Literal::String(format!(
                                    "The {} constructor has already been called",
                                    module.borrow().name
                                ))),
                            ],
                        )),
                    )],
                )),
            );

            // Set the `constructor_called` storage field to `true` at the end of the function
            // storage.initialized.write(true);
            function_body.statements.push(sway::Statement::from(
                sway::Expression::create_function_calls(
                    None,
                    &[
                        (
                            format!("storage::{}", storage_namespace.borrow().name).as_str(),
                            None,
                        ),
                        (constructor_called_variable_name.as_str(), None),
                        (
                            "write",
                            Some((
                                None,
                                vec![sway::Expression::from(sway::Literal::Bool(true))],
                            )),
                        ),
                    ],
                ),
            ));
        }
    }

    // Check for parameters that were mutated and make them local variables
    for parameter in parameters.iter().rev() {
        let Some(variable) = scope
            .borrow()
            .get_variable_from_new_name(&parameter.new_name)
        else {
            panic!(
                "error: Variable not found in scope: \"{}\"",
                parameter.new_name
            );
        };

        if variable.borrow().mutation_count > 0 {
            function_body.statements.insert(
                0,
                sway::Statement::Let(sway::Let {
                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                        is_mutable: true,
                        name: parameter.new_name.clone(),
                    }),
                    type_name: Some(parameter.type_name.clone()),
                    value: sway::Expression::create_identifier(parameter.new_name.clone()),
                }),
            );
        }
    }

    // Propagate the return variable declarations
    for return_parameter in return_parameters.iter().rev() {
        let scope = Rc::new(RefCell::new(ir::Scope::new(contract_name, None)));

        function_body.statements.insert(
            0,
            sway::Statement::Let(sway::Let {
                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                    is_mutable: true,
                    name: return_parameter.new_name.clone(),
                }),
                type_name: Some(return_parameter.type_name.clone()),
                value: create_value_expression(
                    project,
                    module.clone(),
                    scope.clone(),
                    &return_parameter.type_name,
                    None,
                ),
            }),
        );
    }

    // If the function returns values but doesn't end in a return statement, propagate the return variables
    if !return_parameters.is_empty()
        && !matches!(
            function_body.statements.last(),
            Some(sway::Statement::Expression(sway::Expression::Return(_)))
        )
    {
        function_body
            .statements
            .push(sway::Statement::from(sway::Expression::Return(Some(
                Box::new(if return_parameters.len() == 1 {
                    sway::Expression::create_identifier(return_parameters[0].new_name.clone())
                } else {
                    sway::Expression::Tuple(
                        return_parameters
                            .iter()
                            .map(|p| sway::Expression::create_identifier(p.new_name.clone()))
                            .collect(),
                    )
                }),
            ))));
    }

    // Check if the final statement returns a value and change it to be the final expression of the block
    if let Some(sway::Statement::Expression(sway::Expression::Return(Some(value)))) =
        function_body.statements.last().cloned()
    {
        function_body.statements.pop();
        function_body.final_expr = Some(*value);
    }

    // Propagate constructor calls into the function's body
    if let Some(function_constructor_calls) =
        module.borrow().function_constructor_calls.get(&new_name)
    {
        for constructor_call in function_constructor_calls.iter().rev() {
            function_body.statements.insert(
                0,
                sway::Statement::from(sway::Expression::from(constructor_call.clone())),
            );
        }
    }

    // Propagate modifier pre and post functions into the function's body
    let mut modifier_pre_calls = vec![];
    let mut modifier_post_calls = vec![];

    if let Some(modifier_invocations) = module.borrow().function_modifiers.get(&new_name) {
        for modifier_invocation in modifier_invocations.iter() {
            let Some(new_name) = modifier_invocation.function.as_identifier() else {
                panic!("Malformed modifier invocation: {modifier_invocation:#?}");
            };

            let module = module.borrow();
            let Some(modifier) = module.modifiers.iter().find(|v| v.new_name == *new_name) else {
                panic!("Failed to find modifier: {new_name}");
            };

            if modifier.pre_body.is_some() && modifier.post_body.is_some() {
                modifier_pre_calls.push(sway::FunctionCall {
                    function: sway::Expression::create_identifier(format!(
                        "{}_pre",
                        modifier.new_name
                    )),
                    generic_parameters: None,
                    parameters: modifier_invocation.parameters.clone(),
                });

                modifier_post_calls.push(sway::FunctionCall {
                    function: sway::Expression::create_identifier(format!(
                        "{}_post",
                        modifier.new_name
                    )),
                    generic_parameters: None,
                    parameters: modifier_invocation.parameters.clone(),
                });
            } else if modifier.pre_body.is_some() {
                modifier_pre_calls.push(sway::FunctionCall {
                    function: sway::Expression::create_identifier(modifier.new_name.clone()),
                    generic_parameters: None,
                    parameters: modifier_invocation.parameters.clone(),
                });
            } else if modifier.post_body.is_some() {
                modifier_post_calls.push(sway::FunctionCall {
                    function: sway::Expression::create_identifier(modifier.new_name.clone()),
                    generic_parameters: None,
                    parameters: modifier_invocation.parameters.clone(),
                });
            }
        }
    }

    for modifier_pre_call in modifier_pre_calls.iter().rev() {
        function_body.statements.insert(
            0,
            sway::Statement::from(sway::Expression::from(modifier_pre_call.clone())),
        );
    }

    for modifier_post_call in modifier_post_calls.iter().rev() {
        function_body
            .statements
            .push(sway::Statement::from(sway::Expression::from(
                modifier_post_call.clone(),
            )));
    }

    // Create the body for the toplevel function
    sway_function.body = Some(function_body);

    // Add the toplevel function
    let mut toplevel_function = sway_function.clone();
    if let Some((index, _)) = toplevel_function
        .attributes
        .as_ref()
        .map(|a| {
            a.attributes
                .iter()
                .enumerate()
                .find(|(_, a)| a.name == "payable")
        })
        .flatten()
        .clone()
    {
        toplevel_function
            .attributes
            .as_mut()
            .map(|a| a.attributes.remove(index))
            .unwrap();
    }

    let mut impl_item = None;

    if is_public && !is_fallback {
        let mut statements = vec![];

        for p in sway_function.parameters.entries.iter_mut() {
            if p.type_name == Some(sway::TypeName::StringSlice) {
                module
                    .borrow_mut()
                    .ensure_use_declared("std::string::String");
                p.type_name = Some(sway::TypeName::Identifier {
                    name: "String".into(),
                    generic_parameters: None,
                });
                statements.push(sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                        is_mutable: true,
                        name: p.name.clone(),
                    }),
                    type_name: None,
                    value: sway::Expression::create_function_calls(
                        None,
                        &[(p.name.as_str(), None), ("as_str", Some((None, vec![])))],
                    ),
                }))
            }
        }

        // Create the body for the contract impl's function wrapper
        sway_function.body = Some(sway::Block {
            statements,
            final_expr: Some(sway::Expression::create_function_calls(
                None,
                &[(
                    format!("::{}", sway_function.name).as_str(),
                    Some((
                        None,
                        sway_function
                            .parameters
                            .entries
                            .iter()
                            .map(|p| sway::Expression::create_identifier(p.name.clone()))
                            .collect(),
                    )),
                )],
            )),
        });

        sway_function.name = new_name_2.clone();

        // Create the function wrapper item for the contract impl block
        impl_item = Some(sway::ImplItem::Function(sway_function.clone()));
    }

    if abi_fn.is_none() && toplevel_function.body.is_none() {
        toplevel_function.name = new_name_2;
    }

    Ok((toplevel_function, abi_fn, impl_item))
}

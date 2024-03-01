use super::{
    create_value_expression, finalize_block_translation, translate_block, translate_expression,
    translate_return_type_name, translate_statement, translate_storage_name, translate_type_name,
    TranslatedDefinition, TranslatedFunction, TranslatedModifier, TranslatedVariable,
    TranslationScope,
};
use crate::{project::Project, sway, Error};
use convert_case::Case;
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_function_name(
    _project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    function_definition: &solidity::FunctionDefinition,
) -> String {
    // Generate the function signature
    let mut signature = function_definition.name.as_ref().map(|i| i.name.clone()).unwrap_or_default();
    
    signature.push('(');
    
    for (i, (_, parameter)) in function_definition.params.iter().enumerate() {
        signature = format!(
            "{signature}{}{}",
            if i > 0 { "," } else { "" },
            parameter.as_ref().unwrap().ty,
        );
    }

    signature.push(')');

    // Add the translated function name to the function names mapping if we haven't already
    if !translated_definition.function_names.contains_key(&signature) {
        let old_name = function_definition.name.as_ref().map(|i| i.name.clone()).unwrap_or_default();
        let mut new_name = crate::translate_naming_convention(old_name.as_str(), Case::Snake);

        // Prepend the definition name to the beginning of the function name if it is a library
        if let solidity::ContractTy::Library(_)  = translated_definition.kind.as_ref().unwrap() {
            let definition_name = crate::translate_naming_convention(translated_definition.name.as_str(), Case::Snake);
            new_name = format!("{}_{}", definition_name, new_name);
        }

        // Increase the function name count
        let count = translated_definition.function_name_counts.entry(new_name.clone()).or_insert(0);
        *count += 1;

        // Append the function name count to the end of the function name if there is more than 1
        if *count > 1 {
            new_name = format!("{new_name}_{}", *count);
        }

        translated_definition.function_names.insert(signature.clone(), new_name);
    }

    translated_definition.function_names.get(&signature).unwrap().clone()
}

#[inline]
pub fn translate_function_declaration(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    function_definition: &solidity::FunctionDefinition,
) -> Result<TranslatedFunction, Error> {
    // Collect information about the function from its type
    let is_constructor = matches!(function_definition.ty, solidity::FunctionTy::Constructor);
    let is_fallback = matches!(function_definition.ty, solidity::FunctionTy::Fallback);
    let is_receive = matches!(function_definition.ty, solidity::FunctionTy::Receive);

    let (old_name, mut new_name) = if is_constructor {
        (String::new(), "constructor".to_string())
    } else if is_fallback {
        (String::new(), "fallback".to_string())
    } else if is_receive {
        (String::new(), "receive".to_string())
    } else {
        let old_name = function_definition.name.as_ref().unwrap().name.clone();
        let new_name = translate_function_name(project, translated_definition, function_definition);
        (old_name, new_name)
    };

    if let Some(solidity::ContractTy::Abstract(_) | solidity::ContractTy::Library(_)) = &translated_definition.kind {
        new_name = format!("{}_{}", crate::translate_naming_convention(&translated_definition.name, Case::Snake), new_name);
    }

    // Create a scope for modifier invocation translations
    let scope = Rc::new(RefCell::new(TranslationScope {
        parent: Some(translated_definition.toplevel_scope.clone()),
        ..Default::default()
    }));

    // Add the function parameters to the scope
    for (_, p) in function_definition.params.iter() {
        let Some(p) = p else { continue };
        let Some(parameter_identifier) = p.name.as_ref() else { continue };
        
        let old_name = parameter_identifier.name.clone();
        let new_name = crate::translate_naming_convention(old_name.as_str(), Case::Snake);
        let type_name = translate_type_name(project, translated_definition, &p.ty, false);

        scope.borrow_mut().variables.push(Rc::new(RefCell::new(TranslatedVariable {
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
        let solidity::FunctionAttribute::BaseOrModifier(_, base) = attr else { continue };

        let old_name = base.name.identifiers.iter().map(|i| i.name.clone()).collect::<Vec<_>>().join(".");
        let new_name = crate::translate_naming_convention(old_name.as_str(), Case::Snake);

        let parameters = base.args.as_ref()
            .map(|args| args.iter().map(|a| translate_expression(project, translated_definition, scope.clone(), a)).collect::<Result<Vec<_>, _>>())
            .unwrap_or_else(|| Ok(vec![]))?;

        // Check to see if base is a constructor call
        if project.find_definition_with_abi(old_name.as_str()).is_some() {
            let prefix = crate::translate_naming_convention(old_name.as_str(), Case::Snake);
            let name = format!("{prefix}_constructor");
            
            constructor_calls.push(sway::FunctionCall {
                function: sway::Expression::Identifier(name),
                generic_parameters: None,
                parameters,
            });

            continue;
        }

        // Add the base to the modifiers list
        modifiers.push(sway::FunctionCall {
            function: sway::Expression::Identifier(new_name),
            generic_parameters: None,
            parameters,
        });
    }

    // Translate the functions parameters
    let mut parameters = sway::ParameterList::default();

    for (_, parameter) in function_definition.params.iter() {
        let old_name = parameter.as_ref().unwrap().name.as_ref().map(|n| n.name.clone()).unwrap_or("_".into());
        let new_name = crate::translate_naming_convention(old_name.as_str(), Case::Snake);
        let mut type_name = translate_type_name(project, translated_definition, &parameter.as_ref().unwrap().ty, false);

        // Check if the parameter's type is an ABI
        if let sway::TypeName::Identifier { name, generic_parameters: None } = &type_name {
            if project.find_definition_with_abi(name.as_str()).is_some() {
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

    // Translate the function
    let translated_function = TranslatedFunction {
        old_name,
        new_name,
        parameters,
        constructor_calls,
        modifiers,
        return_type: if function_definition.returns.is_empty() {
            None
        } else {
            Some(if function_definition.returns.len() == 1 {
                let type_name = translate_type_name(project, translated_definition, &function_definition.returns[0].1.as_ref().unwrap().ty, false);
                translate_return_type_name(project, translated_definition, type_name)
            } else {
                sway::TypeName::Tuple {
                    type_names: function_definition.returns.iter().map(|(_, p)| {
                        let type_name = translate_type_name(project, translated_definition, &p.as_ref().unwrap().ty, false);
                        translate_return_type_name(project, translated_definition, type_name)
                    }).collect(),
                }
            })
        },
    };

    Ok(translated_function)
}

#[inline]
pub fn translate_modifier_definition(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    function_definition: &solidity::FunctionDefinition,
) -> Result<(), Error> {
    let old_name = function_definition.name.as_ref().unwrap().name.clone();
    let new_name = crate::translate_naming_convention(old_name.as_str(), Case::Snake);

    let mut modifier = TranslatedModifier {
        old_name,
        new_name,
        parameters: sway::ParameterList::default(),
        has_underscore: false,
        pre_body: None,
        post_body: None,
    };

    let scope = Rc::new(RefCell::new(TranslationScope {
        parent: Some(translated_definition.toplevel_scope.clone()),
        ..Default::default()
    }));

    for (_, p) in function_definition.params.iter() {
        let old_name = p.as_ref().unwrap().name.as_ref().unwrap().name.clone();
        let new_name = crate::translate_naming_convention(old_name.as_str(), Case::Snake);
        let type_name = translate_type_name(project, translated_definition, &p.as_ref().unwrap().ty, false);

        modifier.parameters.entries.push(sway::Parameter {
            name: new_name.clone(),
            type_name: Some(type_name.clone()),
            ..Default::default()
        });

        scope.borrow_mut().variables.push(Rc::new(RefCell::new(TranslatedVariable {
            old_name,
            new_name,
            type_name,
            ..Default::default()
        })));
    }

    let solidity::Statement::Block { statements, .. } = function_definition.body.as_ref().unwrap() else {
        panic!("Invalid modifier body, expected block, found: {:#?}", function_definition.body);
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
        if let solidity::Statement::Expression(_, solidity::Expression::Variable(solidity::Identifier { name, .. })) = statement {
            if name == "_" {
                modifier.has_underscore = true;
                
                if let Some(block) = current_body.as_mut() {
                    for variable in current_scope.borrow_mut().variables.iter() {
                        if variable.borrow().is_storage {
                            if variable.borrow().read_count != 0 {
                                *has_storage_read = true;
                            }

                            if variable.borrow().mutation_count != 0 {
                                *has_storage_write = true;
                            }
                        }
                    }

                    finalize_block_translation(project, current_scope.clone(), block)?;
                }

                current_body = &mut modifier.post_body;
                current_scope = Rc::new(RefCell::new(scope.borrow().clone()));

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
        let sway_statement = translate_statement(project, translated_definition, current_scope.clone(), statement)?;

        // Store the index of the sway statement
        let statement_index = block.statements.len();

        // Add the sway statement to the sway block
        block.statements.push(sway_statement);

        // If the sway statement is a variable declaration, keep track of its statement index
        if let Some(sway::Statement::Let(sway_variable)) = block.statements.last() {
            let store_variable_statement_index = |id: &sway::LetIdentifier| {
                let scope = scope.borrow_mut();
                let scope_entry = scope.variables.iter().rev().find(|v| v.borrow().new_name == id.name).unwrap();
                scope_entry.borrow_mut().statement_index = Some(statement_index);
            };

            match &sway_variable.pattern {
                sway::LetPattern::Identifier(id) => store_variable_statement_index(id),
                sway::LetPattern::Tuple(ids) => ids.iter().for_each(store_variable_statement_index),
            }
        }
    }

    for variable in current_scope.borrow().variables.iter() {
        if variable.borrow().is_storage {
            if variable.borrow().read_count != 0 {
                *has_storage_read = true;
            }

            if variable.borrow().mutation_count != 0 {
                *has_storage_write = true;
            }
        }
    }

    if let Some(block) = current_body.as_mut() {
        finalize_block_translation(project, current_scope, block)?;
    }

    let create_attributes = |has_storage_read: bool, has_storage_write: bool| -> Option<sway::AttributeList> {
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
                attributes: vec![
                    sway::Attribute {
                        name: "storage".into(),
                        parameters: Some(parameters),
                    },
                ],
            })
        }
    };

    // Ensure that an underscore statement was encountered while translating the modifier
    if !modifier.has_underscore {
        panic!("Malformed modifier missing underscore statement: {}", modifier.old_name);
    }

    // Generate toplevel modifier functions
    match (modifier.pre_body.as_ref(), modifier.post_body.as_ref()) {
        (Some(pre_body), Some(post_body)) => {
            let modifier_pre_function_name = format!("{}_pre", modifier.new_name);

            translated_definition.functions.push(sway::Function {
                attributes: create_attributes(has_pre_storage_read, has_pre_storage_write),
                is_public: false,
                name: modifier_pre_function_name.clone(),
                generic_parameters: None,
                parameters: modifier.parameters.clone(),
                return_type: None,
                body: Some(pre_body.clone()),
            });

            *translated_definition.function_call_counts.entry(modifier_pre_function_name.clone()).or_insert(0) += 1;

            let modifier_post_function_name = format!("{}_post", modifier.new_name);

            translated_definition.functions.push(sway::Function {
                attributes: create_attributes(has_post_storage_read, has_post_storage_write),
                is_public: false,
                name: modifier_post_function_name.clone(),
                generic_parameters: None,
                parameters: modifier.parameters.clone(),
                return_type: None,
                body: Some(post_body.clone()),
            });

            *translated_definition.function_call_counts.entry(modifier_post_function_name.clone()).or_insert(0) += 1;
        }

        (Some(pre_body), None) => {
            translated_definition.functions.push(sway::Function {
                attributes: create_attributes(has_pre_storage_read, has_pre_storage_write),
                is_public: false,
                name: modifier.new_name.clone(),
                generic_parameters: None,
                parameters: modifier.parameters.clone(),
                return_type: None,
                body: Some(pre_body.clone()),
            });

            *translated_definition.function_call_counts.entry(modifier.new_name.clone()).or_insert(0) += 1;
        }

        (None, Some(post_body)) => {
            translated_definition.functions.push(sway::Function {
                attributes: create_attributes(has_post_storage_read, has_post_storage_write),
                is_public: false,
                name: modifier.new_name.clone(),
                generic_parameters: None,
                parameters: modifier.parameters.clone(),
                return_type: None,
                body: Some(post_body.clone()),
            });

            *translated_definition.function_call_counts.entry(modifier.new_name.clone()).or_insert(0) += 1;
        }

        (None, None) => {
            panic!("Malformed modifier missing pre and post bodies");
        }
    }
    
    // Add the translated modifier to the translated definition
    translated_definition.modifiers.push(modifier);

    Ok(())
}

#[inline]
pub fn translate_function_definition(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    function_definition: &solidity::FunctionDefinition,
) -> Result<(), Error> {
    // Collect information about the function from its type
    let is_constructor = matches!(function_definition.ty, solidity::FunctionTy::Constructor);
    let is_fallback = matches!(function_definition.ty, solidity::FunctionTy::Fallback);
    let is_receive = matches!(function_definition.ty, solidity::FunctionTy::Receive);

    // Collect information about the function from its attributes
    let mut is_public = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Visibility(solidity::Visibility::External(_) | solidity::Visibility::Public(_))));
    let is_constant = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Mutability(solidity::Mutability::Constant(_))));
    let is_pure = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Mutability(solidity::Mutability::Pure(_))));
    let is_view = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Mutability(solidity::Mutability::View(_))));
    let is_payable = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Mutability(solidity::Mutability::Payable(_))));
    let _is_virtual = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Virtual(_)));
    let is_override = function_definition.attributes.iter().any(|x| matches!(x, solidity::FunctionAttribute::Override(_, _)));

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
        translate_function_name(project, translated_definition, function_definition)
    };
    
    let mut new_name = new_name_2.clone();

    if let Some(solidity::ContractTy::Abstract(_) | solidity::ContractTy::Library(_)) = &translated_definition.kind {
       new_name = format!("{}_{}", crate::translate_naming_convention(&translated_definition.name, Case::Snake), new_name);
    }

    // Translate the functions parameters
    let mut parameters = sway::ParameterList::default();

    for (_, parameter) in function_definition.params.iter() {
        let old_name = parameter.as_ref().unwrap().name.as_ref().map(|n| n.name.clone()).unwrap_or("_".into());
        let new_name = crate::translate_naming_convention(old_name.as_str(), Case::Snake);
        let mut type_name = translate_type_name(project, translated_definition, &parameter.as_ref().unwrap().ty, false);

        // Check if the parameter's type is an ABI and make it an Identity
        if let sway::TypeName::Identifier { name, generic_parameters: None } = &type_name {
            if project.find_definition_with_abi(name.as_str()).is_some() {
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
                parameters: Some(
                    if is_view {
                        vec!["read".into()]
                    } else {
                        vec!["read".into(), "write".into()]
                    }
                ),
            });

            if is_payable {
                attributes.push(sway::Attribute {
                    name: "payable".into(),
                    parameters: None,
                });
            }

            Some(sway::AttributeList { attributes })
        },

        is_public: false,
        name: new_name.clone(),
        generic_parameters: None,

        parameters,

        return_type: if function_definition.returns.is_empty() {
            None
        } else {
            Some(if function_definition.returns.len() == 1 {
                let type_name = translate_type_name(project, translated_definition, &function_definition.returns[0].1.as_ref().unwrap().ty, false);
                translate_return_type_name(project, translated_definition, type_name)
            } else {
                sway::TypeName::Tuple {
                    type_names: function_definition.returns.iter().map(|(_, p)| {
                        let type_name = translate_type_name(project, translated_definition, &p.as_ref().unwrap().ty, false);
                        translate_return_type_name(project, translated_definition, type_name)
                    }).collect(),
                }
            })
        },

        body: None,
    };

    if is_public {
        sway_function.name = new_name_2.clone();

        if let Some(abi) = translated_definition.abi.as_mut() {
            // Only add the function to the abi if it doesn't already exist
            if !abi.functions.contains(&sway_function) && !is_override {
                if is_constructor {
                    abi.functions.insert(0, sway_function.clone());
                } else {
                    abi.functions.push(sway_function.clone());
                }
            }
        } else {
            // Add the function to the abi
            if is_constructor {
                translated_definition.get_abi().functions.insert(0, sway_function.clone());
            } else {
                translated_definition.get_abi().functions.push(sway_function.clone());
            }
        }

        sway_function.name = new_name.clone();
    }

    // Convert the statements in the function's body (if any)
    let Some(solidity::Statement::Block { statements, .. }) = function_definition.body.as_ref() else { return Ok(()) };

    // Create the scope for the body of the toplevel function
    let scope = Rc::new(RefCell::new(TranslationScope {
        parent: Some(translated_definition.toplevel_scope.clone()),
        ..Default::default()
    }));

    // Add the function parameters to the scope
    let mut parameters = vec![];

    for (_, p) in function_definition.params.iter() {
        let old_name = p.as_ref().unwrap().name.as_ref().map(|n| n.name.clone()).unwrap_or("_".into());
        let new_name = crate::translate_naming_convention(old_name.as_str(), Case::Snake);
        let mut type_name = translate_type_name(project, translated_definition, &p.as_ref().unwrap().ty, false);
        let mut abi_type_name = None;

        // Check if the parameter's type is an ABI
        if let sway::TypeName::Identifier { name, generic_parameters: None } = &type_name {
            if project.find_definition_with_abi(name.as_str()).is_some() {
                abi_type_name = Some(type_name.clone());

                type_name = sway::TypeName::Identifier {
                    name: "Identity".into(),
                    generic_parameters: None,
                };
            }
        }

        let translated_variable = TranslatedVariable {
            old_name,
            new_name,
            type_name,
            abi_type_name,
            ..Default::default()
        };

        parameters.push(translated_variable.clone());
        scope.borrow_mut().variables.push(Rc::new(RefCell::new(translated_variable)));
    }

    // Add the function's named return parameters to the scope
    let mut return_parameters = vec![];

    for (_, return_parameter) in function_definition.returns.iter() {
        let Some(return_parameter) = return_parameter else { continue };
        let Some(old_name) = return_parameter.name.as_ref().map(|n| n.name.clone()) else { continue };
        let new_name = crate::translate_naming_convention(old_name.as_str(), Case::Snake);
        let mut type_name = translate_type_name(project, translated_definition, &return_parameter.ty, false);
        let mut abi_type_name = None;

        // Check if the parameter's type is an ABI
        if let sway::TypeName::Identifier { name, generic_parameters: None } = &type_name {
            if project.find_definition_with_abi(name.as_str()).is_some() {
                abi_type_name = Some(type_name.clone());

                type_name = sway::TypeName::Identifier {
                    name: "Identity".into(),
                    generic_parameters: None,
                };
            }
        }

        let translated_variable = TranslatedVariable {
            old_name,
            new_name,
            type_name,
            abi_type_name,
            ..Default::default()
        };

        return_parameters.push(translated_variable.clone());
        scope.borrow_mut().variables.push(Rc::new(RefCell::new(translated_variable)));
    }

    // Translate the body for the toplevel function
    let mut function_body = translate_block(project, translated_definition, scope.clone(), statements.as_slice())?;

    if is_constructor {
        let prefix = crate::translate_naming_convention(translated_definition.name.as_str(), Case::Snake);
        let constructor_called_variable_name =  translate_storage_name(project, translated_definition, format!("{prefix}_constructor_called").as_str());
        
        // Add the `constructor_called` field to the storage block
        translated_definition.get_storage().fields.push(sway::StorageField {
            name: constructor_called_variable_name.clone(),
            type_name: sway::TypeName::Identifier {
                name: "bool".into(),
                generic_parameters: None,
            },
            value: sway::Expression::from(sway::Literal::Bool(false)),
        });

        // Add the `constructor_called` requirement to the beginning of the function
        // require(!storage.initialized.read(), "The Contract constructor has already been called");
        function_body.statements.insert(0, sway::Statement::from(sway::Expression::from(sway::FunctionCall {
            function: sway::Expression::Identifier("require".into()),
            generic_parameters: None,
            parameters: vec![
                sway::Expression::from(sway::UnaryExpression {
                    operator: "!".into(),
                    expression: sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::from(sway::MemberAccess {
                            expression: sway::Expression::from(sway::MemberAccess {
                                expression: sway::Expression::Identifier("storage".into()),
                                member: constructor_called_variable_name.clone(),
                            }),
                            member: "read".into(),
                        }),
                        generic_parameters: None,
                        parameters: vec![],
                    })
                }),
                sway::Expression::from(sway::Literal::String(format!("The {} constructor has already been called", translated_definition.name))),
            ],
        })));

        // Set the `constructor_called` storage field to `true` at the end of the function
        // storage.initialized.write(true);
        function_body.statements.push(sway::Statement::from(sway::Expression::from(sway::FunctionCall {
            function: sway::Expression::from(sway::MemberAccess {
                expression: sway::Expression::from(sway::MemberAccess {
                    expression: sway::Expression::Identifier("storage".into()),
                    member: constructor_called_variable_name.clone(),
                }),
                member: "write".into(),
            }),
            generic_parameters: None,
            parameters: vec![
                sway::Expression::from(sway::Literal::Bool(true)),
            ],
        })));
    }

    // Check for parameters that were mutated and make them local variables
    for parameter in parameters.iter().rev() {
        let variable = match scope.borrow().get_variable_from_new_name(&parameter.new_name) {
            Ok(variable) => variable,
            Err(e) => panic!("{e}"),
        };

        if variable.borrow().mutation_count > 0 {
            function_body.statements.insert(0, sway::Statement::Let(sway::Let {
                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                    is_mutable: true,
                    name: parameter.new_name.clone(),
                }),
                type_name: Some(parameter.type_name.clone()),
                value: sway::Expression::Identifier(parameter.new_name.clone()),
            }));
        }
    }

    // Propagate the return variable declarations
    for return_parameter in return_parameters.iter().rev() {
        let scope = Rc::new(RefCell::new(TranslationScope {
            parent: Some(translated_definition.toplevel_scope.clone()),
            ..Default::default()
        }));

        function_body.statements.insert(0, sway::Statement::Let(sway::Let {
            pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                is_mutable: true,
                name: return_parameter.new_name.clone(),
            }),
            type_name: Some(return_parameter.type_name.clone()),
            value: create_value_expression(translated_definition, scope.clone(), &return_parameter.type_name, None),
        }));
    }

    // If the function returns values but doesn't end in a return statement, propagate the return variables
    if !return_parameters.is_empty() && !matches!(function_body.statements.last(), Some(sway::Statement::Expression(sway::Expression::Return(_)))) {
        function_body.statements.push(sway::Statement::from(sway::Expression::Return(Some(Box::new(
            if return_parameters.len() == 1 {
                sway::Expression::Identifier(
                    return_parameters[0].new_name.clone()
                )
            } else {
                sway::Expression::Tuple(
                    return_parameters.iter().map(|p| sway::Expression::Identifier(p.new_name.clone())).collect()
                )
            }
        )))));
    }

    // Check if the final statement returns a value and change it to be the final expression of the block
    if let Some(sway::Statement::Expression(sway::Expression::Return(Some(value)))) = function_body.statements.last().cloned() {
        function_body.statements.pop();
        function_body.final_expr = Some(*value);
    }

    // Get the function from the scope
    let function = match scope.borrow().find_function(|f| f.borrow().new_name == new_name) {
        Some(function) => function,
        None => panic!("ERROR: Failed to find function `{new_name}` in scope"),
    };

    let function = function.borrow();

    // Propagate constructor calls into the function's body
    for constructor_call in function.constructor_calls.iter().rev() {
        function_body.statements.insert(0, sway::Statement::from(sway::Expression::from(constructor_call.clone())));
    }

    // Propagate modifier pre and post functions into the function's body
    let mut modifier_pre_calls = vec![];
    let mut modifier_post_calls = vec![];

    for modifier_invocation in function.modifiers.iter() {
        let sway::Expression::Identifier(new_name) = &modifier_invocation.function else {
            panic!("Malformed modifier invocation: {modifier_invocation:#?}");
        };
        
        let Some(modifier) = translated_definition.modifiers.iter().find(|v| v.new_name == *new_name) else {
            panic!("Failed to find modifier: {new_name}");
        };

        if modifier.pre_body.is_some() && modifier.post_body.is_some() {
            modifier_pre_calls.push(sway::FunctionCall {
                function: sway::Expression::Identifier(format!("{}_pre", modifier.new_name)),
                generic_parameters: None,
                parameters: modifier_invocation.parameters.clone(),
            });

            modifier_post_calls.push(sway::FunctionCall {
                function: sway::Expression::Identifier(format!("{}_post", modifier.new_name)),
                generic_parameters: None,
                parameters: modifier_invocation.parameters.clone(),
            });
        } else if modifier.pre_body.is_some() {
            modifier_pre_calls.push(sway::FunctionCall {
                function: sway::Expression::Identifier(modifier.new_name.clone()),
                generic_parameters: None,
                parameters: modifier_invocation.parameters.clone(),
            });
        } else if modifier.post_body.is_some() {
            modifier_post_calls.push(sway::FunctionCall {
                function: sway::Expression::Identifier(modifier.new_name.clone()),
                generic_parameters: None,
                parameters: modifier_invocation.parameters.clone(),
            });
        }
    }

    for modifier_pre_call in modifier_pre_calls.iter().rev() {
        function_body.statements.insert(0, sway::Statement::from(sway::Expression::from(modifier_pre_call.clone())));
    }

    for modifier_post_call in modifier_post_calls.iter().rev() {
        function_body.statements.push(sway::Statement::from(sway::Expression::from(modifier_post_call.clone())));
    }

    // Create the body for the toplevel function
    sway_function.body = Some(function_body);

    // Add the toplevel function
    translated_definition.functions.push(sway_function.clone());

    if is_public {
        // Create the body for the contract impl's function wrapper
        sway_function.body = Some(sway::Block {
            statements: vec![],
            final_expr: Some(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::Identifier(format!("::{}", sway_function.name)),
                generic_parameters: None,
                parameters: sway_function.parameters.entries.iter().map(|p| sway::Expression::Identifier(p.name.clone())).collect(),
            })),
        });
        
        sway_function.name = new_name_2;
        
        // Create the function wrapper item for the contract impl block
        let impl_item = sway::ImplItem::Function(sway_function.clone());
        
        if let Some(contract_impl) = translated_definition.find_contract_impl_mut() {
            if let Some(sway::ImplItem::Function(f)) = contract_impl.items.iter_mut().find(|item| {
                let sway::ImplItem::Function(f) = item else { return false };
                if f.name != sway_function.name {
                    return false;
                }
                if !sway_function.parameters.entries.iter().zip(f.parameters.entries.iter()).all(|(a, b)| a == b) {
                    return false;
                }
                sway_function.return_type == f.return_type
            }) {
                f.body = sway_function.body;
            }
            // Only add the function wrapper to the contract impl if it doesn't already exist
            else if !contract_impl.items.contains(&impl_item) {
                if is_constructor {
                    contract_impl.items.insert(0, impl_item);
                } else {
                    contract_impl.items.push(impl_item);
                }
            }
        } else {
            // Add the function wrapper to the contract impl
            if is_constructor {
                translated_definition.get_contract_impl().items.insert(0, impl_item);
            } else {
                translated_definition.get_contract_impl().items.push(impl_item);
            }
        }
    }

    Ok(())
}

use super::{
    create_value_expression, translate_assembly_statement, translate_assignment_expression,
    translate_expression, translate_pre_or_post_operator_value_expression, translate_type_name,
    TranslatedDefinition, TranslatedVariable, TranslationScope,
};
use crate::{errors::Error, project::Project, sway};
use convert_case::Case;
use solang_parser::pt as solidity;

pub fn translate_block(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &mut TranslationScope,
    statements: &[solidity::Statement]
) -> Result<sway::Block, Error> {
    let mut block = sway::Block::default();

    // Translate each of the statements in the block
    for statement in statements {
        // Translate the statement
        let sway_statement = translate_statement(project, translated_definition, scope, statement)?;

        // Store the index of the sway statement
        let statement_index = block.statements.len();

        // Add the sway statement to the sway block
        block.statements.push(sway_statement);

        // If the sway statement is a variable declaration, keep track of its statement index
        if let Some(sway::Statement::Let(sway_variable)) = block.statements.last() {
            let mut store_variable_statement_index = |id: &sway::LetIdentifier| {
                if id.name == "_" {
                    return;
                }

                let scope_entry = scope.variables.iter_mut().rev().find(|v| v.new_name == id.name).unwrap();

                scope_entry.statement_index = Some(statement_index);
            };

            match &sway_variable.pattern {
                sway::LetPattern::Identifier(id) => store_variable_statement_index(id),
                sway::LetPattern::Tuple(ids) => ids.iter().for_each(store_variable_statement_index),
            }
        }
    }

    finalize_block_translation(project, scope, &mut block)?;

    Ok(block)
}

pub fn finalize_block_translation(
    _project: &mut Project,
    scope: &TranslationScope,
    block: &mut sway::Block,
) -> Result<(), Error> {
    // Check the block for variable declarations that need to be marked mutable
    for variable in scope.variables.iter() {
        // Only check variables that are declared as statements
        let Some(statement_index) = variable.statement_index else { continue };

        // If the variable has any mutations, mark it as mutable
        if variable.mutation_count > 0 {
            let let_statement = match &mut block.statements[statement_index] {
                sway::Statement::Let(let_statement) => let_statement,
                statement => panic!("Expected let statement, found: {statement:?}"),
            };

            let mark_let_identifier_mutable = |id: &mut sway::LetIdentifier| {
                if id.name == variable.new_name {
                    id.is_mutable = true;
                }
            };

            match &mut let_statement.pattern {
                sway::LetPattern::Identifier(id) => mark_let_identifier_mutable(id),
                sway::LetPattern::Tuple(ids) => ids.iter_mut().for_each(mark_let_identifier_mutable),
            }
        }
    }

    // Check block for sub-blocks that don't contain shadowing variable declarations and flatten them
    for i in (0..block.statements.len()).rev() {
        let mut statements = None;

        {
            let sway::Statement::Expression(sway::Expression::Block(sub_block)) = &block.statements[i] else { continue };
            
            let mut var_count = 0;

            for statement in sub_block.statements.iter() {
                let sway::Statement::Let(sway::Let { pattern, .. }) = statement else { continue };

                let mut check_let_identifier = |identifier: &sway::LetIdentifier| {
                    if let Some(scope) = scope.parent.as_ref() {
                        if scope.get_variable_from_new_name(&identifier.name).is_ok() {
                            var_count += 1;
                        }
                    }
                };

                match pattern {
                    sway::LetPattern::Identifier(identifier) => {
                        check_let_identifier(identifier);
                    }

                    sway::LetPattern::Tuple(identifiers) => {
                        for identifier in identifiers.iter() {
                            check_let_identifier(identifier);
                        }
                    }
                }
            }

            if var_count == 0 {
                statements = Some(sub_block.statements.clone());
            }
        }

        if let Some(statements) = statements {
            block.statements.remove(i);

            for statement in statements.into_iter().rev() {
                block.statements.insert(i, statement);
            }
        }
    }

    // If the last statement is a block, flatten it
    if let Some(sway::Statement::Expression(sway::Expression::Block(inner_block))) = block.statements.last().cloned() {
        block.statements.pop();
        block.statements.extend(inner_block.statements);
    }

    Ok(())
}

pub fn translate_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &mut TranslationScope,
    statement: &solidity::Statement
) -> Result<sway::Statement, Error> {
    match statement {
        solidity::Statement::Block { statements, .. } => translate_block_statement(project, translated_definition, scope, statements),
        solidity::Statement::Assembly { dialect, flags, block, .. } => translate_assembly_statement(project, translated_definition, scope, dialect, flags, block),
        solidity::Statement::Args(_, named_arguments) => translate_args_statement(project, translated_definition, scope, named_arguments),
        solidity::Statement::If(_, condition, then_body, else_if) => translate_if_statement(project, translated_definition, scope, condition, then_body, else_if),
        solidity::Statement::While(_, condition, body) => translate_while_statement(project, translated_definition, scope, condition, body),
        solidity::Statement::Expression(_, expression) => translate_expression_statement(project, translated_definition, scope, expression),
        solidity::Statement::VariableDefinition(_, variable_declaration, initializer) => translate_variable_definition_statement(project, translated_definition, scope, variable_declaration, initializer),
        solidity::Statement::For(_, initialization, condition, update, body) => translate_for_statement(project, translated_definition, scope, initialization, condition, update, body),
        solidity::Statement::DoWhile(_, _, _) => todo!("translate do while statement: {statement:#?}"),
        solidity::Statement::Continue(_) => Ok(sway::Statement::from(sway::Expression::Continue)),
        solidity::Statement::Break(_) => Ok(sway::Statement::from(sway::Expression::Break)),
        solidity::Statement::Return(_, expression) => translate_return_statement(project, translated_definition, scope, expression),
        solidity::Statement::Revert(_, error_type, parameters) => translate_revert_statement(project, translated_definition, scope, error_type, parameters),
        solidity::Statement::RevertNamedArgs(_, _, _) => todo!("translate revert named args statement: {statement:#?}"),
        solidity::Statement::Emit(_, expression) => translate_emit_statement(project, translated_definition, scope, expression),
        solidity::Statement::Try(_, _, _, _) => todo!("translate try statement: {statement:#?}"),
        solidity::Statement::Error(_) => panic!("Encountered a statement that was not parsed correctly"),
    }
}

#[inline]
pub fn translate_block_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &mut TranslationScope,
    statements: &[solidity::Statement],
) -> Result<sway::Statement, Error> {
    let mut inner_scope = TranslationScope {
        parent: Some(Box::new(scope.clone())),
        ..Default::default()
    };

    // Translate the block
    let translated_block = sway::Statement::from(sway::Expression::from(
        translate_block(project, translated_definition, &mut inner_scope, statements)?
    ));

    *scope = *inner_scope.parent.unwrap();

    Ok(translated_block)
}

#[inline]
pub fn translate_args_statement(
    _project: &mut Project,
    _translated_definition: &mut TranslatedDefinition,
    _scope: &mut TranslationScope,
    _named_arguments: &[solidity::NamedArgument],
) -> Result<sway::Statement, Error> {
    todo!("translate args statement")
}

#[inline]
pub fn translate_if_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &mut TranslationScope,
    condition: &solidity::Expression,
    then_body: &solidity::Statement,
    else_if: &Option<Box<solidity::Statement>>,
) -> Result<sway::Statement, Error> {
    let condition = translate_expression(project, translated_definition, scope, condition)?;
    
    let then_body = match translate_statement(project, translated_definition, scope, then_body)? {
        sway::Statement::Expression(sway::Expression::Block(block)) => *block,
        
        statement => sway::Block {
            statements: vec![statement],
            final_expr: None,
        }
    };

    let else_if = if let Some(else_if) = else_if.as_ref() {
        match translate_statement(project, translated_definition, scope, else_if.as_ref())? {
            sway::Statement::Expression(sway::Expression::If(else_if)) => Some(else_if.clone()),
            sway::Statement::Expression(sway::Expression::Block(block)) => Some(Box::new(sway::If {
                condition: None,
                then_body: *block,
                else_if: None,
            })),
            statement => Some(Box::new(sway::If {
                condition: None,
                then_body: sway::Block {
                    statements: vec![statement],
                    final_expr: None,
                },
                else_if: None,
            })),
        }
    } else {
        None
    };

    Ok(sway::Statement::from(sway::Expression::from(sway::If {
        condition: Some(condition),
        then_body,
        else_if,
    })))
}

#[inline]
pub fn translate_while_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &mut TranslationScope,
    condition: &solidity::Expression,
    body: &solidity::Statement,
) -> Result<sway::Statement, Error> {
    Ok(sway::Statement::from(sway::Expression::from(sway::While {
        condition: translate_expression(project, translated_definition, scope, condition)?,
        body: match translate_statement(project, translated_definition, scope, body)? {
            sway::Statement::Expression(sway::Expression::Block(block)) => *block,
            statement => sway::Block {
                statements: vec![statement],
                final_expr: None,
            }
        },
    })))
}

#[inline]
pub fn translate_expression_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &mut TranslationScope,
    expression: &solidity::Expression,
) -> Result<sway::Statement, Error> {
    match expression {
        // Check for an assignment expression where lhs is a list expression
        solidity::Expression::Assign(_, lhs, rhs) => {
            if let solidity::Expression::List(_, parameters) = lhs.as_ref() {
                // Collect variable translations for the scope
                let mut variables = vec![];

                for (_, p) in parameters.iter() {
                    let Some(p) = p.as_ref() else { continue };
                    let Some(name) = p.name.as_ref() else { continue };

                    variables.push(TranslatedVariable {
                        old_name: name.name.clone(),
                        new_name: crate::translate_naming_convention(name.name.as_str(), Case::Snake),
                        type_name: translate_type_name(project, translated_definition, &p.ty, false),
                        ..Default::default()
                    });
                }

                scope.variables.extend(variables);

                // Create the variable declaration statement
                return Ok(sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::Tuple(
                        parameters.iter()
                            .map(|(_, p)| sway::LetIdentifier {
                                is_mutable: false,
                                name: if let Some(p) = p.as_ref() {
                                    if let Some(name) = p.name.as_ref() {
                                        crate::translate_naming_convention(name.name.as_str(), Case::Snake)
                                    } else {
                                        "_".into()
                                    }
                                } else {
                                    "_".into()
                                },
                            })
                            .collect()
                    ),

                    type_name: Some(sway::TypeName::Tuple {
                        type_names: parameters.iter()
                            .map(|(_, p)| {
                                if let Some(p) = p.as_ref() {
                                    translate_type_name(project, translated_definition, &p.ty, false)
                                } else {
                                    sway::TypeName::Identifier {
                                        name: "_".into(),
                                        generic_parameters: None,
                                    }
                                }
                            })
                            .collect(),
                    }),
                    
                    value: translate_expression(project, translated_definition, scope, rhs.as_ref())?,
                }));
            }
        }

        // Check for standalone pre/post decrement statements
        solidity::Expression::PreDecrement(loc, x)
        | solidity::Expression::PostDecrement(loc, x) => return Ok(sway::Statement::from(
            translate_assignment_expression(project, 
                translated_definition,
                scope,
                "-=",
                x,
                &solidity::Expression::NumberLiteral(*loc, "1".into(), "".into(), None),
            )?
        )),

        // Check for standalone pre/post increment statements
        solidity::Expression::PreIncrement(loc, x)
        | solidity::Expression::PostIncrement(loc, x) => return Ok(sway::Statement::from(
            translate_assignment_expression(project, 
                translated_definition,
                scope,
                "+=",
                x,
                &solidity::Expression::NumberLiteral(*loc, "1".into(), "".into(), None),
            )?
        )),

        _ => {}
    }
    
    Ok(sway::Statement::from(
        translate_expression(project, translated_definition, scope, expression)?
    ))
}

#[inline]
pub fn translate_variable_definition_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &mut TranslationScope,
    variable_declaration: &solidity::VariableDeclaration,
    initializer: &Option<solidity::Expression>,
) -> Result<sway::Statement, Error> {
    let old_name = variable_declaration.name.as_ref().unwrap().name.clone();
    let new_name = crate::translate_naming_convention(old_name.as_str(), Case::Snake);
    let type_name = translate_type_name(project, translated_definition, &variable_declaration.ty, false);
    let mut value = None;

    if let Some(solidity::Expression::New(_, new_expression)) = initializer.as_ref() {
        let solidity::Expression::FunctionCall(_, ty, args) = new_expression.as_ref() else {
            panic!("Unexpected new expression: {} - {new_expression:#?}", new_expression);
        };

        let new_type_name = translate_type_name(project, translated_definition, ty, false);

        if type_name != new_type_name {
            panic!("Invalid new expression type name: expected `{type_name}`, found `{new_type_name}`");
        }

        match &type_name {
            sway::TypeName::Identifier { name, generic_parameters: Some(generic_parameters) } if name == "Vec" => {
                // {
                //     let mut v = Vec::with_capacity(length);
                //     let mut i = 0;
                //     while i < length {
                //         v.push(0);
                //         i += 1;
                //     }
                //     v
                // }

                if args.len() != 1 {
                    panic!("Invalid new array expression: expected 1 argument, found {}", args.len());
                }

                let element_type_name = &generic_parameters.entries.first().unwrap().type_name;
                let length = translate_expression(project, translated_definition, scope, &args[0])?;

                value = Some(sway::Expression::from(sway::Block {
                    statements: vec![
                        // let mut v = Vec::with_capacity(length);
                        sway::Statement::from(sway::Let {
                            pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                is_mutable: true,
                                name: "v".into(),
                            }),
                            type_name: Some(type_name.clone()),
                            value: sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier("Vec::with_capacity".into()),
                                generic_parameters: None,
                                parameters: vec![
                                    length.clone(),
                                ],
                            }),
                        }),

                        // let mut i = 0;
                        sway::Statement::from(sway::Let {
                            pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                is_mutable: true,
                                name: "i".into(),
                            }),
                            type_name: None,
                            value: sway::Expression::from(sway::Literal::DecInt(0)),
                        }),

                        // while i < length {
                        //     v.push(0);
                        //     i += 1;
                        // }
                        sway::Statement::from(sway::Expression::from(sway::While {
                            // i < length
                            condition: sway::Expression::from(sway::BinaryExpression {
                                operator: "<".into(),
                                lhs: sway::Expression::Identifier("i".into()),
                                rhs: length.clone(),
                            }),

                            body: sway::Block {
                                statements: vec![
                                    // v.push(0);
                                    sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::from(sway::MemberAccess {
                                            expression: sway::Expression::Identifier("v".into()),
                                            member: "push".into(),
                                        }),
                                        generic_parameters: None,
                                        parameters: vec![
                                            create_value_expression(translated_definition, scope, element_type_name, None),
                                        ],
                                    })),

                                    // i += 1;
                                    sway::Statement::from(sway::Expression::from(sway::BinaryExpression {
                                        operator: "+=".into(),
                                        lhs: sway::Expression::Identifier("i".into()),
                                        rhs: sway::Expression::from(sway::Literal::DecInt(1)),
                                    })),
                                ],
                                final_expr: None,
                            }
                        }))
                    ],

                    // v
                    final_expr: Some(sway::Expression::Identifier("v".into())),
                }));
            }

            _ => {}
        }
    }

    let statement = sway::Statement::from(sway::Let {
        pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
            is_mutable: false,
            name: new_name.clone(),
        }),

        type_name: None,

        value: if let Some(value) = value {
            value
        } else if let Some(x) = initializer.as_ref() {
            translate_pre_or_post_operator_value_expression(project, translated_definition, scope, x)?
        } else {
            create_value_expression(translated_definition, scope, &type_name, None)
        },
    });

    scope.variables.push(TranslatedVariable {
        old_name,
        new_name,
        type_name,
        ..Default::default()
    });

    Ok(statement)
}

#[inline]
pub fn translate_for_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &mut TranslationScope,
    initialization: &Option<Box<solidity::Statement>>,
    condition: &Option<Box<solidity::Expression>>,
    update: &Option<Box<solidity::Expression>>,
    body: &Option<Box<solidity::Statement>>,
) -> Result<sway::Statement, Error> {
    // {
    //     initialization;
    //     while condition {
    //         body;
    //         update;
    //     }                    
    // }

    let mut inner_scope = TranslationScope {
        parent: Some(Box::new(scope.clone())),
        ..Default::default()
    };

    let mut statements = vec![];

    if let Some(initialization) = initialization.as_ref() {
        let statement_index = statements.len();
        let mut statement = translate_statement(project, translated_definition, &mut inner_scope, initialization.as_ref())?;

        // Store the statement index of variable declaration statements in their scope entries
        if let sway::Statement::Let(sway::Let { pattern, .. }) = &mut statement {
            let mut store_let_identifier_statement_index = |id: &mut sway::LetIdentifier| {
                let Ok(variable) = inner_scope.get_variable_from_new_name_mut(id.name.as_str()) else {
                    panic!("Failed to find variable in scope: \"{id}\"");
                };

                variable.statement_index = Some(statement_index);
            };

            match pattern {
                sway::LetPattern::Identifier(id) => store_let_identifier_statement_index(id),
                sway::LetPattern::Tuple(ids) => ids.iter_mut().for_each(store_let_identifier_statement_index),
            }
        }

        statements.push(statement);
    }

    let condition = if let Some(condition) = condition.as_ref() {
        translate_expression(project, translated_definition, &mut inner_scope, condition.as_ref())?
    } else {
        sway::Expression::from(sway::Literal::Bool(true))
    };

    let mut body = match body.as_ref() {
        None => sway::Block::default(),
        Some(body) => match translate_statement(project, translated_definition, &mut inner_scope, body.as_ref())? {
            sway::Statement::Expression(sway::Expression::Block(block)) => *block,
            statement => sway::Block {
                statements: vec![statement],
                final_expr: None,
            }
        }
    };

    if let Some(update) = update.as_ref() {
        body.statements.push(sway::Statement::from(
            match update.as_ref() {
                // Check for standalone pre/post decrement statements
                solidity::Expression::PreDecrement(loc, x)
                | solidity::Expression::PostDecrement(loc, x) => translate_assignment_expression(project, 
                    translated_definition,
                    &mut inner_scope,
                    "-=",
                    x,
                    &solidity::Expression::NumberLiteral(*loc, "1".into(), "".into(), None),
                )?,
    
                // Check for standalone pre/post increment statements
                solidity::Expression::PreIncrement(loc, x)
                | solidity::Expression::PostIncrement(loc, x) => translate_assignment_expression(project, 
                    translated_definition,
                    &mut inner_scope,
                    "+=",
                    x,
                    &solidity::Expression::NumberLiteral(*loc, "1".into(), "".into(), None),
                )?,
    
                _ => translate_expression(project, translated_definition, &mut inner_scope, update.as_ref())?
            }
        ));
    }

    statements.push(
        sway::Statement::from(sway::Expression::from(sway::While {
            condition,
            body,
        }))
    );

    let mut block = sway::Block {
        statements,
        final_expr: None,
    };

    finalize_block_translation(project, &inner_scope, &mut block)?;

    *scope = *inner_scope.parent.unwrap();

    Ok(sway::Statement::from(sway::Expression::from(block)))
}

#[inline]
pub fn translate_return_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &mut TranslationScope,
    expression: &Option<solidity::Expression>,
) -> Result<sway::Statement, Error> {
    Ok(sway::Statement::from(sway::Expression::Return(
        if let Some(x) = expression.as_ref() {
            Some(Box::new(
                translate_expression(project, translated_definition, scope, x)?
            ))
        } else {
            None
        }
    )))
}

#[inline]
pub fn translate_revert_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &mut TranslationScope,
    error_type: &Option<solidity::IdentifierPath>,
    parameters: &Vec<solidity::Expression>,
) -> Result<sway::Statement, Error> {
    if let Some(error_type) = error_type.as_ref() {
        let error_variant_name = error_type.identifiers.first().unwrap().name.clone();

        // Find the errors enum containing the variant
        let Some((errors_enum, _)) = translated_definition.errors_enums.iter().find(|(e, _)| e.variants.iter().any(|v| v.name == error_variant_name)) else {
            panic!("Failed to find error variant \"{error_variant_name}\"");
        };
        
        return Ok(sway::Statement::from(sway::Expression::from(sway::Block {
            statements: vec![
                // 1. log(data)
                sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::Identifier("log".into()),
                    generic_parameters: None,
                    parameters: vec![
                        if parameters.is_empty() {
                            sway::Expression::Identifier(format!(
                                "{}::{}",
                                errors_enum.name,
                                error_variant_name,
                            ))
                        } else {
                            sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier(format!(
                                    "{}::{}",
                                    errors_enum.name,
                                    error_variant_name,
                                )),
                                generic_parameters: None,
                                parameters: vec![
                                    if parameters.len() == 1 {
                                        translate_expression(project, translated_definition, scope, &parameters[0])?
                                    } else {
                                        sway::Expression::Tuple(
                                            parameters.iter()
                                                .map(|p| translate_expression(project, translated_definition, scope, p))
                                                .collect::<Result<Vec<_>, _>>()?
                                        )
                                    },
                                ]
                            })
                        },
                    ]
                })),
                // 2. revert(0)
                sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::Identifier("revert".into()),
                    generic_parameters: None,
                    parameters: vec![
                        sway::Expression::from(sway::Literal::DecInt(0)),
                    ],
                }))
            ],
            final_expr: None,
        })));
    }

    if parameters.is_empty() {
        return Ok(sway::Statement::from(sway::Expression::from(sway::FunctionCall {
            function: sway::Expression::Identifier("revert".into()),
            generic_parameters: None,
            parameters: vec![
                sway::Expression::from(sway::Literal::DecInt(0)),
            ],
        })))
    }

    if let Some(solidity::Expression::StringLiteral(reason)) = parameters.first().as_ref() {
        return Ok(sway::Statement::from(sway::Expression::from(sway::Block {
            statements: vec![
                // 1. log(reason)
                sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::Identifier("log".into()),
                    generic_parameters: None,
                    parameters: vec![
                        sway::Expression::from(sway::Literal::String(
                            reason.iter().map(|s| s.string.clone()).collect::<Vec<_>>().join("")
                        )),
                    ]
                })),
                // 2. revert(0)
                sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::Identifier("revert".into()),
                    generic_parameters: None,
                    parameters: vec![
                        sway::Expression::from(sway::Literal::DecInt(0)),
                    ],
                }))
            ],
            final_expr: None,
        })));
    }

    todo!("translate revert statement")
}

#[inline]
pub fn translate_emit_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &mut TranslationScope,
    expression: &solidity::Expression,
) -> Result<sway::Statement, Error> {
    match expression {
        solidity::Expression::FunctionCall(_, x, parameters) => match x.as_ref() {
            solidity::Expression::Variable(solidity::Identifier { name: event_variant_name, .. }) => {
                // Find the events enum containing the variant
                let Some((events_enum, _)) = translated_definition.events_enums.iter().find(|(e, _)| e.variants.iter().any(|v| v.name == *event_variant_name)) else {
                    panic!("Failed to find event variant \"{event_variant_name}\" in \"{}\": {:#?}", translated_definition.name, translated_definition.events_enums);
                };
                
                return Ok(sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::Identifier("log".into()),
                    generic_parameters: None,
                    parameters: vec![
                        if parameters.is_empty() {
                            sway::Expression::Identifier(format!(
                                "{}::{}",
                                events_enum.name,
                                event_variant_name,
                            ))
                        } else {
                            sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier(format!(
                                    "{}::{}",
                                    events_enum.name,
                                    event_variant_name,
                                )),
                                generic_parameters: None,
                                parameters: vec![
                                    if parameters.len() == 1 {
                                        translate_expression(project, translated_definition, scope, &parameters[0])?
                                    } else {
                                        sway::Expression::Tuple(
                                            parameters.iter()
                                                .map(|p| translate_expression(project, translated_definition, scope, p))
                                                .collect::<Result<Vec<_>, _>>()?
                                        )
                                    },
                                ]
                            })
                        },
                    ]
                })))
            }
            
            _ => {}
        }

        _ => {}
    }

    todo!("translate emit statement")
}

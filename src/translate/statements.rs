use super::{
    create_value_expression, translate_assembly_statement, translate_assignment_expression,
    translate_expression, translate_pre_or_post_operator_value_expression, translate_type_name,
    TranslatedDefinition, TranslatedVariable, TranslationScope,
};
use crate::{errors::Error, project::Project, sway, translate_naming_convention};
use convert_case::Case;
use num_bigint::BigUint;
use num_traits::{One, Zero};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

pub fn translate_block(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    statements: &[solidity::Statement]
) -> Result<sway::Block, Error> {
    let mut block = sway::Block::default();

    // Translate each of the statements in the block
    for statement in statements {
        // Translate the statement
        let sway_statement = translate_statement(project, translated_definition, scope.clone(), statement)?;

        // Store the index of the sway statement
        let statement_index = block.statements.len();

        // Add the sway statement to the sway block
        block.statements.push(sway_statement);

        // If the sway statement is a variable declaration, keep track of its statement index
        if let Some(sway::Statement::Let(sway_variable)) = block.statements.last() {
            let store_variable_statement_index = |id: &sway::LetIdentifier| {
                if id.name == "_" {
                    return;
                }

                let scope = scope.borrow();

                let scope_entry = scope.variables.iter().rev().find(|v| v.borrow().new_name == id.name).unwrap();
                let mut scope_entry = scope_entry.borrow_mut();

                scope_entry.statement_index = Some(statement_index);
            };

            match &sway_variable.pattern {
                sway::LetPattern::Identifier(id) => store_variable_statement_index(id),
                sway::LetPattern::Tuple(ids) => ids.iter().for_each(store_variable_statement_index),
            }
        }
    }

    finalize_block_translation(project, scope.clone(), &mut block)?;

    Ok(block)
}

pub fn finalize_block_translation(
    _project: &mut Project,
    scope: Rc<RefCell<TranslationScope>>,
    block: &mut sway::Block,
) -> Result<(), Error> {
    // Check the block for variable declarations that need to be marked mutable
    for variable in scope.borrow().variables.iter() {
        // Only check variables that are declared as statements
        let Some(statement_index) = variable.borrow().statement_index else { continue };

        // If the variable has any mutations, mark it as mutable
        if variable.borrow().mutation_count > 0 {
            let let_statement = match &mut block.statements[statement_index] {
                sway::Statement::Let(let_statement) => let_statement,
                statement => panic!("Expected let statement, found: {} - {statement:?}", sway::TabbedDisplayer(statement)),
            };

            let mark_let_identifier_mutable = |id: &mut sway::LetIdentifier| {
                if id.name == variable.borrow().new_name {
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
                    if let Some(scope) = scope.borrow().parent.as_ref() {
                        if scope.borrow().get_variable_from_new_name(&identifier.name).is_some() {
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
    scope: Rc<RefCell<TranslationScope>>,
    statement: &solidity::Statement
) -> Result<sway::Statement, Error> {
    match statement {
        solidity::Statement::Block { statements, .. } => translate_block_statement(project, translated_definition, scope.clone(), statements),
        solidity::Statement::Assembly { dialect, flags, block, .. } => translate_assembly_statement(project, translated_definition, scope.clone(), dialect, flags, block),
        solidity::Statement::Args(_, named_arguments) => translate_args_statement(project, translated_definition, scope.clone(), named_arguments),
        solidity::Statement::If(_, condition, then_body, else_if) => translate_if_statement(project, translated_definition, scope.clone(), condition, then_body, else_if),
        solidity::Statement::While(_, condition, body) => translate_while_statement(project, translated_definition, scope.clone(), condition, body),
        solidity::Statement::Expression(_, expression) => translate_expression_statement(project, translated_definition, scope.clone(), expression),
        solidity::Statement::VariableDefinition(_, variable_declaration, initializer) => translate_variable_definition_statement(project, translated_definition, scope.clone(), variable_declaration, initializer),
        solidity::Statement::For(_, initialization, condition, update, body) => translate_for_statement(project, translated_definition, scope.clone(), initialization, condition, update, body),
        solidity::Statement::DoWhile(_, body, condition) => translate_do_while_statement(project, translated_definition, scope.clone(), body, condition),
        solidity::Statement::Continue(_) => Ok(sway::Statement::from(sway::Expression::Continue)),
        solidity::Statement::Break(_) => Ok(sway::Statement::from(sway::Expression::Break)),
        solidity::Statement::Return(_, expression) => translate_return_statement(project, translated_definition, scope.clone(), expression),
        solidity::Statement::Revert(_, error_type, parameters) => translate_revert_statement(project, translated_definition, scope.clone(), error_type, parameters),
        
        solidity::Statement::RevertNamedArgs(_, path, named_args) => {
            translate_revert_named_arguments(project, translated_definition, scope.clone(), path, named_args)
        }

        solidity::Statement::Emit(_, expression) => translate_emit_statement(project, translated_definition, scope.clone(), expression),
        solidity::Statement::Try(_, expr, params_and_body, catch_clauses) => translate_try_catch_statement(project, translated_definition, scope, expr, params_and_body, catch_clauses),
        solidity::Statement::Error(_) => panic!("Encountered a statement that was not parsed correctly"),
    }
}
#[allow(clippy::type_complexity)]
pub fn translate_try_catch_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    expr: &solidity::Expression,
    params_and_body:  &Option<(Vec<(solidity::Loc, Option<solidity::Parameter>)>, Box<solidity::Statement>)>,
    catch_clauses: &[solidity::CatchClause],
) -> Result<sway::Statement, Error> {    
    let mut statements = vec![];
    match params_and_body.as_ref() {
        Some((params, body)) =>  {
            if !params.is_empty() {
                let let_statement = sway::Let { 
                    pattern: if params.len() == 1 {
                        sway::LetPattern::Identifier(sway::LetIdentifier { 
                            is_mutable: false, 
                            name: translate_naming_convention(params[0].1.as_ref().unwrap().name.as_ref().unwrap().name.as_str(), Case::Snake),
                        })
                    } else {
                        sway::LetPattern::Tuple(params.iter().map(|(_, p)| sway::LetIdentifier { 
                            is_mutable: false, 
                            name: translate_naming_convention(p.as_ref().unwrap().name.as_ref().unwrap().name.as_str(), Case::Snake),
                        }).collect())
                    },
                    type_name: None, 
                    value: translate_expression(project, translated_definition, scope.clone(), expr)? 
                };
                let store_let_identifier = |id: &sway::LetIdentifier, type_name: &sway::TypeName| {
                    
                    let variable = Rc::new(RefCell::new(TranslatedVariable {
                        old_name: id.name.clone(),
                        new_name: id.name.clone(),
                        type_name: type_name.clone(),
                        ..Default::default()
                    }));
                    scope.borrow_mut().variables.push(variable);
                    
                    let variable = scope.borrow().get_variable_from_new_name(&id.name).unwrap();
                    variable.borrow_mut().statement_index = Some(statements.len());
                };
                match &let_statement.pattern {
                    sway::LetPattern::Identifier(id) => {
                        let type_name = translate_type_name(project, translated_definition, &params[0].1.as_ref().unwrap().ty, false, false);
                        store_let_identifier(id, &type_name);
                    },
                    sway::LetPattern::Tuple(ids) => {
                        let type_names = params.iter().map(|(_, p)| translate_type_name(project, translated_definition, &p.as_ref().unwrap().ty, false, false)).collect::<Vec<_>>();
                        ids.iter().zip(type_names.iter()).for_each(|(id, type_name)| store_let_identifier(id, type_name));
                    }
                }
                statements.push(sway::Statement::from(let_statement));
            }
            
            match translate_statement(project, translated_definition, scope.clone(), body)? {
                sway::Statement::Expression(sway::Expression::Block(block)) => {
                    if block.statements.len() == 1 {
                        statements.extend(block.statements.clone()) 
                    } else {
                        statements.push(sway::Statement::from(sway::Expression::from(block.as_ref().clone())));
                    }
                }
                stmt => statements.push(stmt), 
            };
            
        },
        None => todo!(),
    }
    
    for cc in catch_clauses {
        statements.push(sway::Statement::Commented(format!("unsupported: {cc}"), None));
    }
    
    Ok(sway::Statement::from(sway::Expression::from(sway::Block { 
        statements, 
        final_expr: None 
    })))
}

#[inline]
pub fn translate_block_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    statements: &[solidity::Statement],
) -> Result<sway::Statement, Error> {
    let scope = Rc::new(RefCell::new(TranslationScope {
        parent: Some(scope.clone()),
        ..Default::default()
    }));

    // Translate the block
    let translated_block = sway::Statement::from(sway::Expression::from(
        translate_block(project, translated_definition, scope.clone(), statements)?
    ));

    Ok(translated_block)
}

#[inline]
pub fn translate_args_statement(
    _project: &mut Project,
    _translated_definition: &mut TranslatedDefinition,
    _scope: Rc<RefCell<TranslationScope>>,
    _named_arguments: &[solidity::NamedArgument],
) -> Result<sway::Statement, Error> {
    todo!("translate args statement")
}

#[inline]
pub fn translate_if_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    condition: &solidity::Expression,
    then_body: &solidity::Statement,
    else_if: &Option<Box<solidity::Statement>>,
) -> Result<sway::Statement, Error> {
    let condition = translate_expression(project, translated_definition, scope.clone(), condition)?;
    
    let then_body = match translate_statement(project, translated_definition, scope.clone(), then_body)? {
        sway::Statement::Expression(sway::Expression::Block(block)) => *block,
        
        statement => sway::Block {
            statements: vec![statement],
            final_expr: None,
        }
    };

    let else_if = if let Some(else_if) = else_if.as_ref() {
        match translate_statement(project, translated_definition, scope.clone(), else_if.as_ref())? {
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
    scope: Rc<RefCell<TranslationScope>>,
    condition: &solidity::Expression,
    body: &solidity::Statement,
) -> Result<sway::Statement, Error> {
    Ok(sway::Statement::from(sway::Expression::from(sway::While {
        condition: translate_expression(project, translated_definition, scope.clone(), condition)?,
        body: match translate_statement(project, translated_definition, scope.clone(), body)? {
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
    scope: Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
) -> Result<sway::Statement, Error> {
    match expression {
        // Check for an assignment expression where lhs is a list expression
        solidity::Expression::Assign(_, lhs, rhs) => {
            if let solidity::Expression::List(_, parameters) = lhs.as_ref() {
                // Check for a pure assignment without new variable declarations
                if parameters.iter().all(|(_, p)| p.as_ref().map(|p| p.name.is_none()).unwrap_or(true)) {
                    return Ok(sway::Statement::from(sway::Expression::from(sway::BinaryExpression {
                        operator: "=".into(),
                        lhs: sway::Expression::Tuple(
                            parameters.iter()
                                .map(|(_, p)| translate_expression(project, translated_definition, scope.clone(), &p.as_ref().unwrap().ty))
                                .collect::<Result<Vec<_>, _>>()?
                        ),
                        rhs: translate_expression(project, translated_definition, scope.clone(), rhs)?,
                    })));
                }

                // Collect variable translations for the scope
                let mut variables = vec![];

                for (_, p) in parameters.iter() {
                    let Some(p) = p.as_ref() else { continue };
                    let Some(name) = p.name.as_ref() else { continue };

                    variables.push(Rc::new(RefCell::new(TranslatedVariable {
                        old_name: name.name.clone(),
                        new_name: crate::translate_naming_convention(name.name.as_str(), Case::Snake),
                        type_name: translate_type_name(project, translated_definition, &p.ty, false, false),
                        ..Default::default()
                    })));
                }

                scope.borrow_mut().variables.extend(variables);

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
                                    translate_type_name(project, translated_definition, &p.ty, false, false)
                                } else {
                                    sway::TypeName::Identifier {
                                        name: "_".into(),
                                        generic_parameters: None,
                                    }
                                }
                            })
                            .collect(),
                    }),
                    
                    value: translate_expression(project, translated_definition, scope.clone(), rhs.as_ref())?,
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
        translate_expression(project, translated_definition, scope.clone(), expression)?
    ))
}

#[inline]
pub fn translate_variable_definition_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    variable_declaration: &solidity::VariableDeclaration,
    initializer: &Option<solidity::Expression>,
) -> Result<sway::Statement, Error> {
    let old_name = variable_declaration.name.as_ref().unwrap().name.clone();
    let new_name = crate::translate_naming_convention(old_name.as_str(), Case::Snake);
    let mut type_name = translate_type_name(project, translated_definition, &variable_declaration.ty, false, false);
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

    let mut value = None;

    if let Some(solidity::Expression::New(_, new_expression)) = initializer.as_ref() {
        let solidity::Expression::FunctionCall(_, ty, args) = new_expression.as_ref() else {
            panic!("Unexpected new expression: {} - {new_expression:#?}", new_expression);
        };

        let new_type_name = translate_type_name(project, translated_definition, ty, false, false);

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
                let length = translate_expression(project, translated_definition, scope.clone(), &args[0])?;

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
                            value: sway::Expression::from(sway::Literal::DecInt(BigUint::zero())),
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
                                            create_value_expression(translated_definition, scope.clone(), element_type_name, None),
                                        ],
                                    })),

                                    // i += 1;
                                    sway::Statement::from(sway::Expression::from(sway::BinaryExpression {
                                        operator: "+=".into(),
                                        lhs: sway::Expression::Identifier("i".into()),
                                        rhs: sway::Expression::from(sway::Literal::DecInt(BigUint::one())),
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
            translate_pre_or_post_operator_value_expression(project, translated_definition, scope.clone(), x)?
        } else {
            create_value_expression(translated_definition, scope.clone(), &type_name, None)
        },
    });

    scope.borrow_mut().variables.push(Rc::new(RefCell::new(TranslatedVariable {
        old_name,
        new_name,
        type_name,
        abi_type_name,
        ..Default::default()
    })));

    Ok(statement)
}

#[inline]
pub fn translate_for_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
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

    // Create a scope for the block that will contain the for loop logic
    let scope = Rc::new(RefCell::new(TranslationScope {
        parent: Some(scope.clone()),
        ..Default::default()
    }));

    // Collect statements for the for loop logic block
    let mut statements = vec![];

    // Translate the initialization statement (if any) and add it to the for loop logic block's statements
    if let Some(initialization) = initialization.as_ref() {
        let statement_index = statements.len();
        let mut statement = translate_statement(project, translated_definition, scope.clone(), initialization.as_ref())?;

        // Store the statement index of variable declaration statements in their scope entries
        if let sway::Statement::Let(sway::Let { pattern, .. }) = &mut statement {
            let store_let_identifier_statement_index = |id: &mut sway::LetIdentifier| {
                let Some(variable) = scope.borrow().get_variable_from_new_name(&id.name) else {
                    panic!("error: Variable not found in scope: \"{}\"", id.name);
                };
                
                variable.borrow_mut().statement_index = Some(statement_index);
            };

            match pattern {
                sway::LetPattern::Identifier(id) => store_let_identifier_statement_index(id),
                sway::LetPattern::Tuple(ids) => ids.iter_mut().for_each(store_let_identifier_statement_index),
            }
        }

        statements.push(statement);
    }

    // Translate the condition of the for loop ahead of time (if any)
    let condition = if let Some(condition) = condition.as_ref() {
        translate_expression(project, translated_definition, scope.clone(), condition.as_ref())?
    } else {
        sway::Expression::from(sway::Literal::Bool(true))
    };

    // Translate the body of the for loop ahead of time (if any)
    let mut body = match body.as_ref() {
        None => sway::Block::default(),
        Some(body) => match translate_statement(project, translated_definition, scope.clone(), body.as_ref())? {
            sway::Statement::Expression(sway::Expression::Block(block)) => *block,
            statement => sway::Block {
                statements: vec![statement],
                final_expr: None,
            }
        }
    };

    // Translate the update statement of the for loop (if any) and add it to the end of the for loop's body block
    if let Some(update) = update.as_ref() {
        body.statements.push(sway::Statement::from(
            match update.as_ref() {
                // Check for standalone pre/post decrement statements
                solidity::Expression::PreDecrement(loc, x)
                | solidity::Expression::PostDecrement(loc, x) => translate_assignment_expression(project, 
                    translated_definition,
                    scope.clone(),
                    "-=",
                    x,
                    &solidity::Expression::NumberLiteral(*loc, "1".into(), "".into(), None),
                )?,
    
                // Check for standalone pre/post increment statements
                solidity::Expression::PreIncrement(loc, x)
                | solidity::Expression::PostIncrement(loc, x) => translate_assignment_expression(project, 
                    translated_definition,
                    scope.clone(),
                    "+=",
                    x,
                    &solidity::Expression::NumberLiteral(*loc, "1".into(), "".into(), None),
                )?,
    
                _ => translate_expression(project, translated_definition, scope.clone(), update.as_ref())?
            }
        ));
    }

    // Create the while loop for the for loop logic ahead of time
    let while_statement = sway::Statement::from(sway::Expression::from(sway::While {
        condition,
        body,
    }));

    // If we don't have any initialization statements, just return the generated while loop
    if statements.is_empty() {
        return Ok(while_statement);
    }
    
    // Add the generated while loop to the for loop logic block's statements
    statements.push(while_statement);

    // Create the for loop logic block using the collected statements
    let mut block = sway::Block {
        statements,
        final_expr: None,
    };

    // Finalize the for loop logic block
    finalize_block_translation(project, scope.clone(), &mut block)?;

    Ok(sway::Statement::from(sway::Expression::from(block)))
}

#[inline]
pub fn translate_do_while_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    body: &solidity::Statement,
    condition: &solidity::Expression,
) -> Result<sway::Statement, Error> {
    Ok(sway::Statement::from(sway::Expression::from(sway::While {
        condition: sway::Expression::from(sway::Literal::Bool(true)),
        body: {
            let mut body = match translate_statement(project, translated_definition, scope.clone(), body)? {
                sway::Statement::Expression(sway::Expression::Block(block)) => *block,
                statement => sway::Block {
                    statements: vec![statement],
                    final_expr: None,
                }
            };

            body.statements.push(sway::Statement::from(sway::Expression::from(sway::If {
                condition: Some(sway::Expression::from(sway::UnaryExpression {
                    operator: "!".into(),
                    expression: translate_expression(project, translated_definition, scope.clone(), condition)?,
                })),
                then_body: sway::Block {
                    statements: vec![
                        sway::Statement::from(sway::Expression::Break),
                    ],
                    final_expr: None,
                },
                else_if: None,
            })));

            body
        }
    })))
}

#[inline]
pub fn translate_return_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    expression: &Option<solidity::Expression>,
) -> Result<sway::Statement, Error> {
    Ok(sway::Statement::from(sway::Expression::Return(
        if let Some(x) = expression.as_ref() {
            Some(Box::new(
                translate_expression(project, translated_definition, scope.clone(), x)?
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
    scope: Rc<RefCell<TranslationScope>>,
    error_type: &Option<solidity::IdentifierPath>,
    parameters: &[solidity::Expression],
) -> Result<sway::Statement, Error> {
    if let Some(error_type) = error_type.as_ref() {
        if !(1..=2).contains(&error_type.identifiers.len()) {
            panic!("Unexpected error type: expected 1 or 2 identifiers, found {}", error_type.identifiers.len());
        }

        let mut ids_iter = error_type.identifiers.iter();

        // Find the error variant
        let (error_variant_name, errors_enum_and_impl) = if error_type.identifiers.len() == 2 {
            let external_definition_name = ids_iter.next().unwrap().name.clone();
            let error_variant_name = ids_iter.next().unwrap().name.clone();
            let external_definition = project.translated_definitions.iter_mut().find(|d| d.name == external_definition_name).unwrap();
            let errors_enum_and_impl = external_definition.errors_enums.iter().find(|(e, _)| e.variants.iter().any(|v| v.name == error_variant_name)).cloned().unwrap();
            (error_variant_name, errors_enum_and_impl)
        } else {
            let error_variant_name = ids_iter.next().unwrap().name.clone();
            let errors_enum_and_impl = translated_definition.errors_enums.iter().find(|(e, _)| e.variants.iter().any(|v| v.name == error_variant_name)).cloned().unwrap();
            (error_variant_name, errors_enum_and_impl)
        };

        // Add the error definition to the current definition if we haven't already
        if !translated_definition.errors_enums.contains(&errors_enum_and_impl) {
            translated_definition.errors_enums.push(errors_enum_and_impl.clone());
        }

        let (errors_enum, _) = errors_enum_and_impl;
        
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
                                        translate_expression(project, translated_definition, scope.clone(), &parameters[0])?
                                    } else {
                                        sway::Expression::Tuple(
                                            parameters.iter()
                                                .map(|p| translate_expression(project, translated_definition, scope.clone(), p))
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
                        sway::Expression::from(sway::Literal::DecInt(BigUint::zero())),
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
                sway::Expression::from(sway::Literal::DecInt(BigUint::zero())),
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
                        sway::Expression::from(sway::Literal::DecInt(BigUint::zero())),
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
    scope: Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
) -> Result<sway::Statement, Error> {
    
    if let solidity::Expression::FunctionCall(_, x, parameters) = expression {
        if let solidity::Expression::Variable(solidity::Identifier { name: event_variant_name, .. }) = x.as_ref() {
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
                                    translate_expression(project, translated_definition, scope.clone(), &parameters[0])?
                                } else {
                                    sway::Expression::Tuple(
                                        parameters.iter()
                                            .map(|p| translate_expression(project, translated_definition, scope.clone(), p))
                                            .collect::<Result<Vec<_>, _>>()?
                                    )
                                },
                            ]
                        })
                    },
                ]
            })))
        }
    }
            

    todo!("translate emit statement")
}

#[inline]
pub fn translate_revert_named_arguments(
    project: &mut Project, 
    translated_definition: &mut TranslatedDefinition, 
    scope: Rc<RefCell<TranslationScope>>, 
    path: &Option<solidity::IdentifierPath>, 
    named_args: &[solidity::NamedArgument]
) -> Result<sway::Statement, Error> {
    // TODO: Keep track of the paramerter names and order them correctly
    let error_identifier = path.as_ref().unwrap().identifiers.first().unwrap().name.clone();
    if translated_definition.errors_enums.iter().any(|e| 
        e.0.variants.iter().any(|v| v.name == error_identifier)
     ) {
        let error_expressions: Vec<_> = named_args.iter().map(|arg| arg.expr.clone()).collect();
        return translate_revert_statement(project, translated_definition, scope, path, &error_expressions)
    }

    todo!("translate revert named arguments : {:#?}", path)
}
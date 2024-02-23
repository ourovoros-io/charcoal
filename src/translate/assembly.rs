use super::{create_value_expression, TranslatedDefinition, TranslatedVariable, TranslationScope};
use crate::{errors::Error, project::Project, sway};
use convert_case::Case;
use solang_parser::pt as solidity;

#[inline]
pub fn translate_assembly_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &mut TranslationScope,
    _dialect: &Option<solidity::StringLiteral>,
    _flags: &Option<Vec<solidity::StringLiteral>>,
    yul_block: &solidity::YulBlock,
) -> Result<sway::Statement, Error> {
    let mut block = sway::Block::default();

    for yul_statement in yul_block.statements.iter() {
        match yul_statement {
            solidity::YulStatement::Assign(_, identifiers, value) => {
                let identifiers = identifiers.iter()
                    .map(|i| translate_yul_expression(project, translated_definition, scope, i))
                    .collect::<Result<Vec<_>, _>>()?;

                for identifier in identifiers.iter() {
                    let sway::Expression::Identifier(name) = identifier else {
                        panic!("Expected identifier, found: {identifier:#?}")
                    };

                    let variable = match scope.get_variable_from_new_name_mut(name) {
                        Ok(variable) => variable,
                        Err(e) => panic!("{e}"),
                    };

                    variable.mutation_count += 1;
                }

                let value = translate_yul_expression(project, translated_definition, scope, value)?;
                
                block.statements.push(sway::Statement::from(sway::Expression::from(sway::BinaryExpression {
                    operator: "=".into(),
                    lhs: if identifiers.len() == 1 {
                        identifiers[0].clone()
                    } else {
                        sway::Expression::Tuple(identifiers)
                    },
                    rhs: value,
                })));
            }
            
            solidity::YulStatement::VariableDeclaration(_, identifiers, value) => {
                // Collect variable translations for the scope
                let mut variables = vec![];

                for p in identifiers.iter() {
                    variables.push(TranslatedVariable {
                        old_name: p.id.name.clone(),
                        new_name: crate::translate_naming_convention(p.id.name.as_str(), Case::Snake),
                        type_name: sway::TypeName::Identifier {
                            name: "u256".into(),
                            generic_parameters: None,
                        },
                        ..Default::default()
                    });
                }

                scope.variables.extend(variables.clone());

                // Create the variable declaration statement
                block.statements.push(sway::Statement::from(sway::Let {
                    pattern: if variables.len() == 1 {
                        sway::LetPattern::Identifier(sway::LetIdentifier {
                            is_mutable: false,
                            name: variables[0].new_name.clone(),
                        })
                    } else {
                        sway::LetPattern::Tuple(
                            variables.iter()
                                .map(|p| sway::LetIdentifier {
                                    is_mutable: false,
                                    name: p.new_name.clone(),
                                })
                                .collect()
                        )
                    },

                    type_name: None,
                    
                    value: if let Some(value) = value.as_ref() {
                        translate_yul_expression(project, translated_definition, scope, value)?
                    } else {
                        create_value_expression(
                            translated_definition,
                            scope,
                            &sway::TypeName::Identifier {
                                name: "u256".into(),
                                generic_parameters: None,
                            },
                            None,
                        )
                    },
                }));
            }

            solidity::YulStatement::If(_, _, _) => todo!("yul if statement: {yul_statement:#?}"),
            solidity::YulStatement::For(_) => todo!("yul for statement: {yul_statement:#?}"),
            solidity::YulStatement::Switch(_) => todo!("yul switch statement: {yul_statement:#?}"),
            solidity::YulStatement::Leave(_) => todo!("yul leave statement: {yul_statement:#?}"),
            solidity::YulStatement::Break(_) => todo!("yul break statement: {yul_statement:#?}"),
            solidity::YulStatement::Continue(_) => todo!("yul continue statement: {yul_statement:#?}"),
            solidity::YulStatement::Block(_) => todo!("yul block statement: {yul_statement:#?}"),
            solidity::YulStatement::FunctionDefinition(_) => todo!("yul function definition statement: {yul_statement:#?}"),
            
            solidity::YulStatement::FunctionCall(_) => {
                block.statements.push(sway::Statement::from(sway::Expression::create_todo(Some(yul_statement.to_string()))));
            }

            solidity::YulStatement::Error(_) => todo!("yul error statement: {yul_statement:#?}"),
        }
    }
    
    Ok(sway::Statement::from(sway::Expression::from(block)))
}

pub fn translate_yul_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &mut TranslationScope,
    expression: &solidity::YulExpression,
) -> Result<sway::Expression, Error> {
    match expression {
        solidity::YulExpression::BoolLiteral(_, value, _) => Ok(sway::Expression::from(sway::Literal::Bool(*value))),
        solidity::YulExpression::NumberLiteral(_, value, _, _) => Ok(sway::Expression::from(sway::Literal::DecInt(value.parse().unwrap()))),
        solidity::YulExpression::HexNumberLiteral(_, value, _) => Ok(sway::Expression::from(sway::Literal::HexInt(u64::from_str_radix(value.trim_start_matches("0x"), 16).unwrap()))),
        solidity::YulExpression::HexStringLiteral(_, _) => todo!("yul hex string literal expression: {expression:#?}"),
        solidity::YulExpression::StringLiteral(string_literal, _) => Ok(sway::Expression::from(sway::Literal::String(string_literal.string.clone()))),
        
        solidity::YulExpression::Variable(solidity::Identifier { name, .. }) => {
            let variable = match scope.get_variable_from_old_name(name.as_str()) {
                Ok(variable) => variable,
                Err(e) => panic!("{e}"),
            };

            if variable.is_storage {
                Ok(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression: sway::Expression::from(sway::MemberAccess {
                            expression: sway::Expression::Identifier("storage".into()),
                            member: variable.new_name.clone(),
                        }),
                        member: "read".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![],
                }))
            } else {
                Ok(sway::Expression::Identifier(variable.new_name.clone()))
            }
        }

        solidity::YulExpression::FunctionCall(function_call) => {
            let parameters = function_call.arguments.iter()
                .map(|a| translate_yul_expression(project, translated_definition, scope, a))
                .collect::<Result<Vec<_>, _>>()?;

            match function_call.id.name.as_str() {
                "add" => {
                    if parameters.len() != 2 {
                        panic!("Invalid yul add function call, expected 2 parameters, found {}", parameters.len());
                    }

                    Ok(sway::Expression::from(sway::BinaryExpression {
                        operator: "+".into(),
                        lhs: parameters[0].clone(),
                        rhs: parameters[1].clone(),
                    }))
                }

                "sub" => {
                    if parameters.len() != 2 {
                        panic!("Invalid yul sub function call, expected 2 parameters, found {}", parameters.len());
                    }

                    Ok(sway::Expression::from(sway::BinaryExpression {
                        operator: "-".into(),
                        lhs: parameters[0].clone(),
                        rhs: parameters[1].clone(),
                    }))
                }

                "mul" => {
                    if parameters.len() != 2 {
                        panic!("Invalid yul mul function call, expected 2 parameters, found {}", parameters.len());
                    }

                    Ok(sway::Expression::from(sway::BinaryExpression {
                        operator: "*".into(),
                        lhs: parameters[0].clone(),
                        rhs: parameters[1].clone(),
                    }))
                }

                "div" => {
                    if parameters.len() != 2 {
                        panic!("Invalid yul div function call, expected 2 parameters, found {}", parameters.len());
                    }

                    Ok(sway::Expression::from(sway::BinaryExpression {
                        operator: "/".into(),
                        lhs: parameters[0].clone(),
                        rhs: parameters[1].clone(),
                    }))
                }

                "lt" => {
                    if parameters.len() != 2 {
                        panic!("Invalid yul lt function call, expected 2 parameters, found {}", parameters.len());
                    }

                    Ok(sway::Expression::from(sway::BinaryExpression {
                        operator: "<".into(),
                        lhs: parameters[0].clone(),
                        rhs: parameters[1].clone(),
                    }))
                }

                "gt" => {
                    if parameters.len() != 2 {
                        panic!("Invalid yul gt function call, expected 2 parameters, found {}", parameters.len());
                    }

                    Ok(sway::Expression::from(sway::BinaryExpression {
                        operator: ">".into(),
                        lhs: parameters[0].clone(),
                        rhs: parameters[1].clone(),
                    }))
                }

                "not" => {
                    if parameters.len() != 1 {
                        panic!("Invalid yul not function call, expected 1 parameter, found {}", parameters.len());
                    }

                    Ok(sway::Expression::from(sway::BinaryExpression {
                        operator: "!=".into(),
                        lhs: parameters[0].clone(),
                        rhs: sway::Expression::from(sway::Literal::DecInt(0)),
                    }))
                }

                "addmod" => {
                    if parameters.len() != 3 {
                        panic!("Invalid yul addmod function call, expected 3 parameters, found {}", parameters.len());
                    }

                    Ok(sway::Expression::from(sway::BinaryExpression {
                        operator: "%".into(),
                        lhs: sway::Expression::Tuple(vec![
                            sway::Expression::from(sway::BinaryExpression {
                                operator: "+".into(),
                                lhs: parameters[0].clone(),
                                rhs: parameters[1].clone(),
                            }),
                        ]),
                        rhs: parameters[3].clone(),
                    }))
                }

                "mulmod" => {
                    if parameters.len() != 3 {
                        panic!("Invalid yul addmod function call, expected 3 parameters, found {}", parameters.len());
                    }

                    Ok(sway::Expression::from(sway::BinaryExpression {
                        operator: "%".into(),
                        lhs: sway::Expression::Tuple(vec![
                            sway::Expression::from(sway::BinaryExpression {
                                operator: "*".into(),
                                lhs: parameters[0].clone(),
                                rhs: parameters[1].clone(),
                            }),
                        ]),
                        rhs: parameters[2].clone(),
                    }))
                }

                "mload" => {
                    Ok(sway::Expression::create_todo(Some(expression.to_string())))
                }

                "mstore" => {
                    Ok(sway::Expression::create_todo(Some(expression.to_string())))
                }

                "keccak256" => {
                    Ok(sway::Expression::create_todo(Some(expression.to_string())))
                }

                "byte" => {
                    Ok(sway::Expression::create_todo(Some(expression.to_string())))
                }

                "create2" => {
                    Ok(sway::Expression::create_todo(Some(expression.to_string())))
                }

                name => todo!("look up yul function in scope: \"{name}\"")
            }
        }

        solidity::YulExpression::SuffixAccess(_, _, _) => todo!("yul suffix access expression: {expression:#?}"),
    }
}

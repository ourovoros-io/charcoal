use std::{cell::RefCell, rc::Rc};
use solang_parser::pt as solidity;
use crate::{errors::Error, project::Project, sway, translate::{TranslatedDefinition, TranslatedVariable, TranslationScope}};
use super::{pre_post::translate_pre_or_post_operator_value_expression, translate_expression, variable::translate_variable_access_expression};

#[inline]
pub fn create_assignment_expression(
    _project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    operator: &str,
    expression: &sway::Expression,
    variable: Rc<RefCell<TranslatedVariable>>,
    rhs: &sway::Expression,
    rhs_type_name: &sway::TypeName,
) -> Result<sway::Expression, Error> {    
    // Generate a unique name for our variable
    let variable_name = scope.borrow_mut().generate_unique_variable_name("x");
    
    let rhs = rhs.clone();
    
    let lhs_type = translated_definition.get_expression_type(scope.clone(), &expression)?;
    let rhs_type = translated_definition.get_expression_type(scope.clone(), &rhs)?;

    variable.borrow_mut().mutation_count += 1;

    let is_storage = variable.borrow().is_storage;
    let type_name = variable.borrow().type_name.clone();

    if is_storage {
        Ok(sway::Expression::from(sway::FunctionCall {
            function: sway::Expression::from(sway::MemberAccess {
                expression: expression.clone(),
                member: match (&type_name, &rhs_type_name) {
                    (
                        sway::TypeName::Identifier { name: lhs_name, .. },
                        sway::TypeName::StringSlice
                    ) if lhs_name == "StorageString" => {
                        "write_slice".into()
                    }

                    _ => "write".into(),
                },
            }),
            generic_parameters: None,
            parameters: vec![
                match operator {
                    "=" => match (&type_name, &rhs_type_name) {
                        (
                            sway::TypeName::Identifier { name: lhs_name, .. },
                            sway::TypeName::StringSlice
                        ) if lhs_name == "StorageString" => {
                            // Ensure `std::string::*` is imported
                            translated_definition.ensure_use_declared("std::string::*");

                            sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier("String::from_ascii_str".into()),
                                generic_parameters: None,
                                parameters: vec![rhs.clone()],
                            })
                        }

                        _ => rhs.clone(),
                    },

                    _ => {
                        variable.borrow_mut().read_count += 1;

                        let operator = operator.trim_end_matches("=").to_string();
                        
                        match lhs_type {
                            sway::TypeName::Identifier { name: lhs_name, generic_parameters } => {
                                match (lhs_name.as_str(), generic_parameters.as_ref()) {
                                    ("u8" | "u16" | "u32" | "u64" | "u256", None) => {
                                        match rhs_type {
                                            sway::TypeName::Identifier { name: rhs_name, generic_parameters } => {
                                                match (rhs_name.as_str(), generic_parameters.as_ref()) {
                                                    ("u8" | "u16" | "u32" | "u64" | "u256", None) => {
                                                        let lhs_bits: usize = lhs_name.trim_start_matches("u").parse().unwrap();
                                                        let rhs_bits: usize = rhs_name.trim_start_matches("u").parse().unwrap();

                                                        if lhs_bits > rhs_bits {
                                                            sway::Expression::from(sway::BinaryExpression{ 
                                                                operator, 
                                                                lhs: sway::Expression::from(sway::FunctionCall {
                                                                    function: sway::Expression::from(sway::MemberAccess {
                                                                        expression: expression.clone(),
                                                                        member: "read".into(),
                                                                    }),
                                                                    generic_parameters: None,
                                                                    parameters: vec![],
                                                                }), 
                                                                rhs: sway::Expression::from(sway::FunctionCall { 
                                                                    function: sway::Expression::from(sway::MemberAccess{ 
                                                                        expression: rhs, 
                                                                        member: format!("as_u{lhs_bits}"), 
                                                                    }), 
                                                                    generic_parameters: None, 
                                                                    parameters: vec![] 
                                                                }) 
                                                            })
                                                        } else if lhs_bits < rhs_bits {
                                                            sway::Expression::from(sway::BinaryExpression{ 
                                                                operator, 
                                                                lhs: sway::Expression::from(sway::FunctionCall { 
                                                                    function: sway::Expression::from(sway::MemberAccess{ 
                                                                        expression: sway::Expression::from(sway::FunctionCall {
                                                                            function: sway::Expression::from(sway::MemberAccess {
                                                                                expression: expression.clone(),
                                                                                member: "read".into(),
                                                                            }),
                                                                            generic_parameters: None,
                                                                            parameters: vec![],
                                                                        }), 
                                                                        member: format!("as_u{rhs_bits}"), 
                                                                    }), 
                                                                    generic_parameters: None, 
                                                                    parameters: vec![] 
                                                                }),
                                                                rhs, 
                                                            })
                                                        } else {
                                                            sway::Expression::from(sway::BinaryExpression{ 
                                                                operator, 
                                                                lhs: sway::Expression::from(sway::FunctionCall {
                                                                    function: sway::Expression::from(sway::MemberAccess {
                                                                        expression: expression.clone(),
                                                                        member: "read".into(),
                                                                    }),
                                                                    generic_parameters: None,
                                                                    parameters: vec![],
                                                                }),
                                                                rhs, 
                                                            })
                                                        }
                                                    }
                                
                                                    _ => sway::Expression::from(sway::BinaryExpression{ 
                                                        operator, 
                                                        lhs: sway::Expression::from(sway::FunctionCall {
                                                            function: sway::Expression::from(sway::MemberAccess {
                                                                expression: expression.clone(),
                                                                member: "read".into(),
                                                            }),
                                                            generic_parameters: None,
                                                            parameters: vec![],
                                                        }),
                                                        rhs, 
                                                    })
                                                }
                                            }
                                
                                            _ => sway::Expression::from(sway::BinaryExpression{ 
                                                operator, 
                                                lhs: sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::from(sway::MemberAccess {
                                                        expression: expression.clone(),
                                                        member: "read".into(),
                                                    }),
                                                    generic_parameters: None,
                                                    parameters: vec![],
                                                }),
                                                rhs, 
                                            })
                                        }
                                    }

                                    _ => sway::Expression::from(sway::BinaryExpression{ 
                                        operator, 
                                        lhs: sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::from(sway::MemberAccess {
                                                expression: expression.clone(),
                                                member: "read".into(),
                                            }),
                                            generic_parameters: None,
                                            parameters: vec![],
                                        }),
                                        rhs, 
                                    })
                                }
                            }

                            _ => sway::Expression::from(sway::BinaryExpression{ 
                                operator, 
                                lhs: sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: expression.clone(),
                                        member: "read".into(),
                                    }),
                                    generic_parameters: None,
                                    parameters: vec![],
                                }),
                                rhs, 
                            }),
                        }
                    }
                },
            ],
        }))
    } else {
        match &type_name {
            sway::TypeName::Identifier { name, .. } if name == "Vec" => match &expression {
                sway::Expression::ArrayAccess(array_access) => Ok(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression: array_access.expression.clone(),
                        member: "set".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![
                        array_access.index.clone(),
                        match operator {
                            "=" => rhs.clone(),
                            
                            _ => {
                                variable.borrow_mut().read_count += 1;
                                
                                sway::Expression::from(sway::BinaryExpression {
                                    operator: operator.trim_end_matches('=').into(),
    
                                    lhs: sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::from(sway::MemberAccess {
                                            expression: sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::from(sway::MemberAccess {
                                                    expression: array_access.expression.clone(),
                                                    member: "get".into(),
                                                }),
                                                generic_parameters: None,
                                                parameters: vec![
                                                    array_access.index.clone(),
                                                ],
                                            }),
                                            member: "unwrap".into(),
                                        }),
                                        generic_parameters: None,
                                        parameters: vec![],
                                    }),
    
                                    rhs: rhs.clone(),
                                })
                            }
                        }
                    ],
                })),

                sway::Expression::MemberAccess(member_access) => match &member_access.expression {
                    sway::Expression::ArrayAccess(array_access) => {
                        // x[i].member = value => {
                        //     let mut x = expr.get(i).unwrap();
                        //     x.member = value;
                        //     expr.set(i, a);
                        // }

                        Ok(sway::Expression::from(sway::Block {
                            statements: vec![
                                // let mut x = expr.get(i).unwrap();
                                sway::Statement::from(sway::Let {
                                    pattern: sway::LetPattern::from(sway::LetIdentifier {
                                        is_mutable: true,
                                        name: variable_name.clone(),
                                    }),
                                    type_name: None,
                                    value: sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::from(sway::MemberAccess {
                                            expression: sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::from(sway::MemberAccess {
                                                    expression: array_access.expression.clone(),
                                                    member: "get".into(),
                                                }),
                                                generic_parameters: None,
                                                parameters: vec![
                                                    array_access.index.clone(),
                                                ],
                                            }),
                                            member: "unwrap".into(),
                                        }),
                                        generic_parameters: None,
                                        parameters: vec![],
                                    }),
                                }),
                                // x.member = value;
                                sway::Statement::from(sway::Expression::from(sway::BinaryExpression {
                                    operator: "=".into(),
                                    lhs: sway::Expression::from(sway::MemberAccess {
                                        expression: sway::Expression::Identifier(variable_name.clone()),
                                        member: member_access.member.clone(),
                                    }),
                                    rhs: rhs.clone(),
                                })),
                                // expr.set(i, a);
                                sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: array_access.expression.clone(),
                                        member: "set".into(),
                                    }),
                                    generic_parameters: None,
                                    parameters: vec![
                                        sway::Expression::Identifier(variable_name.clone()),
                                    ],
                                })),
                            ],
                            final_expr: None,
                        }))
                    }

                    _ => todo!("translation assignment expression: {}", sway::TabbedDisplayer(expression)),
                }
                
                sway::Expression::Identifier(ident) if operator == "=" => {
                    Ok(sway::Expression::from(sway::BinaryExpression {
                        operator: "=".into(),
                        lhs: expression.clone(),
                        rhs: sway::Expression::Identifier(ident.clone())
                    }))
                }

                sway::Expression::FunctionCall(function_call) => match &function_call.function {
                    sway::Expression::MemberAccess(member_access) => match member_access.member.as_str() {
                        "get" if function_call.parameters.len() == 1 => {
                            let container_type = translated_definition.get_expression_type(scope.clone(), &member_access.expression)?;

                            todo!("translation {container_type} assignment expression: {}", sway::TabbedDisplayer(expression))
                        }

                        "unwrap" if function_call.parameters.is_empty() => match &member_access.expression {
                            sway::Expression::FunctionCall(function_call) => match &function_call.function {
                                sway::Expression::MemberAccess(member_access) => match member_access.member.as_str() {
                                    "get" if function_call.parameters.len() == 1 => {
                                        Ok(sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::from(sway::MemberAccess {
                                                expression: member_access.expression.clone(),
                                                member: "set".into(),
                                            }),
                                            generic_parameters: None,
                                            parameters: vec![
                                                function_call.parameters[0].clone(),
                                                rhs,
                                            ],
                                        }))
                                    }

                                    _ => todo!("translation assignment expression: {}", sway::TabbedDisplayer(expression)),
                                }

                                _ => todo!("translation assignment expression: {}", sway::TabbedDisplayer(expression)),
                            }

                            _ => todo!("translation assignment expression: {}", sway::TabbedDisplayer(expression)),
                        }

                        _ => todo!("translation assignment expression: {}", sway::TabbedDisplayer(expression)),
                    }

                    _ => todo!("translation assignment expression: {}", sway::TabbedDisplayer(expression)),
                }

                _ => todo!("translation assignment expression: {}", sway::TabbedDisplayer(expression)),
            }

            _ => match operator {
                "&=" | "|=" | "^=" => {
                    //
                    // NOTE:
                    // Sway doesn't have these operators, so we have to implement them manually.
                    //
                    
                    variable.borrow_mut().read_count += 1;
                    
                    Ok(sway::Expression::from(sway::BinaryExpression {
                        operator: "=".into(),
                        lhs: expression.clone(),
                        rhs: sway::Expression::from(sway::BinaryExpression {
                            operator: operator.trim_end_matches('=').into(),
                            lhs: expression.clone(),
                            rhs: rhs.clone(),
                        }),
                    }))
                }

                _ => Ok(sway::Expression::from(sway::BinaryExpression {
                    operator: operator.into(),
                    lhs: expression.clone(),
                    rhs: rhs.clone(),
                })),
            },
        }
    }
}

#[inline]
pub fn translate_assignment_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    operator: &str,
    lhs: &solidity::Expression,
    rhs: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    // println!(
    //     "Translating assignment expression: {lhs} {operator} {rhs}; from {}",
    //     match project.loc_to_line_and_column(&translated_definition.path, &lhs.loc()) {
    //         Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
    //         None => format!("{} - ", translated_definition.path.to_string_lossy()),
    //     },
    // );
    
    let rhs = match operator {
        "=" => {
            // println!("taking translate_pre_or_post_operator_value_expression path...");
            translate_pre_or_post_operator_value_expression(project, translated_definition, scope.clone(), rhs)?
        }
        _ => {
            // println!("taking translate_expression path...");
            translate_expression(project, translated_definition, scope.clone(), rhs)?
        }
    };
    
    let rhs_type_name = translated_definition.get_expression_type(scope.clone(), &rhs)?;
    
    let (variable, expression) = translate_variable_access_expression(project, translated_definition, scope.clone(), lhs)?;
    
    if variable.is_none() {
        if let sway::Expression::MemberAccess(member_access) = &expression {
            if let sway::TypeName::Identifier{ name, .. } = translated_definition.get_expression_type(scope, &member_access.expression)? {
                if let Some(struct_definition) = translated_definition.structs.iter().find(|s| s.name == name) {
                    if struct_definition.fields.iter().any(|f| f.name == member_access.member) {
                        return Ok(expression);
                    }
                }
            }
        }
        
        panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
    }

    let variable = variable.unwrap();

    create_assignment_expression(project, translated_definition, scope.clone(), operator, &expression, variable, &rhs, &rhs_type_name)
}

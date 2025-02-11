use std::{cell::RefCell, rc::Rc};
use solang_parser::pt as solidity;
use crate::{errors::Error, project::Project, sway, translate::{TranslatedDefinition, TranslationScope}};
use super::translate_expression;

#[inline]
pub fn translate_binary_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    operator: &str,
    lhs: &solidity::Expression,
    rhs: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    // Hack: x.code.length == 0 => x.as_contract_id().is_none()
    if let solidity::Expression::MemberAccess(_, x, member2) = lhs {
        if let solidity::Expression::MemberAccess(_, x, member1) = x.as_ref() {
            if member1.name == "code" && member2.name == "length" {
                let expression = translate_expression(project, translated_definition, scope.clone(), x)?;
                let type_name = translated_definition.get_expression_type(scope.clone(), &expression)?;

                if let sway::TypeName::Identifier { name, generic_parameters: None } = type_name {
                    if name == "Identity" {
                        if let solidity::Expression::NumberLiteral(_, value, _, _) = rhs {
                            if value == "0" {
                                return Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::from(sway::MemberAccess {
                                                expression,
                                                member: "as_contract_id".into(),
                                            }),
                                            generic_parameters: None,
                                            parameters: vec![],
                                        }),
                                        member: "is_none".into(),
                                    }),
                                    generic_parameters: None,
                                    parameters: vec![],
                                }));
                            }
                        }
                    }
                }
            }
        }
    }

    let lhs = translate_expression(project, translated_definition, scope.clone(), lhs)?;
    let mut new_name = None;

    if let sway::Expression::FunctionCall(expr) = &lhs {
        if let sway::Expression::MemberAccess(expr) = &expr.function {
            if expr.member == "read" {
                if let sway::Expression::MemberAccess(expr) = &expr.expression {
                    if let sway::Expression::Identifier(ident) = &expr.expression {
                        if ident == "storage" {
                            new_name = Some(expr.member.clone());
                        }
                    }
                }
            }
        }
    }
    
    if new_name.is_some() {
        let lhs_type_name = scope.borrow()
        .get_variable_from_new_name(new_name.as_ref().unwrap().as_str())
        .and_then(|v| v.borrow().abi_type_name.clone());

        if let Some(lhs_type_name) = lhs_type_name {
            if let solidity::Expression::FunctionCall(_, rhs_expr, rhs_exprs) = rhs {
                if let solidity::Expression::Variable(solidity::Identifier { name: rhs_name, .. }) = rhs_expr.as_ref() {
                    if *rhs_name == lhs_type_name.to_string() {
                        return Ok(sway::Expression::from(sway::BinaryExpression {
                            operator: operator.into(),
                            lhs,
                            rhs: translate_expression(project, translated_definition, scope.clone(), &rhs_exprs[0])?,
                        }));
                    }
                }
            }
        }
    }

    let rhs = translate_expression(project, translated_definition, scope.clone(), rhs)?;

    let lhs_type = translated_definition.get_expression_type(scope.clone(), &lhs)?;
    let rhs_type = translated_definition.get_expression_type(scope.clone(), &rhs)?;

    Ok(match lhs_type {
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
                                            operator: operator.into(), 
                                            lhs, 
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
                                            operator: operator.into(), 
                                            lhs: sway::Expression::from(sway::FunctionCall { 
                                                function: sway::Expression::from(sway::MemberAccess{ 
                                                    expression: lhs, 
                                                    member: format!("as_u{rhs_bits}"), 
                                                }), 
                                                generic_parameters: None, 
                                                parameters: vec![] 
                                            }),
                                            rhs, 
                                        })
                                    } else {
                                        sway::Expression::from(sway::BinaryExpression{ 
                                            operator: operator.into(), 
                                            lhs,
                                            rhs, 
                                        })
                                    }
                                }
            
                                _ => sway::Expression::from(sway::BinaryExpression{ 
                                    operator: operator.into(), 
                                    lhs,
                                    rhs, 
                                })
                            }
                        }
            
                        _ => sway::Expression::from(sway::BinaryExpression{ 
                                operator: operator.into(), 
                                lhs,
                                rhs, 
                            })
                    }
                }

                _ => sway::Expression::from(sway::BinaryExpression{ 
                        operator: operator.into(), 
                        lhs,
                        rhs, 
                    })
            }
        }

        _ => sway::Expression::from(sway::BinaryExpression{ 
                operator: operator.into(), 
                lhs,
                rhs, 
            })
    })
}

#[inline]
pub fn translate_unary_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    operator: &str,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    let expression = translate_expression(project, translated_definition, scope.clone(), expression)?;

    // NOTE: Sway does not have a negate operator, so we need to make sure to use the correct translation
    if operator == "-" {
        let type_name = translated_definition.get_expression_type(scope, &expression)?;

        match &type_name {
            sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                ("I8" | "I16" | "I32" | "I64" | "I128" | "I256", None) => return Ok(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression,
                        member: "wrapping_neg".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![],
                })),

                ("u8" | "u16" | "u32" | "u64" | "u256", None) => { 
                    let bits: usize = name.trim_start_matches('u').parse().unwrap();
                    
                    translated_definition.ensure_dependency_declared(
                        "sway_libs = { git = \"https://github.com/FuelLabs/sway-libs\", tag = \"v0.24.0\" }"
                    );
                    translated_definition.ensure_use_declared(format!("sway_libs::signed_integers::i{bits}::*").as_str());

                    return Ok(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::from(sway::MemberAccess {
                            expression: sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::from(sway::MemberAccess {
                                    expression: sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::Identifier(format!("I{bits}::from_uint")),
                                        generic_parameters: None,
                                        parameters: vec![
                                            expression.clone()
                                        ],
                                    }),
                                    member: "wrapping_neg".into(),
                                }),
                                generic_parameters: None,
                                parameters: vec![],
                            }),
                            member: "underlying".into(),
                        }),
                        generic_parameters: None,
                        parameters: vec![],
                    }))
                },

                _ => {
                    // HACK: allow literals to be negated
                    if let sway::Expression::Literal(sway::Literal::DecInt(_, _) | sway::Literal::HexInt(_, _)) = &expression {
                        return Ok(sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::from(sway::MemberAccess {
                                expression,
                                member: "wrapping_neg".into(),
                            }),
                            generic_parameters: None,
                            parameters: vec![],
                        }));
                    }

                    panic!("Unhandled {type_name} negate operator translation")
                }
            }

            _ => panic!("Unhandled {type_name} negate operator translation"),
        }
    }

    Ok(sway::Expression::from(sway::UnaryExpression {
        operator: operator.into(),
        expression,
    }))
}

use crate::{error::Error, project::Project, sway, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_binary_expression(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: Rc<RefCell<TranslationScope>>,
    operator: &str,
    lhs: &solidity::Expression,
    rhs: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    // Hack: x.code.length == 0 => x.as_contract_id().is_none()
    if let solidity::Expression::MemberAccess(_, x, member2) = lhs {
        if let solidity::Expression::MemberAccess(_, x, member1) = x.as_ref() {
            if member1.name == "code" && member2.name == "length" {
                let expression = translate_expression(project, module.clone(), scope.clone(), x)?;
                let type_name =
                    module
                        .borrow_mut()
                        .get_expression_type(project, scope.clone(), &expression)?;

                if let sway::TypeName::Identifier {
                    name,
                    generic_parameters: None,
                } = type_name
                {
                    if name == "Identity" {
                        if let solidity::Expression::NumberLiteral(_, value, _, _) = rhs {
                            if value == "0" {
                                return Ok(sway::Expression::create_function_calls(
                                    Some(expression),
                                    &[
                                        ("as_contract_id", Some((None, vec![]))),
                                        ("is_none", Some((None, vec![]))),
                                    ],
                                ));
                            }
                        }
                    }
                }
            }
        }
    }

    let mut lhs = translate_expression(project, module.clone(), scope.clone(), lhs)?;
    let mut lhs_type = module
        .borrow_mut()
        .get_expression_type(project, scope.clone(), &lhs)?;

    let mut rhs = translate_expression(project, module.clone(), scope.clone(), rhs)?;
    println!("rhs : {}", sway::TabbedDisplayer(&rhs));
    let mut rhs_type = module
        .borrow_mut()
        .get_expression_type(project, scope.clone(), &rhs)?;

    if lhs_type.is_storage_key() {
        lhs = sway::Expression::create_function_calls(Some(lhs), &[("read", Some((None, vec![])))]);
        lhs_type = module
            .borrow_mut()
            .get_expression_type(project, scope.clone(), &lhs)?;
    }

    if rhs_type.is_storage_key() {
        rhs = sway::Expression::create_function_calls(Some(rhs), &[("read", Some((None, vec![])))]);
        rhs_type = module
            .borrow_mut()
            .get_expression_type(project, scope.clone(), &rhs)?;
    }

    // HACK: de-cast identity abi cast comparisons
    let mut abi_check = |lhs_type: &sway::TypeName,
                         rhs: &mut sway::Expression,
                         rhs_type: &mut sway::TypeName|
     -> bool {
        if lhs_type.is_identity() {
            if let sway::Expression::FunctionCall(expr) = &rhs {
                if let Some(ident) = expr.function.as_identifier() {
                    if ident == "abi" && expr.parameters.len() == 2 {
                        *rhs = expr.parameters[1].clone();
                        if let sway::Expression::FunctionCall(f) = &rhs {
                            if let sway::Expression::MemberAccess(e) = &f.function {
                                if e.member == "into" {
                                    if let sway::Expression::FunctionCall(f) = &e.expression {
                                        if let sway::Expression::MemberAccess(e) = &f.function {
                                            if e.member == "unwrap" {
                                                if let sway::Expression::FunctionCall(f) =
                                                    &e.expression
                                                {
                                                    if let sway::Expression::MemberAccess(e) =
                                                        &f.function
                                                    {
                                                        if e.member == "as_contract_id" {
                                                            *rhs = e.expression.clone();
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        *rhs_type = module
                            .borrow_mut()
                            .get_expression_type(project, scope.clone(), rhs)
                            .unwrap();
                        return true;
                    }
                }
            }
        }
        false
    };

    if !abi_check(&lhs_type, &mut rhs, &mut rhs_type) {
        abi_check(&rhs_type, &mut lhs, &mut lhs_type);
    }

    rhs = coerce_expression(&rhs, &rhs_type, &lhs_type).unwrap();

    Ok(sway::Expression::from(sway::BinaryExpression {
        operator: operator.into(),
        lhs,
        rhs,
    }))
}

#[inline]
pub fn translate_unary_expression(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: Rc<RefCell<TranslationScope>>,
    operator: &str,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    let expression = translate_expression(project, module.clone(), scope.clone(), expression)?;

    // NOTE: Sway does not have a negate operator, so we need to make sure to use the correct translation
    if operator == "-" {
        let type_name =
            module
                .borrow_mut()
                .get_expression_type(project, scope.clone(), &expression)?;

        match &type_name {
            sway::TypeName::Identifier {
                name,
                generic_parameters,
            } => match (name.as_str(), generic_parameters.as_ref()) {
                ("I8" | "I16" | "I32" | "I64" | "I128" | "I256", None) => {
                    return Ok(sway::Expression::create_function_calls(
                        Some(expression),
                        &[("wrapping_neg", Some((None, vec![])))],
                    ));
                }

                ("u8" | "u16" | "u32" | "u64" | "u256", None) => {
                    let bits: usize = name.trim_start_matches('u').parse().unwrap();

                    module.borrow_mut().ensure_dependency_declared(
                        "sway_libs = { git = \"https://github.com/FuelLabs/sway-libs\", tag = \"v0.25.2\" }"
                    );
                    module.borrow_mut().ensure_use_declared(
                        format!("sway_libs::signed_integers::i{bits}::*").as_str(),
                    );

                    return Ok(sway::Expression::create_function_calls(
                        None,
                        &[
                            (
                                format!("I{bits}::from_uint").as_str(),
                                Some((None, vec![expression.clone()])),
                            ),
                            ("wrapping_neg", Some((None, vec![]))),
                            ("underlying", Some((None, vec![]))),
                        ],
                    ));
                }

                _ => {
                    // HACK: allow literals to be negated
                    if let sway::Expression::Literal(
                        sway::Literal::DecInt(_, _) | sway::Literal::HexInt(_, _),
                    ) = &expression
                    {
                        return Ok(sway::Expression::create_function_calls(
                            Some(expression),
                            &[("wrapping_neg", Some((None, vec![])))],
                        ));
                    }

                    panic!("Unhandled {type_name} negate operator translation")
                }
            },

            _ => panic!("Unhandled {type_name} negate operator translation"),
        }
    }

    Ok(sway::Expression::from(sway::UnaryExpression {
        operator: operator.into(),
        expression,
    }))
}

#[inline]
pub fn translate_power_expression(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: Rc<RefCell<TranslationScope>>,
    lhs: &solidity::Expression,
    rhs: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    // lhs ** rhs => lhs.pow(rhs)

    // Ensure std::math::Power is imported for the pow function
    module.borrow_mut().ensure_use_declared("std::math::Power");

    let lhs = translate_expression(project, module.clone(), scope.clone(), lhs)?;
    let rhs = translate_expression(project, module.clone(), scope.clone(), rhs)?;

    Ok(sway::Expression::create_function_calls(
        Some(lhs),
        &[("pow", Some((None, vec![rhs])))],
    ))
}

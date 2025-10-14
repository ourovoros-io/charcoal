use crate::{error::Error, project::Project, sway, translate::*};
use num_bigint::BigUint;
use num_traits::ToPrimitive;
use num_traits::{Num, Zero};
use solang_parser::pt as solidity;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Shl, Shr, Sub};
use std::{cell::RefCell, rc::Rc};

mod address_call;
mod array;
mod assignment;
mod conditional;
mod delete;
mod expression_type;
mod function_call;
mod list;
mod literals;
mod member_access;
mod new;
mod operators;
mod parenthesis;
mod pre_post;
mod variable;
pub use address_call::*;
pub use array::*;
pub use assignment::*;
pub use conditional::*;
pub use delete::*;
pub use expression_type::*;
pub use function_call::*;
pub use list::*;
pub use literals::*;
pub use member_access::*;
pub use new::*;
pub use operators::*;
pub use parenthesis::*;
pub use pre_post::*;
pub use variable::*;

pub fn evaluate_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    type_name: &sway::TypeName,
    expression: &sway::Expression,
) -> sway::Expression {
    match expression {
        sway::Expression::Literal(literal) => match literal {
            sway::Literal::DecInt(value, _) | sway::Literal::HexInt(value, _) => match type_name {
                sway::TypeName::Identifier {
                    name,
                    generic_parameters,
                } => match (name.as_str(), generic_parameters.as_ref()) {
                    ("b256", None) if value.is_zero() => {
                        sway::Expression::create_function_call("b256::zero", None, vec![])
                    }

                    _ => expression.clone(),
                },

                _ => expression.clone(),
            },

            _ => expression.clone(),
        },

        sway::Expression::PathExpr(path_expr) => {
            let Some(identifier) = path_expr.as_identifier() else {
                todo!("evaluate non-identifier path expression: {path_expr} - {path_expr:#?}")
            };

            if let Some(constant) = module.borrow().constants.iter().find(|c| c.name == identifier) {
                assert!(type_name.is_compatible_with(&constant.type_name));
                return constant.value.as_ref().unwrap().clone();
            }

            if let Some(configurable) = module.borrow().configurable.as_ref()
                && let Some(field) = configurable.fields.iter().find(|f| f.name == identifier)
            {
                assert!(type_name.is_compatible_with(&field.type_name));
                return field.value.clone();
            }

            if identifier.contains("__") {
                let parts = identifier.split("__").collect::<Vec<_>>();
                if parts.len() == 2 && !parts[0].is_empty() {
                    if let Some(enum_item) = project.find_enum(module.clone(), parts[0]) {
                        let enum_type_name =
                            get_underlying_type(project, module.clone(), &enum_item.type_definition.name);

                        let type_name = get_underlying_type(project, module.clone(), type_name);

                        assert!(
                            type_name.is_compatible_with(&enum_type_name),
                            "{} vs {}",
                            type_name,
                            enum_type_name
                        );

                        if let Some(constant) = enum_item.constants.iter().find(|c| c.name == identifier) {
                            return constant.value.as_ref().unwrap().clone();
                        }
                    }
                }
            }

            let Some(variable) = scope.borrow().find_variable(|v| v.borrow().new_name == *identifier) else {
                panic!("Failed to find variable: {} in scope", identifier);
            };

            todo!("evaluate path expression: {expression:#?} - {variable:#?}")
        }

        sway::Expression::FunctionCall(function_call) => match &function_call.function {
            sway::Expression::PathExpr(path_expr) => {
                let Some(identifier) = path_expr.as_identifier() else {
                    match path_expr.to_string().as_str() {
                        "Address::from" => {
                            return sway::Expression::from(sway::FunctionCall {
                                function: function_call.function.clone(),
                                generic_parameters: function_call.generic_parameters.clone(),
                                parameters: function_call
                                    .parameters
                                    .iter()
                                    .map(|p| evaluate_expression(project, module.clone(), scope.clone(), type_name, p))
                                    .collect(),
                            });
                        }

                        "Address::zero" => {
                            return sway::Expression::from(sway::FunctionCall {
                                function: function_call.function.clone(),
                                generic_parameters: function_call.generic_parameters.clone(),
                                parameters: function_call
                                    .parameters
                                    .iter()
                                    .map(|p| evaluate_expression(project, module.clone(), scope.clone(), type_name, p))
                                    .collect(),
                            });
                        }

                        "b256::from_be_bytes" => {
                            // TODO: sigh...
                            return expression.clone();
                        }

                        "b256::zero" => {
                            return expression.clone();
                        }

                        "keccak256" | "std::hash::keccak256" if function_call.parameters.len() == 1 => {
                            match &function_call.parameters[0] {
                                sway::Expression::Literal(sway::Literal::String(s)) => {
                                    use sha3::Digest;

                                    let mut hasher = sha3::Keccak256::new();
                                    hasher.update(s.as_bytes());

                                    let value =
                                        BigUint::from_str_radix(hex::encode(hasher.finalize()).as_str(), 16).unwrap();

                                    return sway::Expression::Commented(
                                        format!("{}", sway::TabbedDisplayer(expression)),
                                        Box::new(if value.is_zero() {
                                            sway::Expression::create_function_call("b256::zero", None, vec![])
                                        } else {
                                            sway::Expression::from(sway::Literal::HexInt(value, None))
                                        }),
                                    );
                                }

                                _ => todo!("evaluate function call: {expression:#?}"),
                            }
                        }

                        "I8::from_uint" | "I16::from_uint" | "I32::from_uint" | "I64::from_uint"
                        | "I128::from_uint" | "I256::from_uint" => {
                            return evaluate_expression(
                                project,
                                module,
                                scope.clone(),
                                type_name,
                                &function_call.parameters[0],
                            );
                        }

                        "Identity::Address" | "Identity::ContractId" => {
                            return sway::Expression::from(sway::FunctionCall {
                                function: function_call.function.clone(),
                                generic_parameters: function_call.generic_parameters.clone(),
                                parameters: function_call
                                    .parameters
                                    .iter()
                                    .map(|p| evaluate_expression(project, module.clone(), scope.clone(), type_name, p))
                                    .collect(),
                            });
                        }

                        "u8::max" => {
                            assert!(function_call.parameters.is_empty());
                            return sway::Expression::from(sway::Literal::DecInt(u8::MAX.into(), None));
                        }

                        "u8::min" => {
                            assert!(function_call.parameters.is_empty());
                            return sway::Expression::from(sway::Literal::DecInt(u8::MIN.into(), None));
                        }

                        "u16::max" => {
                            assert!(function_call.parameters.is_empty());
                            return sway::Expression::from(sway::Literal::DecInt(u16::MAX.into(), None));
                        }

                        "u16::min" => {
                            assert!(function_call.parameters.is_empty());
                            return sway::Expression::from(sway::Literal::DecInt(u16::MIN.into(), None));
                        }

                        "u32::max" => {
                            assert!(function_call.parameters.is_empty());
                            return sway::Expression::from(sway::Literal::DecInt(u32::MAX.into(), None));
                        }

                        "u32::min" => {
                            assert!(function_call.parameters.is_empty());
                            return sway::Expression::from(sway::Literal::DecInt(u32::MIN.into(), None));
                        }

                        "u32::try_from" => {
                            assert!(function_call.parameters.len() == 1);
                            let param_type = get_expression_type(
                                project,
                                module.clone(),
                                scope.clone(),
                                &function_call.parameters[0],
                            )
                            .unwrap();

                            if param_type.is_u32() {
                                return function_call.parameters[0].clone().into_some_call();
                            }

                            let mut container = evaluate_expression(
                                project,
                                module.clone(),
                                scope.clone(),
                                &param_type,
                                &function_call.parameters[0],
                            );

                            if let sway::Expression::Tuple(tuple) = &container
                                && tuple.len() == 1
                            {
                                let container_type_name =
                                    get_expression_type(project, module.clone(), scope.clone(), &tuple[0]).unwrap();

                                container = evaluate_expression(
                                    project,
                                    module.clone(),
                                    scope.clone(),
                                    &container_type_name,
                                    &tuple[0],
                                );
                            }

                            let sway::Expression::Literal(
                                sway::Literal::DecInt(value, _) | sway::Literal::HexInt(value, _),
                            ) = &container
                            else {
                                todo!("{}", container.display())
                            };

                            if value.bits() > 32 {
                                let extra = value >> 32;

                                if format!("{:x}", extra).chars().all(|c| c == 'f') {
                                    return sway::Expression::Literal(sway::Literal::HexInt(
                                        value & BigUint::from(u32::MAX),
                                        Some("u32".to_string()),
                                    ))
                                    .into_some_call();
                                }
                            }

                            if value.bits() <= 32 {
                                match &container {
                                    sway::Expression::Literal(sway::Literal::DecInt(value, _)) => {
                                        return sway::Expression::Literal(sway::Literal::DecInt(
                                            value.clone(),
                                            Some("u32".to_string()),
                                        ))
                                        .into_some_call();
                                    }

                                    sway::Expression::Literal(sway::Literal::HexInt(value, _)) => {
                                        return sway::Expression::Literal(sway::Literal::HexInt(
                                            value.clone(),
                                            Some("u32".to_string()),
                                        ))
                                        .into_some_call();
                                    }

                                    _ => todo!(),
                                }
                            }

                            todo!("container: {} - {}", container.display(), value.bits())
                        }

                        "u64::max" => {
                            assert!(function_call.parameters.is_empty());
                            return sway::Expression::from(sway::Literal::DecInt(u64::MAX.into(), None));
                        }

                        "u64::min" => {
                            assert!(function_call.parameters.is_empty());
                            return sway::Expression::from(sway::Literal::DecInt(u64::MIN.into(), None));
                        }

                        "u64::try_from" => {
                            assert!(function_call.parameters.len() == 1);
                            let param_type = get_expression_type(
                                project,
                                module.clone(),
                                scope.clone(),
                                &function_call.parameters[0],
                            )
                            .unwrap();

                            if param_type.is_u64() {
                                return function_call.parameters[0].clone().into_some_call();
                            }

                            let mut container = function_call.parameters[0].clone();

                            if let sway::Expression::Tuple(tuple) = &container
                                && tuple.len() == 1
                            {
                                let container_type_name =
                                    get_expression_type(project, module.clone(), scope.clone(), &tuple[0]).unwrap();

                                container = evaluate_expression(
                                    project,
                                    module.clone(),
                                    scope.clone(),
                                    &container_type_name,
                                    &tuple[0],
                                );
                            }

                            let mut comment = None;

                            if let sway::Expression::Commented(c, expr) = &container {
                                comment = Some(c.clone());
                                container = expr.as_ref().clone();
                            }

                            let sway::Expression::Literal(
                                sway::Literal::DecInt(value, _) | sway::Literal::HexInt(value, _),
                            ) = &container
                            else {
                                todo!("{}", container.display())
                            };

                            if value.bits() <= 64 {
                                match &container {
                                    sway::Expression::Literal(sway::Literal::DecInt(value, _)) => {
                                        return if let Some(comment) = comment {
                                            sway::Expression::Commented(
                                                comment,
                                                Box::new(
                                                    sway::Expression::Literal(sway::Literal::DecInt(
                                                        value.clone(),
                                                        Some("u64".to_string()),
                                                    ))
                                                    .into_some_call(),
                                                ),
                                            )
                                        } else {
                                            sway::Expression::Literal(sway::Literal::DecInt(
                                                value.clone(),
                                                Some("u64".to_string()),
                                            ))
                                            .into_some_call()
                                        };
                                    }

                                    sway::Expression::Literal(sway::Literal::HexInt(value, _)) => {
                                        return if let Some(comment) = comment {
                                            sway::Expression::Commented(
                                                comment,
                                                Box::new(
                                                    sway::Expression::Literal(sway::Literal::HexInt(
                                                        value.clone(),
                                                        Some("u64".to_string()),
                                                    ))
                                                    .into_some_call(),
                                                ),
                                            )
                                        } else {
                                            sway::Expression::Literal(sway::Literal::HexInt(
                                                value.clone(),
                                                Some("u64".to_string()),
                                            ))
                                            .into_some_call()
                                        };
                                    }

                                    _ => todo!(),
                                }
                            }

                            todo!()
                        }

                        "u256::from_be_bytes" => {
                            // TODO: sigh...
                            return expression.clone();
                        }

                        _ => {}
                    }

                    todo!(
                        "evaluate non-identifier path function call expression: {path_expr}({}) - {path_expr:#?}",
                        function_call
                            .parameters
                            .iter()
                            .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
                            .collect::<Result<Vec<_>, _>>()
                            .unwrap()
                            .iter()
                            .map(|t| t.to_string())
                            .collect::<Vec<_>>()
                            .join(", "),
                    )
                };

                match identifier {
                    "todo!" => expression.clone(),

                    "abi" => sway::Expression::create_todo(Some(format!("{}", sway::TabbedDisplayer(expression)))),

                    "__to_str_array" => expression.clone(),

                    _ => todo!("evaluate function call: {expression:#?}"),
                }
            }

            sway::Expression::MemberAccess(member_access) => {
                let container_type_name =
                    get_expression_type(project, module.clone(), scope.clone(), &member_access.expression).unwrap();

                let mut container = evaluate_expression(
                    project,
                    module.clone(),
                    scope.clone(),
                    &container_type_name,
                    &member_access.expression,
                );

                match member_access.member.as_str() {
                    "as_u32" if function_call.parameters.is_empty() => {
                        if container_type_name.is_u32() {
                            return container.clone();
                        }

                        let mut comment = None;

                        if let sway::Expression::Commented(c, expr) = &container {
                            comment = Some(c.clone());
                            container = expr.as_ref().clone();
                        }

                        let result;

                        if let sway::Expression::Literal(
                            sway::Literal::DecInt(_, suffix) | sway::Literal::HexInt(_, suffix),
                        ) = &container
                        {
                            if let Some("u32") = suffix.as_ref().map(|s| s.as_str()) {
                                result = container.clone();
                            } else {
                                match &container {
                                    sway::Expression::Literal(sway::Literal::DecInt(value, _)) => {
                                        result = sway::Expression::Literal(sway::Literal::DecInt(
                                            value.clone(),
                                            Some("u32".to_string()),
                                        ));
                                    }
                                    sway::Expression::Literal(sway::Literal::HexInt(value, _)) => {
                                        result = sway::Expression::Literal(sway::Literal::HexInt(
                                            value.clone(),
                                            Some("u32".to_string()),
                                        ));
                                    }

                                    _ => todo!("container: {}", container.display()),
                                }
                            }
                        } else {
                            todo!()
                        }

                        return match comment {
                            Some(c) => sway::Expression::Commented(c, Box::new(result)),
                            None => result,
                        };
                    }

                    "as_u256" if function_call.parameters.is_empty() => {
                        if container_type_name.is_u256() {
                            return container.clone();
                        }

                        let mut comment = None;

                        if let sway::Expression::Commented(c, expr) = &container {
                            comment = Some(c.clone());
                            container = expr.as_ref().clone();
                        }

                        let result;

                        if let sway::Expression::Literal(
                            sway::Literal::DecInt(_, suffix) | sway::Literal::HexInt(_, suffix),
                        ) = &container
                        {
                            if let Some("u256") = suffix.as_ref().map(|s| s.as_str()) {
                                result = container.clone();
                            } else {
                                match &container {
                                    sway::Expression::Literal(sway::Literal::DecInt(value, _)) => {
                                        result = sway::Expression::Literal(sway::Literal::DecInt(
                                            value.clone(),
                                            Some("u256".to_string()),
                                        ));
                                    }
                                    sway::Expression::Literal(sway::Literal::HexInt(value, _)) => {
                                        result = sway::Expression::Literal(sway::Literal::HexInt(
                                            value.clone(),
                                            Some("u256".to_string()),
                                        ));
                                    }

                                    _ => todo!("container: {}", container.display()),
                                }
                            }
                        } else {
                            todo!()
                        }

                        return match comment {
                            Some(c) => sway::Expression::Commented(c, Box::new(result)),
                            None => result,
                        };
                    }

                    "as_b256" if function_call.parameters.is_empty() => {
                        if container_type_name.is_b256() {
                            return container.clone();
                        }

                        let mut comment = None;

                        if let sway::Expression::Commented(c, expr) = &container {
                            comment = Some(c.clone());
                            container = expr.as_ref().clone();
                        }

                        if let sway::Expression::Literal(
                            sway::Literal::DecInt(_, suffix) | sway::Literal::HexInt(_, suffix),
                        ) = &container
                        {
                            if let Some("b256") = suffix.as_ref().map(|s| s.as_str()) {
                                return container.clone();
                            }

                            match &container {
                                sway::Expression::Literal(sway::Literal::DecInt(value, _)) => {
                                    return if let Some(comment) = comment {
                                        sway::Expression::Commented(
                                            comment,
                                            Box::new(sway::Expression::Literal(sway::Literal::DecInt(
                                                value.clone(),
                                                Some("b256".to_string()),
                                            ))),
                                        )
                                    } else {
                                        sway::Expression::Literal(sway::Literal::DecInt(
                                            value.clone(),
                                            Some("b256".to_string()),
                                        ))
                                    };
                                }
                                sway::Expression::Literal(sway::Literal::HexInt(value, _)) => {
                                    return if let Some(comment) = comment {
                                        sway::Expression::Commented(
                                            comment,
                                            Box::new(sway::Expression::Literal(sway::Literal::HexInt(
                                                value.clone(),
                                                Some("b256".to_string()),
                                            ))),
                                        )
                                    } else {
                                        sway::Expression::Literal(sway::Literal::HexInt(
                                            value.clone(),
                                            Some("b256".to_string()),
                                        ))
                                    };
                                }

                                _ => {}
                            }
                        }

                        todo!("container: {}", container.display());
                    }

                    "wrapping_neg" if function_call.parameters.is_empty() => {
                        assert!(container_type_name.is_int());
                        let Some(bits) = container_type_name.int_bits() else {
                            unreachable!()
                        };

                        let sway::Expression::Literal(
                            sway::Literal::DecInt(value, suffix) | sway::Literal::HexInt(value, suffix),
                        ) = &container
                        else {
                            unreachable!()
                        };

                        sway::Expression::Literal(sway::Literal::HexInt(
                            ((BigUint::one() << bits) - BigUint::one()) - (value - BigUint::one()),
                            suffix.clone(),
                        ))
                    }

                    "underlying" if function_call.parameters.is_empty() => {
                        assert!(container_type_name.is_int());
                        let sway::Expression::Literal(_) = &container else {
                            todo!()
                        };

                        container
                    }

                    "pow" if function_call.parameters.len() == 1 => {
                        let mut rhs = evaluate_expression(
                            project,
                            module,
                            scope.clone(),
                            type_name,
                            &function_call.parameters[0],
                        );

                        let mut comment = None;

                        if let sway::Expression::Commented(c, expr) = &container {
                            comment = Some(c.clone());
                            container = expr.as_ref().clone();
                        }

                        let sway::Expression::Literal(
                            sway::Literal::DecInt(lhs_value, lhs_suffix) | sway::Literal::HexInt(lhs_value, lhs_suffix),
                        ) = &container
                        else {
                            todo!("integer pow lhs expression: {container:#?}")
                        };

                        let mut rhs_comment = None;

                        if let sway::Expression::Commented(c, expr) = &rhs {
                            rhs_comment = Some(c.clone());
                            rhs = expr.as_ref().clone();
                        }

                        let sway::Expression::Literal(
                            sway::Literal::DecInt(rhs_value, _) | sway::Literal::HexInt(rhs_value, _),
                        ) = rhs
                        else {
                            todo!("integer pow rhs expression: {rhs:#?}")
                        };

                        let rhs_value: u32 = rhs_value.to_string().parse().unwrap();

                        sway::Expression::Commented(
                            if let (Some(lhs_comment), Some(rhs_comment)) = (comment, rhs_comment) {
                                format!("{} ** {}", lhs_comment, rhs_comment)
                            } else {
                                format!("{}", sway::TabbedDisplayer(expression))
                            },
                            Box::new(sway::Expression::from(sway::Literal::DecInt(
                                lhs_value.pow(rhs_value),
                                if type_name.is_uint() {
                                    // TODO: we should verify the suffix is valid
                                    Some(type_name.to_string().to_lowercase())
                                } else {
                                    lhs_suffix.clone()
                                },
                            ))),
                        )
                    }

                    "unwrap" if function_call.parameters.is_empty() => {
                        let mut comment = None;

                        if let sway::Expression::Commented(c, expr) = &container {
                            comment = Some(c.clone());
                            container = expr.as_ref().clone();
                        }

                        let sway::Expression::FunctionCall(f) = container else {
                            todo!("container: {}", container.display())
                        };

                        let Some("Some") = f.function.as_identifier() else {
                            todo!()
                        };

                        return if let Some(comment) = comment {
                            sway::Expression::Commented(comment, Box::new(f.parameters[0].clone()))
                        } else {
                            f.parameters[0].clone()
                        };
                    }

                    "bits" if function_call.parameters.is_empty() => {
                        assert!(container_type_name.is_identity());

                        if let sway::Expression::FunctionCall(f) = &container
                            && let sway::Expression::PathExpr(path_expr) = &f.function
                            && let sway::PathExprRoot::Identifier(identifier) = &path_expr.root
                            && identifier == "Identity"
                            && path_expr.segments.len() == 1
                            && path_expr.segments[0].name == "Address"
                            && path_expr.segments[0].generic_parameters.is_none()
                            && f.parameters.len() == 1
                        {
                            if let sway::Expression::FunctionCall(f) = &f.parameters[0]
                                && let sway::Expression::PathExpr(path_expr) = &f.function
                                && let sway::PathExprRoot::Identifier(identifier) = &path_expr.root
                                && identifier == "Address"
                                && path_expr.segments.len() == 1
                                && path_expr.segments[0].name == "from"
                                && path_expr.segments[0].generic_parameters.is_none()
                                && f.parameters.len() == 1
                            {
                                assert!(matches!(
                                    &f.parameters[0],
                                    sway::Expression::Literal(
                                        sway::Literal::DecInt(_, _) | sway::Literal::HexInt(_, _)
                                    )
                                ));

                                return sway::Expression::Commented(
                                    container.display().to_string(),
                                    Box::new(f.parameters[0].clone()),
                                );
                            }
                        }

                        todo!("container: {}", container.display())
                    }

                    member => todo!(
                        "translate {container_type_name}::{member} member call: {}",
                        sway::TabbedDisplayer(expression)
                    ),
                }
            }

            _ => todo!("evaluate function call: {expression:#?}"),
        },

        sway::Expression::FunctionCallBlock(_) => {
            todo!("evaluate function call block: {expression:#?}")
        }

        sway::Expression::Tuple(exprs) => {
            let exprs = exprs
                .iter()
                .map(|e| {
                    let expr_type_name = get_expression_type(project, module.clone(), scope.clone(), e).unwrap();
                    evaluate_expression(project, module.clone(), scope.clone(), &expr_type_name, e)
                })
                .collect::<Vec<_>>();

            if exprs.len() == 1 {
                return exprs[0].clone();
            }

            sway::Expression::Tuple(exprs)
        }

        sway::Expression::BinaryExpression(binary_expr) => {
            let lhs_type = get_expression_type(project, module.clone(), scope.clone(), &binary_expr.lhs).unwrap();
            let lhs_type = get_underlying_type(project, module.clone(), &lhs_type);
            let mut lhs = evaluate_expression(project, module.clone(), scope.clone(), &lhs_type, &binary_expr.lhs);

            let rhs_type = get_expression_type(project, module.clone(), scope.clone(), &binary_expr.rhs).unwrap();
            let rhs_type = get_underlying_type(project, module.clone(), &rhs_type);
            let mut rhs = evaluate_expression(project, module.clone(), scope.clone(), &rhs_type, &binary_expr.rhs);

            let mut lhs_comment = None;
            let mut rhs_comment = None;

            if let sway::Expression::Commented(comment, expr) = lhs.clone() {
                lhs = expr.as_ref().clone();
                lhs_comment = Some(comment.clone());
            }

            if let sway::Expression::Commented(comment, expr) = rhs.clone() {
                rhs = expr.as_ref().clone();
                rhs_comment = Some(comment.clone());
            }

            assert!(lhs_type.is_compatible_with(&rhs_type), "{} vs {}", lhs_type, rhs_type);

            let op = match binary_expr.operator.as_str() {
                "+" => BigUint::add,
                "-" => BigUint::sub,
                "*" => BigUint::mul,
                "/" => BigUint::div,
                "&" => BigUint::bitand,
                "|" => BigUint::bitor,
                "^" => BigUint::bitxor,
                "%" => BigUint::rem,
                "<<" | ">>" => {
                    let op = match binary_expr.operator.as_str() {
                        "<<" => BigUint::shl,
                        ">>" => BigUint::shr,
                        _ => unreachable!(),
                    };

                    return match (lhs, rhs) {
                        (
                            sway::Expression::Literal(sway::Literal::DecInt(lhs_value, lhs_suffix)),
                            sway::Expression::Literal(sway::Literal::DecInt(rhs_value, rhs_suffix)),
                        ) => {
                            if let Some(lhs_comment) = lhs_comment {
                                if let Some(rhs_comment) = rhs_comment {
                                    sway::Expression::Commented(
                                        format!("{} {} {}", lhs_comment, binary_expr.operator, rhs_comment),
                                        Box::new(sway::Expression::Literal(sway::Literal::DecInt(
                                            op(lhs_value, rhs_value.to_u128().unwrap()),
                                            lhs_suffix.or(rhs_suffix),
                                        ))),
                                    )
                                } else {
                                    sway::Expression::Commented(
                                        format!("{} {} {}", lhs_comment, binary_expr.operator, rhs_value),
                                        Box::new(sway::Expression::Literal(sway::Literal::DecInt(
                                            op(lhs_value, rhs_value.to_u128().unwrap()),
                                            lhs_suffix.or(rhs_suffix),
                                        ))),
                                    )
                                }
                            } else {
                                sway::Expression::Literal(sway::Literal::DecInt(
                                    op(lhs_value, rhs_value.to_u128().unwrap()),
                                    lhs_suffix.or(rhs_suffix),
                                ))
                            }
                        }

                        (
                            sway::Expression::Literal(sway::Literal::HexInt(lhs_value, lhs_suffix)),
                            sway::Expression::Literal(sway::Literal::HexInt(rhs_value, rhs_suffix)),
                        ) => {
                            if let Some(lhs_comment) = lhs_comment {
                                if let Some(rhs_comment) = rhs_comment {
                                    sway::Expression::Commented(
                                        format!("{} {} {}", lhs_comment, binary_expr.operator, rhs_comment),
                                        Box::new(sway::Expression::Literal(sway::Literal::HexInt(
                                            op(lhs_value, rhs_value.to_u128().unwrap()),
                                            lhs_suffix.or(rhs_suffix),
                                        ))),
                                    )
                                } else {
                                    sway::Expression::Commented(
                                        format!("{} {} {}", lhs_comment, binary_expr.operator, rhs_value),
                                        Box::new(sway::Expression::Literal(sway::Literal::HexInt(
                                            op(lhs_value, rhs_value.to_u128().unwrap()),
                                            lhs_suffix.or(rhs_suffix),
                                        ))),
                                    )
                                }
                            } else {
                                sway::Expression::Literal(sway::Literal::HexInt(
                                    op(lhs_value, rhs_value.to_u128().unwrap()),
                                    lhs_suffix.or(rhs_suffix),
                                ))
                            }
                        }

                        (
                            sway::Expression::Literal(sway::Literal::DecInt(lhs_value, lhs_suffix)),
                            sway::Expression::Literal(sway::Literal::HexInt(rhs_value, rhs_suffix)),
                        ) => {
                            if let Some(lhs_comment) = lhs_comment {
                                if let Some(rhs_comment) = rhs_comment {
                                    sway::Expression::Commented(
                                        format!("{} {} {}", lhs_comment, binary_expr.operator, rhs_comment),
                                        Box::new(sway::Expression::Literal(sway::Literal::DecInt(
                                            op(lhs_value, rhs_value.to_u128().unwrap()),
                                            lhs_suffix.or(rhs_suffix),
                                        ))),
                                    )
                                } else {
                                    sway::Expression::Commented(
                                        format!("{} {} {}", lhs_comment, binary_expr.operator, rhs_value),
                                        Box::new(sway::Expression::Literal(sway::Literal::DecInt(
                                            op(lhs_value, rhs_value.to_u128().unwrap()),
                                            lhs_suffix.or(rhs_suffix),
                                        ))),
                                    )
                                }
                            } else {
                                sway::Expression::Literal(sway::Literal::DecInt(
                                    op(lhs_value, rhs_value.to_u128().unwrap()),
                                    lhs_suffix.or(rhs_suffix),
                                ))
                            }
                        }

                        (
                            sway::Expression::Literal(sway::Literal::HexInt(lhs_value, lhs_suffix)),
                            sway::Expression::Literal(sway::Literal::DecInt(rhs_value, rhs_suffix)),
                        ) => {
                            if let Some(lhs_comment) = lhs_comment {
                                if let Some(rhs_comment) = rhs_comment {
                                    sway::Expression::Commented(
                                        format!("{} {} {}", lhs_comment, binary_expr.operator, rhs_comment),
                                        Box::new(sway::Expression::Literal(sway::Literal::HexInt(
                                            op(lhs_value, rhs_value.to_u128().unwrap()),
                                            lhs_suffix.or(rhs_suffix),
                                        ))),
                                    )
                                } else {
                                    sway::Expression::Commented(
                                        format!("{} {} {}", lhs_comment, binary_expr.operator, rhs_value),
                                        Box::new(sway::Expression::Literal(sway::Literal::HexInt(
                                            op(lhs_value, rhs_value.to_u128().unwrap()),
                                            lhs_suffix.or(rhs_suffix),
                                        ))),
                                    )
                                }
                            } else {
                                sway::Expression::Literal(sway::Literal::HexInt(
                                    op(lhs_value, rhs_value.to_u128().unwrap()),
                                    lhs_suffix.or(rhs_suffix),
                                ))
                            }
                        }

                        _ => todo!(),
                    };
                }

                _ => todo!(),
            };

            match (lhs, rhs) {
                (
                    sway::Expression::Literal(sway::Literal::DecInt(lhs_value, lhs_suffix)),
                    sway::Expression::Literal(sway::Literal::DecInt(rhs_value, rhs_suffix)),
                ) => {
                    if let Some(lhs_comment) = lhs_comment {
                        if let Some(rhs_comment) = rhs_comment {
                            sway::Expression::Commented(
                                format!("{} {} {}", lhs_comment, binary_expr.operator, rhs_comment),
                                Box::new(sway::Expression::Literal(sway::Literal::DecInt(
                                    op(lhs_value, rhs_value),
                                    lhs_suffix.or(rhs_suffix),
                                ))),
                            )
                        } else {
                            sway::Expression::Commented(
                                format!("{} {} {}", lhs_comment, binary_expr.operator, rhs_value),
                                Box::new(sway::Expression::Literal(sway::Literal::DecInt(
                                    op(lhs_value, rhs_value),
                                    lhs_suffix.or(rhs_suffix),
                                ))),
                            )
                        }
                    } else {
                        sway::Expression::Literal(sway::Literal::DecInt(
                            op(lhs_value, rhs_value),
                            lhs_suffix.or(rhs_suffix),
                        ))
                    }
                }

                (
                    sway::Expression::Literal(sway::Literal::HexInt(lhs_value, lhs_suffix)),
                    sway::Expression::Literal(sway::Literal::HexInt(rhs_value, rhs_suffix)),
                ) => {
                    if let Some(lhs_comment) = lhs_comment {
                        if let Some(rhs_comment) = rhs_comment {
                            sway::Expression::Commented(
                                format!("{} {} {}", lhs_comment, binary_expr.operator, rhs_comment),
                                Box::new(sway::Expression::Literal(sway::Literal::HexInt(
                                    op(lhs_value, rhs_value),
                                    lhs_suffix.or(rhs_suffix),
                                ))),
                            )
                        } else {
                            sway::Expression::Commented(
                                format!("{} {} {}", lhs_comment, binary_expr.operator, rhs_value),
                                Box::new(sway::Expression::Literal(sway::Literal::HexInt(
                                    op(lhs_value, rhs_value),
                                    lhs_suffix.or(rhs_suffix),
                                ))),
                            )
                        }
                    } else {
                        sway::Expression::Literal(sway::Literal::HexInt(
                            op(lhs_value, rhs_value),
                            lhs_suffix.or(rhs_suffix),
                        ))
                    }
                }

                (
                    sway::Expression::Literal(sway::Literal::DecInt(lhs_value, lhs_suffix)),
                    sway::Expression::Literal(sway::Literal::HexInt(rhs_value, rhs_suffix)),
                ) => {
                    if let Some(lhs_comment) = lhs_comment {
                        if let Some(rhs_comment) = rhs_comment {
                            sway::Expression::Commented(
                                format!("{} {} {}", lhs_comment, binary_expr.operator, rhs_comment),
                                Box::new(sway::Expression::Literal(sway::Literal::DecInt(
                                    op(lhs_value, rhs_value),
                                    lhs_suffix.or(rhs_suffix),
                                ))),
                            )
                        } else {
                            sway::Expression::Commented(
                                format!("{} {} {}", lhs_comment, binary_expr.operator, rhs_value),
                                Box::new(sway::Expression::Literal(sway::Literal::DecInt(
                                    op(lhs_value, rhs_value),
                                    lhs_suffix.or(rhs_suffix),
                                ))),
                            )
                        }
                    } else {
                        sway::Expression::Literal(sway::Literal::DecInt(
                            op(lhs_value, rhs_value),
                            lhs_suffix.or(rhs_suffix),
                        ))
                    }
                }

                (
                    sway::Expression::Literal(sway::Literal::HexInt(lhs_value, lhs_suffix)),
                    sway::Expression::Literal(sway::Literal::DecInt(rhs_value, rhs_suffix)),
                ) => {
                    if let Some(lhs_comment) = lhs_comment {
                        if let Some(rhs_comment) = rhs_comment {
                            sway::Expression::Commented(
                                format!("{} {} {}", lhs_comment, binary_expr.operator, rhs_comment),
                                Box::new(sway::Expression::Literal(sway::Literal::HexInt(
                                    op(lhs_value, rhs_value),
                                    lhs_suffix.or(rhs_suffix),
                                ))),
                            )
                        } else {
                            sway::Expression::Commented(
                                format!("{} {} {}", lhs_comment, binary_expr.operator, rhs_value),
                                Box::new(sway::Expression::Literal(sway::Literal::HexInt(
                                    op(lhs_value, rhs_value),
                                    lhs_suffix.or(rhs_suffix),
                                ))),
                            )
                        }
                    } else {
                        sway::Expression::Literal(sway::Literal::HexInt(
                            op(lhs_value, rhs_value),
                            lhs_suffix.or(rhs_suffix),
                        ))
                    }
                }

                (lhs, rhs) => todo!("{} {} {}", lhs.display(), binary_expr.operator, rhs.display()),
            }
        }

        sway::Expression::Block(_)
        | sway::Expression::Return(_)
        | sway::Expression::Array(_)
        | sway::Expression::ArrayAccess(_)
        | sway::Expression::MemberAccess(_)
        | sway::Expression::If(_)
        | sway::Expression::Match(_)
        | sway::Expression::While(_)
        | sway::Expression::UnaryExpression(_)
        | sway::Expression::Constructor(_)
        | sway::Expression::Continue
        | sway::Expression::Break
        | sway::Expression::AsmBlock(_) => {
            create_value_expression(project, module, scope.clone(), type_name, Some(expression))
        }

        sway::Expression::Commented(comment, expression) => sway::Expression::Commented(
            comment.clone(),
            Box::new(evaluate_expression(
                project,
                module,
                scope.clone(),
                type_name,
                expression,
            )),
        ),

        sway::Expression::Comment(comment) => sway::Expression::Comment(comment.clone()),
        sway::Expression::Panic(msg) => sway::Expression::Panic(msg.clone()),
    }
}

pub fn translate_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    // use solang_parser::pt::CodeLocation;
    // println!(
    //     "Translating expression: {expression}; from {}",
    //     project.loc_to_file_location_string(module.clone(), &expression.loc()),
    // );

    match expression {
        solidity::Expression::BoolLiteral(_, _)
        | solidity::Expression::NumberLiteral(_, _, _, _)
        | solidity::Expression::RationalNumberLiteral(_, _, _, _, _)
        | solidity::Expression::HexNumberLiteral(_, _, _)
        | solidity::Expression::AddressLiteral(_, _)
        | solidity::Expression::HexLiteral(_)
        | solidity::Expression::StringLiteral(_) => translate_literal_expression(project, expression),

        solidity::Expression::Type(_, _) => translate_type_expression(project, module, scope.clone(), expression),

        solidity::Expression::Variable(_) => translate_variable_expression(project, module, scope.clone(), expression),

        solidity::Expression::ArrayLiteral(_, expressions) => {
            translate_array_literal_expression(project, module, scope.clone(), expressions.as_slice())
        }

        solidity::Expression::ArraySubscript(_, _, _) => {
            translate_array_subscript_expression(project, module, scope.clone(), expression)
        }

        solidity::Expression::ArraySlice(_, _, _, _) => {
            translate_array_slice_expression(project, module, scope.clone(), expression)
        }

        solidity::Expression::List(_, parameters) => {
            translate_list_expression(project, module, scope.clone(), parameters.as_slice())
        }

        solidity::Expression::Parenthesis(_, expression) => {
            translate_parenthesis_expression(project, module, scope.clone(), expression)
        }

        solidity::Expression::MemberAccess(_, container, member) => {
            translate_member_access_expression(project, module, scope.clone(), expression, container, member)
        }

        solidity::Expression::FunctionCall(_, function, arguments) => {
            translate_function_call_expression(project, module, scope.clone(), expression, function, None, arguments)
        }

        solidity::Expression::FunctionCallBlock(_, function, block) => {
            translate_function_call_block_expression(project, module, scope.clone(), function, block)
        }

        solidity::Expression::NamedFunctionCall(_, function, named_arguments) => translate_function_call_expression(
            project,
            module,
            scope.clone(),
            expression,
            function,
            Some(named_arguments),
            &[],
        ),

        solidity::Expression::Not(_, x) => translate_unary_expression(project, module, scope.clone(), "!", x),

        solidity::Expression::BitwiseNot(_, x) => translate_unary_expression(project, module, scope.clone(), "!", x),

        solidity::Expression::UnaryPlus(_, x) => translate_expression(project, module, scope.clone(), x),

        solidity::Expression::Negate(_, x) => translate_unary_expression(project, module, scope.clone(), "-", x),

        solidity::Expression::Power(_, lhs, rhs) => {
            translate_power_expression(project, module, scope.clone(), lhs, rhs)
        }

        solidity::Expression::Multiply(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "*", lhs, rhs)
        }

        solidity::Expression::Divide(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "/", lhs, rhs)
        }

        solidity::Expression::Modulo(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "%", lhs, rhs)
        }

        solidity::Expression::Add(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "+", lhs, rhs)
        }

        solidity::Expression::Subtract(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "-", lhs, rhs)
        }

        solidity::Expression::ShiftLeft(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "<<", lhs, rhs)
        }

        solidity::Expression::ShiftRight(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), ">>", lhs, rhs)
        }

        solidity::Expression::BitwiseAnd(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "&", lhs, rhs)
        }

        solidity::Expression::BitwiseXor(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "^", lhs, rhs)
        }

        solidity::Expression::BitwiseOr(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "|", lhs, rhs)
        }

        solidity::Expression::Less(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "<", lhs, rhs)
        }

        solidity::Expression::More(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), ">", lhs, rhs)
        }

        solidity::Expression::LessEqual(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "<=", lhs, rhs)
        }

        solidity::Expression::MoreEqual(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), ">=", lhs, rhs)
        }

        solidity::Expression::Equal(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "==", lhs, rhs)
        }

        solidity::Expression::NotEqual(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "!=", lhs, rhs)
        }

        solidity::Expression::And(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "&&", lhs, rhs)
        }

        solidity::Expression::Or(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "||", lhs, rhs)
        }

        solidity::Expression::ConditionalOperator(_, condition, then_value, else_value) => {
            translate_conditional_operator_expression(project, module, scope.clone(), condition, then_value, else_value)
        }

        solidity::Expression::Assign(_, lhs, rhs) => {
            translate_assignment_expression(project, module, scope.clone(), "=", lhs.as_ref(), rhs.as_ref())
        }

        solidity::Expression::AssignOr(_, lhs, rhs) => {
            translate_assignment_expression(project, module, scope.clone(), "|=", lhs.as_ref(), rhs.as_ref())
        }

        solidity::Expression::AssignAnd(_, lhs, rhs) => {
            translate_assignment_expression(project, module, scope.clone(), "&=", lhs.as_ref(), rhs.as_ref())
        }

        solidity::Expression::AssignXor(_, lhs, rhs) => {
            translate_assignment_expression(project, module, scope.clone(), "^=", lhs.as_ref(), rhs.as_ref())
        }

        solidity::Expression::AssignShiftLeft(_, lhs, rhs) => {
            translate_assignment_expression(project, module, scope.clone(), "<<=", lhs.as_ref(), rhs.as_ref())
        }

        solidity::Expression::AssignShiftRight(_, lhs, rhs) => {
            translate_assignment_expression(project, module, scope.clone(), ">>=", lhs.as_ref(), rhs.as_ref())
        }

        solidity::Expression::AssignAdd(_, lhs, rhs) => {
            translate_assignment_expression(project, module, scope.clone(), "+=", lhs.as_ref(), rhs.as_ref())
        }

        solidity::Expression::AssignSubtract(_, lhs, rhs) => {
            translate_assignment_expression(project, module, scope.clone(), "-=", lhs.as_ref(), rhs.as_ref())
        }

        solidity::Expression::AssignMultiply(_, lhs, rhs) => {
            translate_assignment_expression(project, module, scope.clone(), "*=", lhs.as_ref(), rhs.as_ref())
        }

        solidity::Expression::AssignDivide(_, lhs, rhs) => {
            translate_assignment_expression(project, module, scope.clone(), "/=", lhs.as_ref(), rhs.as_ref())
        }

        solidity::Expression::AssignModulo(_, lhs, rhs) => {
            translate_assignment_expression(project, module, scope.clone(), "%=", lhs.as_ref(), rhs.as_ref())
        }

        solidity::Expression::PreIncrement(_, _)
        | solidity::Expression::PostIncrement(_, _)
        | solidity::Expression::PreDecrement(_, _)
        | solidity::Expression::PostDecrement(_, _) => {
            translate_pre_or_post_operator_value_expression(project, module, scope.clone(), expression)
        }

        solidity::Expression::New(_, expression) => {
            translate_new_expression(project, module, scope.clone(), expression)
        }

        solidity::Expression::Delete(_, expression) => {
            translate_delete_expression(project, module, scope.clone(), expression)
        }
    }
}

pub fn create_value_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    type_name: &sway::TypeName,
    value: Option<&sway::Expression>,
) -> sway::Expression {
    if let Some(value) = value {
        let value_type = get_expression_type(project, module.clone(), scope.clone(), value).unwrap();

        let Some(result) = coerce_expression(project, module.clone(), scope.clone(), value, &value_type, type_name)
        else {
            panic!(
                "Failed to coerce from `{}` to `{}`: `{}`",
                value_type,
                type_name,
                sway::TabbedDisplayer(value)
            );
        };

        return result;
    }

    match type_name {
        sway::TypeName::Undefined => panic!("Undefined type name"),

        sway::TypeName::Identifier {
            name,
            generic_parameters,
        } => match (name.as_str(), generic_parameters.as_ref()) {
            ("todo!", None) => sway::Expression::create_function_call("todo!", None, vec![]),

            ("bool", None) => sway::Expression::Literal(sway::Literal::Bool(false)),

            ("b256", None) => sway::Expression::create_function_call("b256::zero", None, vec![]),

            ("I8" | "I16" | "I32" | "I64" | "I128" | "I256", None) => {
                let value = match name.as_str() {
                    "I128" => {
                        module.borrow_mut().ensure_use_declared("std::u128::*");

                        sway::Expression::create_function_call("U128::zero", None, vec![])
                    }

                    _ => sway::Expression::from(sway::Literal::DecInt(
                        BigUint::zero(),
                        Some(format!("u{}", name.trim_start_matches('I'))),
                    )),
                };

                let value_type = get_expression_type(project, module.clone(), scope.clone(), &value).unwrap();

                coerce_expression(project, module.clone(), scope.clone(), &value, &value_type, type_name).unwrap()
            }

            ("u8" | "u16" | "u32" | "u64" | "u256", None) => {
                sway::Expression::Literal(sway::Literal::DecInt(BigUint::zero(), None))
            }

            ("raw_ptr", None) => {
                // Ensure `std::alloc::alloc_bytes` is imported
                module.borrow_mut().ensure_use_declared("std::alloc::alloc_bytes");

                // alloc_bytes(0)
                sway::Expression::create_function_call(
                    "alloc_bytes",
                    None,
                    vec![sway::Expression::from(sway::Literal::DecInt(0u8.into(), None))],
                )
            }

            ("Bytes", None) => {
                // Ensure `std::bytes::Bytes` is imported
                module.borrow_mut().ensure_use_declared("std::bytes::Bytes");

                sway::Expression::create_function_call("Bytes::new", None, vec![])
            }

            ("Identity", None) => sway::Expression::create_function_call(
                "Identity::Address",
                None,
                vec![sway::Expression::create_function_call(
                    "Address::from",
                    None,
                    vec![sway::Expression::create_function_call("b256::zero", None, vec![])],
                )],
            ),

            ("Option", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                sway::Expression::create_identifier("None".into())
            }

            ("StorageKey", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                sway::Expression::create_todo(Some("create storage key".into()))
            }

            ("StorageMap", Some(_)) => sway::Expression::from(sway::Constructor {
                type_name: sway::TypeName::create_identifier("StorageMap"),
                fields: vec![],
            }),

            ("StorageString", None) => sway::Expression::from(sway::Constructor {
                type_name: sway::TypeName::create_identifier("StorageString"),
                fields: vec![],
            }),

            ("StorageVec", Some(_)) => sway::Expression::from(sway::Constructor {
                type_name: sway::TypeName::create_identifier("StorageVec"),
                fields: vec![],
            }),

            ("String", None) => sway::Expression::create_function_call("String::new", None, vec![]),

            ("Vec", Some(_)) => sway::Expression::create_function_call("Vec::new", None, vec![]),

            (name, _) => {
                // Check to see if the type is a type definition
                if project.is_type_definition_declared(module.clone(), name) {
                    let underlying_type = get_underlying_type(project, module.clone(), type_name);

                    return create_value_expression(project, module, scope.clone(), &underlying_type, None);
                }
                // Check to see if the type is a translated enum
                else if let Some(translated_enum) = project.find_enum(module.clone(), name) {
                    if let Some(value) = translated_enum.constants.first() {
                        return sway::Expression::create_identifier(&value.name);
                    }

                    let underlying_type = get_underlying_type(project, module.clone(), type_name);

                    return create_value_expression(project, module.clone(), scope.clone(), &underlying_type, None);
                }
                // Check to see if the type is a struct definition
                else if let Some(struct_definition) = project.find_struct(module.clone(), scope.clone(), name) {
                    let struct_definition = struct_definition.borrow();

                    let fields = if struct_definition.memory.name == *name {
                        struct_definition.memory.fields.as_slice()
                    } else if struct_definition.storage.name == *name {
                        struct_definition.storage.fields.as_slice()
                    } else {
                        todo!()
                    };

                    return sway::Expression::from(sway::Constructor {
                        type_name: sway::TypeName::create_identifier(name),
                        fields: fields
                            .iter()
                            .map(|f| sway::ConstructorField {
                                name: f.new_name.clone(),
                                value: create_value_expression(
                                    project,
                                    module.clone(),
                                    scope.clone(),
                                    &f.type_name,
                                    None,
                                ),
                            })
                            .collect(),
                    });
                }

                panic!("Unknown value type: {type_name}")
            }
        },

        sway::TypeName::Array { type_name, length } => sway::Expression::Array(sway::Array {
            elements: (0..*length)
                .map(|_| create_value_expression(project, module.clone(), scope.clone(), type_name, None))
                .collect(),
        }),

        sway::TypeName::Tuple { type_names } => sway::Expression::Tuple(
            type_names
                .iter()
                .map(|type_name| create_value_expression(project, module.clone(), scope.clone(), type_name, None))
                .collect(),
        ),

        sway::TypeName::StringSlice => sway::Expression::from(sway::Literal::String(String::new())),

        sway::TypeName::StringArray { length } => sway::Expression::create_function_call(
            "__to_str_array",
            None,
            vec![sway::Expression::create_string_literal(
                &(0..*length).map(|_| " ").collect::<String>(),
            )],
        ),

        sway::TypeName::Function { .. } => sway::Expression::create_todo(Some(type_name.to_string())),

        sway::TypeName::Abi { .. } => sway::Expression::create_function_call(
            "Identity::Address",
            None,
            vec![sway::Expression::create_function_call(
                "Address::from",
                None,
                vec![sway::Expression::create_function_call("b256::zero", None, vec![])],
            )],
        ),
    }
}

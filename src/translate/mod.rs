use crate::{error::Error, ir, project::Project, sway};
use convert_case::{Case, Casing};
use num_bigint::BigUint;
use num_traits::{One, Zero};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

mod assembly;
mod contracts;
mod enums;
mod expressions;
mod functions;
mod import_directives;
mod statements;
mod storage;
mod structs;
mod symbols;
mod types;
pub use assembly::*;
pub use contracts::*;
pub use enums::*;
pub use expressions::*;
pub use functions::*;
pub use import_directives::*;
pub use statements::*;
pub use storage::*;
pub use structs::*;
pub use symbols::*;
pub use types::*;

#[inline]
pub fn translate_naming_convention(name: &str, case: Case) -> String {
    // HACK: do not allow dollar signs
    let mut name = name.replace('$', "dollar_sign").to_string();

    // HACK: do not allow name to start with double underscore
    while name.starts_with("__") {
        name = name[2..].to_string();
    }

    let name = if name.chars().all(|c| c == '_') {
        name.to_string()
    } else {
        let prefix = name.chars().take_while(|c| *c == '_').collect::<String>();
        let postfix = name
            .chars()
            .rev()
            .take_while(|c| *c == '_')
            .collect::<String>();
        format!("{prefix}{}{postfix}", name.to_case(case))
    };

    match name.as_str() {
        "self" => "this".into(),
        _ => name,
    }
}

/// Coerces an expression from one type to another
pub fn coerce_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &sway::Expression,
    from_type_name: &sway::TypeName,
    to_type_name: &sway::TypeName,
) -> Option<sway::Expression> {
    if from_type_name.is_compatible_with(to_type_name) {
        return Some(expression.clone());
    }

    // println!(
    //     "Coercing from `{from_type_name}` to `{to_type_name}`: {}",
    //     sway::TabbedDisplayer(expression)
    // );

    let b256_type_name = sway::TypeName::Identifier {
        name: "b256".into(),
        generic_parameters: None,
    };

    let u256_type_name = sway::TypeName::Identifier {
        name: "u256".into(),
        generic_parameters: None,
    };

    let mut expression = expression.clone();
    let mut from_type_name = from_type_name.clone();
    let mut add_read_member_call = false;

    // HACK: If the expression is reading from a `StorageKey<T>`, remove the `.read()` temporarily
    if let sway::Expression::FunctionCall(f) = &expression {
        if let sway::Expression::MemberAccess(m) = &f.function {
            if m.member == "read" && f.parameters.is_empty() {
                let container_type =
                    get_expression_type(project, module.clone(), scope.clone(), &m.expression)
                        .unwrap();

                if container_type.is_storage_key() {
                    expression = m.expression.clone();
                    from_type_name = container_type;
                    add_read_member_call = true;
                }
            }
        }
    }

    // Check for `StorageKey<StorageString>` to `Bytes` coercions
    if let Some(storage_key_type) = from_type_name.storage_key_type() {
        if storage_key_type.is_storage_string() && to_type_name.is_bytes() {
            return Some(sway::Expression::create_function_calls(
                Some(expression),
                &[
                    ("read_slice", Some((None, vec![]))),
                    ("unwrap", Some((None, vec![]))),
                    ("as_bytes", Some((None, vec![]))),
                ],
            ));
        }
    }

    // Check for `StorageKey<StorageVec<T>>` to `Vec<T>` coercions
    if let Some(storage_key_type) = from_type_name.storage_key_type() {
        if let Some(storage_vec_type) = storage_key_type.storage_vec_type() {
            if let Some(vec_type) = to_type_name.vec_type() {
                let get_expression = sway::Expression::create_function_calls(
                    Some(expression.clone()),
                    &[
                        (
                            "get",
                            Some((
                                None,
                                vec![sway::Expression::create_identifier("i".to_string())],
                            )),
                        ),
                        ("unwrap", Some((None, vec![]))),
                        ("read", Some((None, vec![]))),
                    ],
                );

                let element_expression = coerce_expression(
                    project,
                    module.clone(),
                    scope.clone(),
                    &get_expression,
                    &storage_vec_type,
                    &vec_type,
                )
                .unwrap();

                return Some(sway::Expression::from(sway::Block {
                    statements: vec![
                        sway::Statement::from(sway::Let {
                            pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                is_mutable: false,
                                name: "len".to_string(),
                            }),
                            type_name: None,
                            value: sway::Expression::create_function_calls(
                                Some(expression.clone()),
                                &[("len", Some((None, vec![])))],
                            ),
                        }),
                        sway::Statement::from(sway::Let {
                            pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                is_mutable: true,
                                name: "v".to_string(),
                            }),
                            type_name: None,
                            value: sway::Expression::create_function_calls(
                                None,
                                &[(
                                    "Vec::with_capacity",
                                    Some((
                                        None,
                                        vec![sway::Expression::create_identifier(
                                            "len".to_string(),
                                        )],
                                    )),
                                )],
                            ),
                        }),
                        sway::Statement::from(sway::Let {
                            pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                is_mutable: true,
                                name: "i".to_string(),
                            }),
                            type_name: None,
                            value: sway::Expression::from(sway::Literal::DecInt(
                                BigUint::zero(),
                                None,
                            )),
                        }),
                        sway::Statement::from(sway::Expression::from(sway::While {
                            condition: sway::Expression::from(sway::BinaryExpression {
                                operator: "<".to_string(),
                                lhs: sway::Expression::create_identifier("i".to_string()),
                                rhs: sway::Expression::create_identifier("len".to_string()),
                            }),
                            body: sway::Block {
                                statements: vec![
                                    sway::Statement::from(sway::Expression::create_function_calls(
                                        None,
                                        &[
                                            ("v", None),
                                            (
                                                "push",
                                                Some((None, vec![element_expression.clone()])),
                                            ),
                                        ],
                                    )),
                                    sway::Statement::from(sway::Expression::from(
                                        sway::BinaryExpression {
                                            operator: "+=".to_string(),
                                            lhs: sway::Expression::create_identifier(
                                                "i".to_string(),
                                            ),
                                            rhs: sway::Expression::from(sway::Literal::DecInt(
                                                BigUint::one(),
                                                None,
                                            )),
                                        },
                                    )),
                                ],
                                final_expr: None,
                            },
                        })),
                    ],
                    final_expr: Some(sway::Expression::create_identifier("v".to_string())),
                }));
            }
        }
    }

    // HACK: Restore the `.read()` if we removed it
    if add_read_member_call {
        expression = sway::Expression::create_function_calls(
            Some(expression),
            &[("read", Some((None, vec![])))],
        );

        from_type_name =
            get_expression_type(project, module.clone(), scope.clone(), &expression).unwrap();
    }

    // Check for `StorageKey<T>` to `T` coercions
    if let Some(storage_key_type) = from_type_name.storage_key_type() {
        if to_type_name.is_compatible_with(&storage_key_type) {
            return Some(sway::Expression::create_function_calls(
                Some(expression),
                &[("read", Some((None, vec![])))],
            ));
        }
    }

    // Check for `T` to `StorageKey<T>` coercions
    if let Some(storage_key_type) = to_type_name.storage_key_type() {
        if storage_key_type.is_compatible_with(&from_type_name) {
            if let sway::Expression::FunctionCall(f) = &expression {
                if let sway::Expression::MemberAccess(m) = &f.function {
                    if m.member == "read" && f.parameters.len() == 0 {
                        return Some(m.expression.clone());
                    }
                }
            }
        }
    }

    // Check for `Identity` to `b256` coercions
    if from_type_name.is_identity() && to_type_name.is_b256() {
        return Some(sway::Expression::create_function_calls(
            Some(expression),
            &[("bits", Some((None, vec![])))],
        ));
    }

    // Check for `Identity` to `u256` coercions
    if from_type_name.is_identity() && to_type_name.is_u256() {
        return Some(sway::Expression::create_function_calls(
            Some(expression),
            &[
                ("bits", Some((None, vec![]))),
                ("as_u256", Some((None, vec![]))),
            ],
        ));
    }

    // From uint to Identity
    if (from_type_name.is_uint() || from_type_name.is_b256()) && to_type_name.is_identity() {
        if from_type_name.is_uint() {
            expression = coerce_expression(
                project,
                module.clone(),
                scope.clone(),
                &expression,
                &from_type_name,
                &b256_type_name,
            )
            .unwrap();
        }

        return Some(sway::Expression::create_function_calls(
            None,
            &[(
                "Identity::Address",
                Some((
                    None,
                    vec![sway::Expression::create_function_calls(
                        None,
                        &[("Address::from", Some((None, vec![expression])))],
                    )],
                )),
            )],
        ));
    }

    // Check for abi cast to `Identity` coercions
    if to_type_name.is_identity() {
        let mut comment = None;
        let mut expression = expression.clone();

        if let sway::Expression::Commented(c, e) = &expression {
            comment = Some(c.clone());
            expression = e.as_ref().clone();
        }

        if let sway::Expression::FunctionCall(f) = &expression {
            if let Some("abi") = f.function.as_identifier() {
                if f.parameters.len() == 2 {
                    return Some(
                        coerce_expression(
                            project,
                            module.clone(),
                            scope.clone(),
                            &if let Some(comment) = comment {
                                sway::Expression::Commented(
                                    comment,
                                    Box::new(f.parameters[1].clone()),
                                )
                            } else {
                                f.parameters[1].clone()
                            },
                            &b256_type_name,
                            to_type_name,
                        )
                        .unwrap(),
                    );
                }
            }
        }
    }

    // Check for `Identity` to abi cast coercions
    if from_type_name.is_identity() {
        if project
            .find_contract(module.clone(), to_type_name.to_string().as_str())
            .is_some()
        {
            return Some(sway::Expression::create_function_calls(
                None,
                &[(
                    "abi",
                    Some((
                        None,
                        vec![
                            sway::Expression::create_identifier(to_type_name.to_string()),
                            sway::Expression::create_function_calls(
                                Some(expression),
                                &[
                                    ("as_contract_id", Some((None, vec![]))),
                                    ("unwrap", Some((None, vec![]))),
                                    ("into", Some((None, vec![]))),
                                ],
                            ),
                        ],
                    )),
                )],
            ));
        }
    }

    // Check for `ContractId` to `Identity` coercions
    if from_type_name.is_contract_id() && to_type_name.is_identity() {
        return Some(sway::Expression::create_function_calls(
            None,
            &[("Identity::ContractId", Some((None, vec![expression])))],
        ));
    }

    // Check for uint to int coercions
    if from_type_name.is_uint() && to_type_name.is_int() {
        let lhs_bits: usize = from_type_name
            .to_string()
            .trim_start_matches('u')
            .trim_start_matches('U')
            .trim_start_matches('I')
            .parse()
            .unwrap();

        let rhs_bits: usize = to_type_name
            .to_string()
            .trim_start_matches('u')
            .trim_start_matches('U')
            .trim_start_matches('I')
            .parse()
            .unwrap();

        if lhs_bits > rhs_bits {
            expression = sway::Expression::create_function_calls(
                None,
                &[
                    (
                        format!("u{rhs_bits}::try_from").as_str(),
                        Some((None, vec![expression.clone()])),
                    ),
                    ("unwrap", Some((None, vec![]))),
                ],
            );
        } else if lhs_bits < rhs_bits {
            expression = sway::Expression::create_function_calls(
                Some(expression.clone()),
                &[(format!("as_u{rhs_bits}").as_str(), Some((None, vec![])))],
            );
        }

        return Some(sway::Expression::create_function_calls(
            None,
            &[(
                format!("I{rhs_bits}::from_uint").as_str(),
                Some((None, vec![expression.clone()])),
            )],
        ));
    }

    // Check for int to uint coercions
    if from_type_name.is_int() && to_type_name.is_uint() {
        let lhs_bits: usize = from_type_name
            .to_string()
            .trim_start_matches('u')
            .trim_start_matches('U')
            .trim_start_matches('I')
            .parse()
            .unwrap();

        let rhs_bits: usize = to_type_name
            .to_string()
            .trim_start_matches('u')
            .trim_start_matches('U')
            .trim_start_matches('I')
            .parse()
            .unwrap();

        if lhs_bits > rhs_bits {
            expression = sway::Expression::create_function_calls(
                None,
                &[
                    (
                        format!("u{rhs_bits}::try_from").as_str(),
                        Some((
                            None,
                            vec![sway::Expression::from(sway::MemberAccess {
                                expression: expression.clone(),
                                member: "underlying".to_string(),
                            })],
                        )),
                    ),
                    ("unwrap", Some((None, vec![]))),
                ],
            );
        } else if lhs_bits < rhs_bits {
            expression = sway::Expression::create_function_calls(
                Some(expression.clone()),
                &[
                    ("underlying", None),
                    (format!("as_u{rhs_bits}").as_str(), Some((None, vec![]))),
                ],
            );
        }

        return Some(sway::Expression::create_function_calls(
            None,
            &[(
                format!("I{rhs_bits}::from_uint").as_str(),
                Some((None, vec![expression.clone()])),
            )],
        ));
    }

    // Check for uint/int coercions of different bit lengths
    if (from_type_name.is_uint() && to_type_name.is_uint())
        || (from_type_name.is_int() && to_type_name.is_int())
    {
        let lhs_bits: usize = from_type_name
            .to_string()
            .trim_start_matches('u')
            .trim_start_matches('U')
            .trim_start_matches('I')
            .parse()
            .unwrap();

        let rhs_bits: usize = to_type_name
            .to_string()
            .trim_start_matches('u')
            .trim_start_matches('U')
            .trim_start_matches('I')
            .parse()
            .unwrap();

        match &expression {
            sway::Expression::Literal(sway::Literal::DecInt(i, suffix)) => {
                if suffix.is_none() {
                    return Some(sway::Expression::Literal(sway::Literal::DecInt(
                        i.clone(),
                        Some(format!(
                            "{}{}",
                            to_type_name.to_string().chars().nth(0).unwrap(),
                            rhs_bits
                        )),
                    )));
                }

                if lhs_bits > rhs_bits {
                    // x.as_u256()
                    // u64::try_from(x).unwrap()
                    return Some(sway::Expression::create_function_calls(
                        None,
                        &[
                            (
                                format!("{to_type_name}::try_from").as_str(),
                                Some((None, vec![expression.clone()])),
                            ),
                            ("unwrap", Some((None, vec![]))),
                        ],
                    ));
                }

                if lhs_bits < rhs_bits {
                    return Some(sway::Expression::create_function_calls(
                        Some(expression.clone()),
                        &[(format!("as_{to_type_name}").as_str(), Some((None, vec![])))],
                    ));
                }
            }

            _ => {
                if lhs_bits > rhs_bits {
                    // x.as_u256()
                    // u64::try_from(x).unwrap()
                    return Some(sway::Expression::create_function_calls(
                        None,
                        &[
                            (
                                format!("{to_type_name}::try_from").as_str(),
                                Some((None, vec![expression.clone()])),
                            ),
                            ("unwrap", Some((None, vec![]))),
                        ],
                    ));
                }

                if lhs_bits < rhs_bits {
                    return Some(sway::Expression::create_function_calls(
                        Some(expression.clone()),
                        &[(format!("as_{to_type_name}").as_str(), Some((None, vec![])))],
                    ));
                }
            }
        }
    }

    // Check for uint to `b256` coercions
    if from_type_name.is_uint() && to_type_name.is_b256() {
        expression = coerce_expression(
            project,
            module.clone(),
            scope.clone(),
            &expression,
            &from_type_name,
            &u256_type_name,
        )
        .unwrap();

        return Some(sway::Expression::create_function_calls(
            Some(expression),
            &[("as_b256", Some((None, vec![])))],
        ));
    }

    // Check for b256 to `u256` coercions
    if from_type_name.is_b256() && to_type_name.is_u256() {
        return Some(sway::Expression::create_function_calls(
            Some(expression),
            &[("as_u256", Some((None, vec![])))],
        ));
    }

    // Check for uint to `Bytes` coercions
    if (from_type_name.is_uint() || from_type_name.is_b256()) && to_type_name.is_bytes() {
        if from_type_name.is_u8() {
            todo!()
        }

        return Some(sway::Expression::create_function_calls(
            Some(expression),
            &[("to_be_bytes", Some((None, vec![])))],
        ));
    }

    // Check for `String` to `Bytes` coercions
    if from_type_name.is_string() && to_type_name.is_bytes() {
        return Some(sway::Expression::create_function_calls(
            Some(expression),
            &[("as_bytes", Some((None, vec![])))],
        ));
    }

    // Check for byte array coercions
    if let Some(to_byte_count) = to_type_name.u8_array_length() {
        let mut from_bits = 0;

        if let Some(bits) = from_type_name.uint_bits() {
            from_bits = bits;
        } else if from_type_name.is_b256() {
            from_bits = 32;
        }

        // Check for uint to byte array coersions
        if from_bits != 0 {
            let from_byte_count = from_bits / 8;
            let to_bits = to_byte_count * 8;

            if from_byte_count == to_byte_count {
                return Some(sway::Expression::create_function_calls(
                    Some(expression),
                    &[("to_be_bytes", Some((None, vec![])))],
                ));
            } else if from_byte_count < to_byte_count {
                return Some(sway::Expression::create_function_calls(
                    Some(expression),
                    &[
                        (format!("as_u{to_bits}").as_str(), Some((None, vec![]))),
                        ("to_be_bytes", Some((None, vec![]))),
                    ],
                ));
            } else if from_byte_count > to_byte_count {
                if to_byte_count == 1 {
                    return Some(sway::Expression::Array(sway::Array {
                        elements: vec![sway::Expression::create_function_calls(
                            Some(expression),
                            &[
                                (format!("try_as_u{to_bits}").as_str(), Some((None, vec![]))),
                                ("unwrap", Some((None, vec![]))),
                            ],
                        )],
                    }));
                }

                return Some(sway::Expression::create_function_calls(
                    Some(expression),
                    &[
                        (format!("try_as_u{to_bits}").as_str(), Some((None, vec![]))),
                        ("unwrap", Some((None, vec![]))),
                        ("to_be_bytes", Some((None, vec![]))),
                    ],
                ));
            }
        }

        // Check for `Identity` to `[u8; N]` coersions
        if from_type_name.is_identity() && to_byte_count <= 32 {
            return Some(sway::Expression::from(sway::Block {
                statements: vec![sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                        is_mutable: false,
                        name: "x".into(),
                    }),
                    type_name: None,
                    value: sway::Expression::from(sway::Match {
                        expression,
                        branches: vec![
                            sway::MatchBranch {
                                pattern: sway::Expression::create_function_calls(
                                    None,
                                    &[(
                                        "Identity::Address",
                                        Some((
                                            None,
                                            vec![sway::Expression::create_identifier("x".into())],
                                        )),
                                    )],
                                ),
                                value: sway::Expression::create_function_calls(
                                    None,
                                    &[(
                                        "Bytes::from",
                                        Some((
                                            None,
                                            vec![sway::Expression::create_identifier("x".into())],
                                        )),
                                    )],
                                ),
                            },
                            sway::MatchBranch {
                                pattern: sway::Expression::create_function_calls(
                                    None,
                                    &[(
                                        "Identity::ContractId",
                                        Some((
                                            None,
                                            vec![sway::Expression::create_identifier("x".into())],
                                        )),
                                    )],
                                ),
                                value: sway::Expression::create_function_calls(
                                    None,
                                    &[(
                                        "Bytes::from",
                                        Some((
                                            None,
                                            vec![sway::Expression::create_identifier("x".into())],
                                        )),
                                    )],
                                ),
                            },
                        ],
                    }),
                })],
                final_expr: Some(sway::Expression::from(sway::Array {
                    elements: (0..to_byte_count)
                        .map(|i| {
                            sway::Expression::create_function_calls(
                                None,
                                &[
                                    ("x", None),
                                    (
                                        "get",
                                        Some((
                                            None,
                                            vec![sway::Expression::from(sway::Literal::DecInt(
                                                i.into(),
                                                None,
                                            ))],
                                        )),
                                    ),
                                    ("unwrap", Some((None, vec![]))),
                                ],
                            )
                        })
                        .collect(),
                })),
            }));
        }
    }

    // Check for `String` to `str` coercions
    if from_type_name.is_string() && to_type_name.is_string_slice() {
        return Some(sway::Expression::create_function_calls(
            Some(expression),
            &[("as_str", Some((None, vec![])))],
        ));
    }

    // Check for `str` to `String` coercions
    if from_type_name.is_string_slice() && to_type_name.is_string() {
        // String::from_ascii_str(x)
        return Some(sway::Expression::create_function_calls(
            None,
            &[(
                "String::from_ascii_str",
                Some((None, vec![expression.clone()])),
            )],
        ));
    }

    // Check for `str[]` to `String` coercions
    if from_type_name.is_string_array() && to_type_name.is_string() {
        // String::from_ascii_str(from_str_array(x))
        return Some(sway::Expression::create_function_calls(
            None,
            &[(
                "String::from_ascii_str",
                Some((
                    None,
                    vec![sway::Expression::create_function_calls(
                        None,
                        &[("from_str_array", Some((None, vec![expression.clone()])))],
                    )],
                )),
            )],
        ));
    }

    // Check for `str` to `Bytes` coercions
    if from_type_name.is_string_slice() && to_type_name.is_bytes() {
        // Bytes::from(raw_slice::from_parts::<u8>((s.as_ptr(), s.len())))
        return Some(sway::Expression::create_function_calls(
            None,
            &[(
                "Bytes::from",
                Some((
                    None,
                    vec![sway::Expression::create_function_calls(
                        None,
                        &[(
                            "raw_slice::from_parts",
                            Some((
                                Some(sway::GenericParameterList {
                                    entries: vec![sway::GenericParameter {
                                        type_name: sway::TypeName::Identifier {
                                            name: "u8".into(),
                                            generic_parameters: None,
                                        },
                                        implements: None,
                                    }],
                                }),
                                vec![
                                    sway::Expression::create_function_calls(
                                        Some(expression.clone()),
                                        &[("as_ptr", Some((None, vec![])))],
                                    ),
                                    sway::Expression::create_function_calls(
                                        Some(expression.clone()),
                                        &[("len", Some((None, vec![])))],
                                    ),
                                ],
                            )),
                        )],
                    )],
                )),
            )],
        ));
    }

    // Check for `[u8; 32]` to `b256` coercions
    if let Some(length) = from_type_name.u8_array_length() {
        if length == 32 && to_type_name.is_b256() {
            return Some(sway::Expression::create_function_calls(
                None,
                &[("b256::from_be_bytes", Some((None, vec![expression])))],
            ));
        }
    }

    // Check for array to array coercions
    if let (Some((lhs_type_name, lhs_len)), Some((rhs_type_name, rhs_len))) =
        (from_type_name.array_info(), to_type_name.array_info())
    {
        if lhs_len == rhs_len {
            if let sway::Expression::Array(array) = &expression {
                return Some(sway::Expression::from(sway::Array {
                    elements: array
                        .elements
                        .iter()
                        .map(|e| {
                            coerce_expression(
                                project,
                                module.clone(),
                                scope.clone(),
                                e,
                                &lhs_type_name,
                                &rhs_type_name,
                            )
                            .unwrap()
                        })
                        .collect(),
                }));
            }

            return Some(sway::Expression::from(sway::Array {
                elements: (0..rhs_len)
                    .map(|i| {
                        coerce_expression(
                            project,
                            module.clone(),
                            scope.clone(),
                            &sway::Expression::from(sway::ArrayAccess {
                                expression: expression.clone(),
                                index: sway::Expression::from(sway::Literal::DecInt(
                                    i.into(),
                                    None,
                                )),
                            }),
                            &lhs_type_name,
                            &rhs_type_name,
                        )
                        .unwrap()
                    })
                    .collect(),
            }));
        }
    }

    // Check for tuple to tuple coercions
    if let (Some(lhs_type_names), Some(rhs_type_names)) = (
        from_type_name.tuple_type_names(),
        to_type_name.tuple_type_names(),
    ) {
        match &expression {
            sway::Expression::PathExpr(path_expr) if path_expr.is_identifier() => {
                let component_names = ('a'..='z')
                    .enumerate()
                    .take_while(|(i, _)| *i < lhs_type_names.len())
                    .map(|(_, c)| sway::LetIdentifier {
                        is_mutable: false,
                        name: c.to_string(),
                    })
                    .collect::<Vec<_>>();

                let let_stmt = sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::Tuple(component_names.clone()),
                    type_name: None,
                    value: expression.clone(),
                });

                let exprs = component_names
                    .iter()
                    .enumerate()
                    .map(|(i, c)| {
                        let expr = sway::Expression::create_identifier(c.name.clone());
                        coerce_expression(
                            project,
                            module.clone(),
                            scope.clone(),
                            &expr,
                            &lhs_type_names[i],
                            &rhs_type_names[i],
                        )
                    })
                    .collect::<Vec<_>>();

                if exprs.iter().any(|x| x.is_none()) {
                    return None;
                }

                return Some(sway::Expression::from(sway::Block {
                    statements: vec![let_stmt],
                    final_expr: Some(sway::Expression::Tuple(
                        exprs.iter().flatten().cloned().collect(),
                    )),
                }));
            }

            sway::Expression::Tuple(expressions) => {
                let mut expressions = expressions.clone();

                if expressions.len() != rhs_type_names.len() {
                    return None;
                }

                for (i, (lhs, rhs)) in lhs_type_names.iter().zip(rhs_type_names.iter()).enumerate()
                {
                    match coerce_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        &expressions[i],
                        lhs,
                        rhs,
                    ) {
                        Some(expr) => expressions[i] = expr,
                        None => return None,
                    }
                }

                return Some(sway::Expression::Tuple(expressions));
            }

            _ => {}
        }
    }

    None
}

/// Gets the base underlying type of the supplied type name
pub fn get_underlying_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    type_name: &sway::TypeName,
) -> sway::TypeName {
    // Check to see if the expression's type is a type definition and get the underlying type
    if let Some(type_definition) =
        project.find_type_definition(module.clone(), type_name.to_string().as_str())
    {
        return get_underlying_type(
            project,
            module.clone(),
            type_definition.underlying_type.as_ref().unwrap(),
        );
    }

    // If we didn't find a type definition, check to see if an enum exists and get its underlying type
    if let Some(enum_definition) = project.find_enum(module.clone(), type_name.to_string().as_str())
    {
        return get_underlying_type(
            project,
            module.clone(),
            enum_definition
                .type_definition
                .underlying_type
                .as_ref()
                .unwrap(),
        );
    }

    type_name.clone()
}

#[inline]
pub fn get_return_type_name(
    _project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    type_name: &sway::TypeName,
) -> sway::TypeName {
    match type_name {
        sway::TypeName::StringSlice => {
            // Ensure `std::string::*` is imported
            module.borrow_mut().ensure_use_declared("std::string::*");

            sway::TypeName::Identifier {
                name: "String".into(),
                generic_parameters: None,
            }
        }

        _ => type_name.clone(),
    }
}

/// Attempts to get the type of the supplied expression.
pub fn get_expression_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &sway::Expression,
) -> Result<sway::TypeName, Error> {
    match expression {
        sway::Expression::Literal(literal) => Ok(get_literal_type(literal)),
        sway::Expression::PathExpr(path_expr) => Ok(get_path_expr_type(
            project,
            module.clone(),
            scope.clone(),
            path_expr,
        )),
        sway::Expression::FunctionCall(_) | sway::Expression::FunctionCallBlock(_) => {
            get_function_call_type(project, module.clone(), scope.clone(), expression)
        }
        sway::Expression::Block(block) => {
            get_block_type(project, module.clone(), scope.clone(), block)
        }
        sway::Expression::Return(value) => {
            get_return_type(project, module.clone(), scope.clone(), value.as_deref())
        }
        sway::Expression::Array(array) => {
            get_array_type(project, module.clone(), scope.clone(), array)
        }
        sway::Expression::ArrayAccess(array_access) => {
            get_array_access_type(project, module.clone(), scope.clone(), array_access)
        }
        sway::Expression::MemberAccess(member_access) => get_member_access_type(
            project,
            module.clone(),
            scope.clone(),
            member_access,
            expression,
        ),
        sway::Expression::Tuple(tuple) => {
            get_tuple_type(project, module.clone(), scope.clone(), tuple)
        }
        sway::Expression::If(if_expr) => {
            get_if_type(project, module.clone(), scope.clone(), if_expr)
        }
        sway::Expression::Match(match_expr) => {
            get_match_type(project, module.clone(), scope.clone(), match_expr)
        }
        sway::Expression::While(_) => Ok(sway::TypeName::Tuple { type_names: vec![] }),
        sway::Expression::UnaryExpression(unary_expression) => {
            get_unary_expression_type(project, module.clone(), scope.clone(), unary_expression)
        }
        sway::Expression::BinaryExpression(binary_expression) => {
            get_binary_expression_type(project, module.clone(), scope.clone(), binary_expression)
        }
        sway::Expression::Constructor(constructor) => Ok(constructor.type_name.clone()),
        sway::Expression::Continue => Ok(sway::TypeName::Tuple { type_names: vec![] }),
        sway::Expression::Break => Ok(sway::TypeName::Tuple { type_names: vec![] }),
        sway::Expression::AsmBlock(asm_block) => get_asm_block_type(asm_block),
        sway::Expression::Commented(_, x) => {
            get_expression_type(project, module.clone(), scope.clone(), x)
        }
    }
}

#[inline(always)]
fn get_literal_type(literal: &sway::Literal) -> sway::TypeName {
    match literal {
        sway::Literal::Bool(_) => sway::TypeName::Identifier {
            name: "bool".into(),
            generic_parameters: None,
        },

        sway::Literal::DecInt(value, suffix) => sway::TypeName::Identifier {
            name: if let Some(suffix) = suffix.as_ref() {
                suffix.clone()
            } else {
                let mut bits = value.bits();
                let remainder = bits % 8;

                if remainder != 0 {
                    bits = bits + 8 - remainder;
                }

                if bits < 64 {
                    bits = 64;
                } else if bits > 64 && bits < 256 {
                    bits = 256;
                } else if bits > 256 {
                    panic!("integer has too many bits: {bits}")
                }

                format!("u{bits}")
            },
            generic_parameters: None,
        },

        sway::Literal::HexInt(value, suffix) => sway::TypeName::Identifier {
            name: if let Some(suffix) = suffix.as_ref() {
                suffix.clone()
            } else {
                let mut bits = value.bits();
                let remainder = bits % 8;

                if remainder != 0 {
                    bits = bits + 8 - remainder;
                }

                if bits < 64 {
                    bits = 64;
                } else if bits > 64 && bits < 256 {
                    bits = 256;
                } else if bits > 256 {
                    panic!("integer has too many bits: {bits}")
                }

                format!("u{bits}")
            },
            generic_parameters: None,
        },

        sway::Literal::String(_) => sway::TypeName::StringSlice,
    }
}

#[inline(always)]
fn get_path_expr_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    path_expr: &sway::PathExpr,
) -> sway::TypeName {
    fn check_expr(
        project: &mut Project,
        module: Rc<RefCell<ir::Module>>,
        scope: Rc<RefCell<ir::Scope>>,
        path_expr: &sway::PathExpr,
    ) -> Option<sway::TypeName> {
        let Some(name) = path_expr.as_identifier() else {
            todo!("get type of non-identifier path expressions: {path_expr} - {path_expr:#?}")
        };

        // HACK: Check if the identifier is a translated enum variant
        if name.contains("::") {
            let parts = name.split("::").collect::<Vec<_>>();

            if parts.len() == 2 {
                let enum_name = parts[0];
                let variant_name = parts[1];

                if module.borrow().enums.iter().any(|e| {
                    let sway::TypeName::Identifier {
                        name,
                        generic_parameters: None,
                    } = &e.implementation.as_ref().unwrap().type_definition.name
                    else {
                        return false;
                    };

                    if !e
                        .implementation
                        .as_ref()
                        .unwrap()
                        .variants_impl
                        .items
                        .iter()
                        .any(|i| {
                            let sway::ImplItem::Constant(variant) = i else {
                                return false;
                            };

                            variant.name == variant_name
                        })
                    {
                        return false;
                    }

                    name == enum_name
                }) {
                    return Some(sway::TypeName::Identifier {
                        name: enum_name.into(),
                        generic_parameters: None,
                    });
                }
            }
        }

        if let Some(variable) = scope.borrow().get_variable_from_new_name(name) {
            let variable = variable.borrow();

            return Some(variable.type_name.clone());
        }

        if let Some(function) = module.borrow().functions.iter().find(|f| {
            let sway::TypeName::Function { new_name, .. } = &f.signature else {
                unreachable!()
            };
            new_name == name
        }) {
            return Some(function.signature.clone());
        }

        if let Some(constant) = module.borrow().constants.iter().find(|c| c.name == name) {
            return Some(constant.type_name.clone());
        }

        if let Some(configurable) = module.borrow().configurable.as_ref() {
            if let Some(field) = configurable.fields.iter().find(|c| c.name == name) {
                return Some(field.type_name.clone());
            }
        }

        if let Some(contract_name) = scope.borrow().get_contract_name() {
            if let Some(module) =
                project.find_module_containing_contract(module.clone(), &contract_name)
            {
                let scope = Rc::new(RefCell::new(ir::Scope::new(
                    Some(contract_name.as_str()),
                    Some(scope.clone()),
                )));

                for use_item in module.borrow().uses.iter() {
                    if let Some(module) = project.resolve_use(use_item) {
                        if let Some(result) =
                            check_expr(project, module.clone(), scope.clone(), path_expr)
                        {
                            return Some(result);
                        }
                    }
                }
            }
        }

        None
    }

    if let Some(result) = check_expr(project, module.clone(), scope.clone(), path_expr) {
        return result;
    }

    panic!(
        "error: Variable not found in scope: \"{}\"",
        sway::TabbedDisplayer(path_expr)
    );
}

#[inline(always)]
fn get_block_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    block: &sway::Block,
) -> Result<sway::TypeName, Error> {
    let Some(expression) = block.final_expr.as_ref() else {
        return Ok(sway::TypeName::Tuple { type_names: vec![] });
    };

    let inner_scope = Rc::new(RefCell::new(ir::Scope::new(None, Some(scope.clone()))));

    for statement in block.statements.iter() {
        let sway::Statement::Let(sway::Let {
            pattern,
            type_name,
            value,
        }) = statement
        else {
            continue;
        };

        let type_name = match type_name.as_ref() {
            Some(type_name) => type_name.clone(),
            None => get_expression_type(project, module.clone(), inner_scope.clone(), value)?,
        };

        let add_variable = |id: &sway::LetIdentifier, type_name: &sway::TypeName| {
            inner_scope
                .borrow_mut()
                .add_variable(Rc::new(RefCell::new(ir::Variable {
                    old_name: String::new(),
                    new_name: id.name.clone(),
                    type_name: type_name.clone(),
                    ..Default::default()
                })));
        };

        match pattern {
            sway::LetPattern::Identifier(id) => add_variable(id, &type_name),

            sway::LetPattern::Tuple(ids) => {
                let sway::TypeName::Tuple { type_names } = &type_name else {
                    panic!("Expected tuple type, found {type_name}");
                };

                for (id, type_name) in ids.iter().zip(type_names.iter()) {
                    add_variable(id, type_name);
                }
            }
        }
    }

    get_expression_type(project, module.clone(), inner_scope.clone(), expression)
}

#[inline(always)]
fn get_return_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    value: Option<&sway::Expression>,
) -> Result<sway::TypeName, Error> {
    if let Some(value) = value.as_ref() {
        get_expression_type(project, module.clone(), scope.clone(), value)
    } else {
        Ok(sway::TypeName::Tuple { type_names: vec![] })
    }
}

#[inline(always)]
fn get_array_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    array: &sway::Array,
) -> Result<sway::TypeName, Error> {
    Ok(sway::TypeName::Array {
        type_name: Box::new(if let Some(expression) = array.elements.first() {
            get_expression_type(project, module.clone(), scope.clone(), expression)?
        } else {
            sway::TypeName::Tuple { type_names: vec![] }
        }),
        length: array.elements.len(),
    })
}

#[inline(always)]
fn get_array_access_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    array_access: &sway::ArrayAccess,
) -> Result<sway::TypeName, Error> {
    let element_type_name = get_expression_type(
        project,
        module.clone(),
        scope.clone(),
        &array_access.expression,
    )?;

    let type_name = match &element_type_name {
        sway::TypeName::Identifier {
            name,
            generic_parameters: Some(generic_parameters),
        } if name == "Vec" => &generic_parameters.entries.first().unwrap().type_name,

        sway::TypeName::Array { type_name, .. } => type_name.as_ref(),

        _ => todo!(
            "array access for type {element_type_name}: {}",
            sway::TabbedDisplayer(array_access)
        ),
    };

    Ok(type_name.clone())
}

#[inline(always)]
fn get_member_access_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    member_access: &sway::MemberAccess,
    expression: &sway::Expression,
) -> Result<sway::TypeName, Error> {
    if let sway::Expression::PathExpr(path) = &member_access.expression {
        let name = path.to_string();

        if name.starts_with("storage::") {
            let parts = name.split("::").collect::<Vec<_>>();

            assert!(parts.len() == 2);

            let contract_name = scope.borrow().get_contract_name().unwrap();
            let storage_namespace_name = parts[1];

            fn check_contract(
                project: &mut Project,
                module: Rc<RefCell<ir::Module>>,
                contract: Rc<RefCell<ir::Contract>>,
                storage_namespace_name: &str,
                storage_field_name: &str,
            ) -> Result<Option<sway::TypeName>, Error> {
                let contract_storage_namespace_name = contract.borrow().name.to_case(Case::Snake);

                if storage_namespace_name == contract_storage_namespace_name {
                    if let Some(storage) = contract.borrow().storage.as_ref() {
                        if let Some(storage_namespace) = storage
                            .borrow()
                            .namespaces
                            .iter()
                            .find(|n| n.borrow().name == contract_storage_namespace_name)
                        {
                            if let Some(field) = storage_namespace
                                .borrow()
                                .fields
                                .iter()
                                .find(|f| f.name == storage_field_name)
                            {
                                return Ok(Some(field.type_name.clone()));
                            }
                        }
                    }
                }

                let inherits = contract.borrow().abi.inherits.clone();

                for inherited_contract_name in inherits {
                    let (module, inherited_contract) = project
                        .find_module_and_contract(
                            module.clone(),
                            inherited_contract_name.to_string().as_str(),
                        )
                        .unwrap();

                    if let Some(result) = check_contract(
                        project,
                        module.clone(),
                        inherited_contract.clone(),
                        storage_namespace_name,
                        storage_field_name,
                    )? {
                        return Ok(Some(result));
                    }
                }

                Ok(None)
            }

            let contract = project
                .find_contract(module.clone(), contract_name.as_str())
                .unwrap();

            if let Some(type_name) = check_contract(
                project,
                module.clone(),
                contract.clone(),
                storage_namespace_name,
                member_access.member.as_str(),
            )? {
                return Ok(sway::TypeName::Identifier {
                    name: "StorageKey".into(),
                    generic_parameters: Some(sway::GenericParameterList {
                        entries: vec![sway::GenericParameter {
                            type_name,
                            implements: None,
                        }],
                    }),
                });
            }

            panic!(
                "Failed to find storage variable in scope: `{}`",
                sway::TabbedDisplayer(member_access),
            )
        }
    }

    let container_type = get_expression_type(
        project,
        module.clone(),
        scope.clone(),
        &member_access.expression,
    )?;

    // Check if field is a signed integer
    if let Some(bits) = container_type.int_bits() {
        match member_access.member.as_str() {
            "underlying" => {
                return Ok(sway::TypeName::Identifier {
                    name: match bits {
                        8 => "u8",
                        16 => "u16",
                        32 => "u32",
                        64 => "u64",
                        128 => "U128",
                        256 => "u256",
                        _ => unimplemented!("I{bits}"),
                    }
                    .into(),
                    generic_parameters: None,
                });
            }

            _ => {}
        }
    }

    // Check if container is a struct
    if let sway::TypeName::Identifier {
        name,
        generic_parameters: None,
    } = &container_type
    {
        if let Some(struct_definition) = project.find_struct(module.clone(), name) {
            if let Some(field) = struct_definition
                .borrow()
                .fields
                .iter()
                .find(|f| f.name == member_access.member)
            {
                return Ok(field.type_name.clone());
            }
        }
    }

    todo!("get type of {container_type} member access expression: {expression:#?}")
}

#[inline(always)]
fn get_tuple_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    tuple: &[sway::Expression],
) -> Result<sway::TypeName, Error> {
    if tuple.len() == 1 {
        get_expression_type(
            project,
            module.clone(),
            scope.clone(),
            tuple.first().unwrap(),
        )
    } else {
        Ok(sway::TypeName::Tuple {
            type_names: tuple
                .iter()
                .map(|x| get_expression_type(project, module.clone(), scope.clone(), x))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}

#[inline(always)]
fn get_if_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    if_expr: &sway::If,
) -> Result<sway::TypeName, Error> {
    if let Some(expression) = if_expr.then_body.final_expr.as_ref() {
        get_expression_type(project, module.clone(), scope.clone(), expression)
    } else {
        Ok(sway::TypeName::Tuple { type_names: vec![] })
    }
}

#[inline(always)]
fn get_match_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    match_expr: &sway::Match,
) -> Result<sway::TypeName, Error> {
    if let Some(branch) = match_expr.branches.first() {
        let branch_scope = Rc::new(RefCell::new(ir::Scope::new(None, Some(scope.clone()))));

        // Add branch pattern destructured variables to branch-specific scope
        match &branch.pattern {
            sway::Expression::FunctionCall(f) => match &f.function {
                sway::Expression::PathExpr(path_expr) => match path_expr.to_string().as_str() {
                    "Identity::Address" if f.parameters.len() == 1 => {
                        if let Some(ident) = f.parameters[0].as_identifier() {
                            branch_scope.borrow_mut().add_variable(Rc::new(RefCell::new(
                                ir::Variable {
                                    new_name: ident.into(),
                                    type_name: sway::TypeName::Identifier {
                                        name: "Address".into(),
                                        generic_parameters: None,
                                    },
                                    ..Default::default()
                                },
                            )));
                        }
                    }

                    "Identity::ContractId" if f.parameters.len() == 1 => {
                        if let Some(ident) = f.parameters[0].as_identifier() {
                            branch_scope.borrow_mut().add_variable(Rc::new(RefCell::new(
                                ir::Variable {
                                    new_name: ident.into(),
                                    type_name: sway::TypeName::Identifier {
                                        name: "ContractId".into(),
                                        generic_parameters: None,
                                    },
                                    ..Default::default()
                                },
                            )));
                        }
                    }

                    _ => {}
                },

                _ => {}
            },

            _ => {}
        }

        return get_expression_type(project, module.clone(), branch_scope, &branch.value);
    }

    Ok(sway::TypeName::Tuple { type_names: vec![] })
}

#[inline(always)]
fn get_unary_expression_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    unary_expression: &sway::UnaryExpression,
) -> Result<sway::TypeName, Error> {
    get_expression_type(
        project,
        module.clone(),
        scope.clone(),
        &unary_expression.expression,
    )
}

#[inline(always)]
fn get_binary_expression_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    binary_expression: &sway::BinaryExpression,
) -> Result<sway::TypeName, Error> {
    match binary_expression.operator.as_str() {
        "==" | "!=" | ">" | "<" | ">=" | "<=" | "&&" | "||" => Ok(sway::TypeName::Identifier {
            name: "bool".into(),
            generic_parameters: None,
        }),

        _ => get_expression_type(
            project,
            module.clone(),
            scope.clone(),
            &binary_expression.lhs,
        ),
    }
}

#[inline(always)]
fn get_asm_block_type(asm_block: &sway::AsmBlock) -> Result<sway::TypeName, Error> {
    match asm_block.final_expression.as_ref() {
        Some(expression) => match expression.type_name.as_ref() {
            Some(type_name) => Ok(type_name.clone()),
            None => todo!(),
        },
        None => todo!(),
    }
}

#[inline(always)]
fn get_function_call_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &sway::Expression,
) -> Result<sway::TypeName, Error> {
    let (function, function_generic_parameters, parameters) = match expression {
        sway::Expression::FunctionCall(f) => {
            (&f.function, f.generic_parameters.as_ref(), &f.parameters)
        }
        sway::Expression::FunctionCallBlock(f) => {
            (&f.function, f.generic_parameters.as_ref(), &f.parameters)
        }
        _ => unimplemented!(),
    };

    match function {
        sway::Expression::PathExpr(path_expr) => Ok(get_path_expr_function_call_type(
            project,
            module.clone(),
            scope.clone(),
            path_expr,
            function_generic_parameters,
            parameters.as_slice(),
        )?
        .unwrap()),

        sway::Expression::MemberAccess(member_access) => get_member_access_function_call_type(
            project,
            module.clone(),
            scope.clone(),
            member_access,
            function_generic_parameters,
            parameters.as_slice(),
        ),

        _ => todo!(
            "get type of function call expression: {} - {expression:#?}",
            sway::TabbedDisplayer(expression)
        ),
    }
}

#[inline(always)]
fn get_path_expr_function_call_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    path_expr: &sway::PathExpr,
    generic_parameters: Option<&sway::GenericParameterList>,
    parameters: &[sway::Expression],
) -> Result<Option<sway::TypeName>, Error> {
    //
    // TODO: check generic parameters!
    //

    let name = path_expr.to_string();

    match name.as_str() {
        "todo!" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "todo!".into(),
                generic_parameters: None,
            }));
        }

        "abi" => {
            assert!(
                parameters.len() == 2,
                "Malformed abi cast, expected 2 parameters, found {}",
                parameters.len()
            );

            let Some(definition_name) = parameters[0].as_identifier() else {
                panic!(
                    "Malformed abi cast, expected identifier, found {:#?}",
                    parameters[0]
                );
            };

            return Ok(Some(sway::TypeName::Identifier {
                name: definition_name.into(),
                generic_parameters: None,
            }));
        }

        _ => {}
    }

    let parameter_types = parameters
        .iter()
        .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
        .collect::<Result<Vec<_>, _>>()?;

    match name.as_str() {
        "__size_of" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "u64".into(),
                generic_parameters: None,
            }));
        }

        "Address::from" => {
            assert!(
                parameters.len() == 1,
                "Malformed `Address::from` call, expected 1 parameter, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::Identifier {
                name: "Address".into(),
                generic_parameters: None,
            }));
        }

        "AssetId::default" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "AssetId".into(),
                generic_parameters: None,
            }));
        }

        "b256::from" | "b256::from_be_bytes" | "b256::from_le_bytes" | "b256::zero" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "b256".into(),
                generic_parameters: None,
            }));
        }

        "Bytes::new" | "Bytes::from" | "Bytes::with_capacity" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "Bytes".into(),
                generic_parameters: None,
            }));
        }

        "ContractId::from" | "ContractId::this" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "ContractId".into(),
                generic_parameters: None,
            }));
        }

        "I8::from" | "I8::from_uint" | "I8::max" | "I8::min" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "I8".into(),
                generic_parameters: None,
            }));
        }

        "I8::try_from" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "Option".into(),
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![sway::GenericParameter {
                        type_name: sway::TypeName::Identifier {
                            name: "I8".into(),
                            generic_parameters: None,
                        },
                        implements: None,
                    }],
                }),
            }));
        }

        "I16::from" | "I16::from_uint" | "I16::max" | "I16::min" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "I16".into(),
                generic_parameters: None,
            }));
        }

        "I16::try_from" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "Option".into(),
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![sway::GenericParameter {
                        type_name: sway::TypeName::Identifier {
                            name: "I16".into(),
                            generic_parameters: None,
                        },
                        implements: None,
                    }],
                }),
            }));
        }

        "I32::from" | "I32::from_uint" | "I32::max" | "I32::min" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "I32".into(),
                generic_parameters: None,
            }));
        }

        "I32::try_from" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "Option".into(),
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![sway::GenericParameter {
                        type_name: sway::TypeName::Identifier {
                            name: "I32".into(),
                            generic_parameters: None,
                        },
                        implements: None,
                    }],
                }),
            }));
        }

        "I64::from" | "I64::from_uint" | "I64::max" | "I64::min" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "I64".into(),
                generic_parameters: None,
            }));
        }

        "I64::try_from" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "Option".into(),
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![sway::GenericParameter {
                        type_name: sway::TypeName::Identifier {
                            name: "I64".into(),
                            generic_parameters: None,
                        },
                        implements: None,
                    }],
                }),
            }));
        }

        "I128::from" | "I128::from_uint" | "I128::max" | "I128::min" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "I128".into(),
                generic_parameters: None,
            }));
        }

        "I128::try_from" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "Option".into(),
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![sway::GenericParameter {
                        type_name: sway::TypeName::Identifier {
                            name: "I128".into(),
                            generic_parameters: None,
                        },
                        implements: None,
                    }],
                }),
            }));
        }

        "I256::from" | "I256::from_uint" | "I256::max" | "I256::min" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "I256".into(),
                generic_parameters: None,
            }));
        }

        "I256::try_from" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "Option".into(),
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![sway::GenericParameter {
                        type_name: sway::TypeName::Identifier {
                            name: "I256".into(),
                            generic_parameters: None,
                        },
                        implements: None,
                    }],
                }),
            }));
        }

        "Identity::Address" | "Identity::ContractId" | "Identity::from" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "Identity".into(),
                generic_parameters: None,
            }));
        }

        "msg_sender" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "Option".into(),
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![sway::GenericParameter {
                        type_name: sway::TypeName::Identifier {
                            name: "Identity".into(),
                            generic_parameters: None,
                        },
                        implements: None,
                    }],
                }),
            }));
        }

        "raw_slice::from_parts" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "raw_slice".into(),
                generic_parameters: None,
            }));
        }

        "Secp256k1::from" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "Secp256k1".into(),
                generic_parameters: None,
            }));
        }

        "std::alloc::alloc" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "raw_ptr".into(),
                generic_parameters: None,
            }));
        }

        "std::block::block_header_hash" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "Result".into(),
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![
                        sway::GenericParameter {
                            type_name: sway::TypeName::Identifier {
                                name: "b256".into(),
                                generic_parameters: None,
                            },
                            implements: None,
                        },
                        sway::GenericParameter {
                            type_name: sway::TypeName::Identifier {
                                name: "BlockHashError".into(),
                                generic_parameters: None,
                            },
                            implements: None,
                        },
                    ],
                }),
            }));
        }

        "std::block::height" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "u32".into(),
                generic_parameters: None,
            }));
        }

        "std::block::timestamp" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "u64".into(),
                generic_parameters: None,
            }));
        }

        "std::context::balance_of" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "u64".into(),
                generic_parameters: None,
            }));
        }

        "std::context::msg_amount" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "u64".into(),
                generic_parameters: None,
            }));
        }

        "std::context::this_balance" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "u64".into(),
                generic_parameters: None,
            }));
        }

        "std::hash::keccak256" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "b256".into(),
                generic_parameters: None,
            }));
        }

        "std::hash::sha256" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "b256".into(),
                generic_parameters: None,
            }));
        }

        "std::inputs::input_message_data" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "Option".into(),
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![sway::GenericParameter {
                        type_name: sway::TypeName::Identifier {
                            name: "Bytes".into(),
                            generic_parameters: None,
                        },
                        implements: None,
                    }],
                }),
            }));
        }

        "std::registers::balance" => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::balance` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::Identifier {
                name: "u64".into(),
                generic_parameters: None,
            }));
        }

        "std::registers::context_gas" => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::context_gas` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::Identifier {
                name: "u64".into(),
                generic_parameters: None,
            }));
        }

        "std::registers::error" => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::error` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::Identifier {
                name: "u64".into(),
                generic_parameters: None,
            }));
        }

        "std::registers::flags" => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::flags` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::Identifier {
                name: "u64".into(),
                generic_parameters: None,
            }));
        }

        "std::registers::frame_ptr" => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::frame_ptr` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::Identifier {
                name: "raw_ptr".into(),
                generic_parameters: None,
            }));
        }

        "std::registers::global_gas" => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::global_gas` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::Identifier {
                name: "u64".into(),
                generic_parameters: None,
            }));
        }

        "std::registers::heap_ptr" => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::heap_ptr` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::Identifier {
                name: "raw_ptr".into(),
                generic_parameters: None,
            }));
        }

        "std::registers::instrs_start" => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::instrs_start` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::Identifier {
                name: "raw_ptr".into(),
                generic_parameters: None,
            }));
        }

        "std::registers::overflow" => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::overflow` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::Identifier {
                name: "u64".into(),
                generic_parameters: None,
            }));
        }

        "std::registers::program_counter" => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::program_counter` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::Identifier {
                name: "raw_ptr".into(),
                generic_parameters: None,
            }));
        }

        "std::registers::return_value" => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::return_value` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::Identifier {
                name: "u64".into(),
                generic_parameters: None,
            }));
        }

        "std::registers::return_length" => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::return_length` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::Identifier {
                name: "u64".into(),
                generic_parameters: None,
            }));
        }

        "std::registers::stack_ptr" => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::stack_ptr` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::Identifier {
                name: "raw_ptr".into(),
                generic_parameters: None,
            }));
        }

        "std::registers::stack_start_ptr" => {
            assert!(
                parameters.is_empty(),
                "Malformed `std::registers::stack_start_ptr` call, expected 0 parameters, found {}",
                parameters.len()
            );

            return Ok(Some(sway::TypeName::Identifier {
                name: "raw_ptr".into(),
                generic_parameters: None,
            }));
        }

        "String::from_ascii" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "String".into(),
                generic_parameters: None,
            }));
        }

        "u8::from" | "u8::max" | "u8::min" | "u8::from_be_bytes" | "u8::from_le_bytes" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "u8".into(),
                generic_parameters: None,
            }));
        }

        "u8::try_from" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "Option".into(),
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![sway::GenericParameter {
                        type_name: sway::TypeName::Identifier {
                            name: "u8".into(),
                            generic_parameters: None,
                        },
                        implements: None,
                    }],
                }),
            }));
        }

        "u16::from" | "u16::max" | "u16::min" | "u16::from_be_bytes" | "u16::from_le_bytes" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "u16".into(),
                generic_parameters: None,
            }));
        }

        "u16::try_from" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "Option".into(),
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![sway::GenericParameter {
                        type_name: sway::TypeName::Identifier {
                            name: "u16".into(),
                            generic_parameters: None,
                        },
                        implements: None,
                    }],
                }),
            }));
        }

        "u32::from" | "u32::max" | "u32::min" | "u32::from_be_bytes" | "u32::from_le_bytes" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "u32".into(),
                generic_parameters: None,
            }));
        }

        "u32::try_from" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "Option".into(),
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![sway::GenericParameter {
                        type_name: sway::TypeName::Identifier {
                            name: "u32".into(),
                            generic_parameters: None,
                        },
                        implements: None,
                    }],
                }),
            }));
        }

        "u64::from" | "u64::max" | "u64::min" | "u64::from_be_bytes" | "u64::from_le_bytes" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "u64".into(),
                generic_parameters: None,
            }));
        }

        "u64::try_from" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "Option".into(),
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![sway::GenericParameter {
                        type_name: sway::TypeName::Identifier {
                            name: "u64".into(),
                            generic_parameters: None,
                        },
                        implements: None,
                    }],
                }),
            }));
        }

        "u256::from"
        | "u256::max"
        | "u256::min"
        | "u256::from_be_bytes"
        | "u256::from_le_bytes" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "u256".into(),
                generic_parameters: None,
            }));
        }

        "u256::try_from" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "Option".into(),
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![sway::GenericParameter {
                        type_name: sway::TypeName::Identifier {
                            name: "u256".into(),
                            generic_parameters: None,
                        },
                        implements: None,
                    }],
                }),
            }));
        }

        "U128::from" | "U128::max" | "U128::min" | "U128::zero" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "U128".into(),
                generic_parameters: None,
            }));
        }

        "U128::try_from" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "Option".into(),
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![sway::GenericParameter {
                        type_name: sway::TypeName::Identifier {
                            name: "U128".into(),
                            generic_parameters: None,
                        },
                        implements: None,
                    }],
                }),
            }));
        }

        "U256::from" | "U256::max" | "U256::min" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "U256".into(),
                generic_parameters: None,
            }));
        }

        "U256::try_from" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "Option".into(),
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![sway::GenericParameter {
                        type_name: sway::TypeName::Identifier {
                            name: "U256".into(),
                            generic_parameters: None,
                        },
                        implements: None,
                    }],
                }),
            }));
        }

        "Vec::with_capacity" => {
            return Ok(Some(sway::TypeName::Identifier {
                name: "Vec".into(),
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![sway::GenericParameter {
                        type_name: sway::TypeName::Identifier {
                            name: "_".into(),
                            generic_parameters: None,
                        },
                        implements: None,
                    }],
                }),
            }));
        }

        _ => {}
    }

    fn check_function(
        project: &mut Project,
        module: Rc<RefCell<ir::Module>>,
        scope: Rc<RefCell<ir::Scope>>,
        name: &str,
        generic_parameters: Option<&sway::GenericParameterList>,
        parameters: &[sway::Expression],
        parameter_types: &[sway::TypeName],
    ) -> Result<Option<sway::TypeName>, Error> {
        // Attempt to find a function in scope
        if let Some(function) = module.borrow().functions.iter().find(|f| {
            let sway::TypeName::Function {
                new_name: fn_name,
                parameters: fn_parameters,
                ..
            } = &f.signature
            else {
                unreachable!()
            };

            // Ensure the function's new name matches the function call we're translating
            if *fn_name != name {
                return false;
            }

            // Ensure the supplied function call args match the function's parameters
            if parameters.len() != fn_parameters.entries.len() {
                return false;
            }

            for (i, value_type_name) in parameter_types.iter().enumerate() {
                let Some(parameter_type_name) = fn_parameters.entries[i].type_name.as_ref() else {
                    continue;
                };

                // HACK: allow numeric literals for any uint types
                if value_type_name.is_uint() && parameter_type_name.is_uint() {
                    match &parameters[i] {
                        sway::Expression::Literal(
                            sway::Literal::DecInt(_, None) | sway::Literal::HexInt(_, None),
                        ) => continue,

                        sway::Expression::Commented(_, expression) => match expression.as_ref() {
                            sway::Expression::Literal(
                                sway::Literal::DecInt(_, None) | sway::Literal::HexInt(_, None),
                            ) => continue,

                            _ => {}
                        },

                        _ => {}
                    }
                }

                // HACK: allow array literals of uint types containing only literals if the lengths match
                if let (
                    sway::TypeName::Array {
                        type_name: value_type_name,
                        length: value_length,
                    },
                    sway::TypeName::Array {
                        type_name: parameter_type_name,
                        length: parameter_length,
                    },
                ) = (value_type_name, parameter_type_name)
                {
                    if value_length != parameter_length {
                        return false;
                    }

                    if value_type_name.is_uint() && parameter_type_name.is_uint() {
                        match &parameters[i] {
                            sway::Expression::Array(array) => {
                                if array.elements.iter().all(|e| {
                                    matches!(
                                        e,
                                        sway::Expression::Literal(
                                            sway::Literal::DecInt(_, None)
                                                | sway::Literal::HexInt(_, None)
                                        )
                                    )
                                }) {
                                    continue;
                                }
                            }

                            sway::Expression::Commented(_, expression) => {
                                match expression.as_ref() {
                                    sway::Expression::Array(array) => {
                                        if array.elements.iter().all(|e| {
                                            matches!(
                                                e,
                                                sway::Expression::Literal(
                                                    sway::Literal::DecInt(_, None)
                                                        | sway::Literal::HexInt(_, None)
                                                )
                                            )
                                        }) {
                                            continue;
                                        }
                                    }

                                    _ => {}
                                }
                            }

                            _ => {}
                        }
                    }
                }

                if !value_type_name.is_compatible_with(parameter_type_name) {
                    return false;
                }
            }

            true
        }) {
            let sway::TypeName::Function { return_type, .. } = &function.signature else {
                unreachable!()
            };

            if let Some(return_type) = return_type.as_ref() {
                return Ok(Some(return_type.as_ref().clone()));
            }

            return Ok(Some(sway::TypeName::Tuple { type_names: vec![] }));
        }

        // Attempt to find a function pointer variable in scope
        if let Some(variable) = scope.borrow().find_variable(|v| {
            let v = v.borrow();

            let sway::TypeName::Function {
                parameters: fn_parameters,
                ..
            } = &v.type_name
            else {
                return false;
            };

            // Ensure the function's new name matches the function call we're translating
            if v.new_name != *name {
                return false;
            }

            // Ensure the supplied function call args match the function's parameters
            if parameters.len() != fn_parameters.entries.len() {
                return false;
            }

            for (i, value_type_name) in parameter_types.iter().enumerate() {
                let Some(parameter_type_name) = fn_parameters.entries[i].type_name.as_ref() else {
                    continue;
                };

                if !value_type_name.is_compatible_with(parameter_type_name) {
                    return false;
                }
            }

            true
        }) {
            let variable = variable.borrow();
            let sway::TypeName::Function { return_type, .. } = &variable.type_name else {
                unreachable!()
            };

            if let Some(return_type) = return_type.as_ref() {
                return Ok(Some(return_type.as_ref().clone()));
            } else {
                return Ok(Some(sway::TypeName::Tuple { type_names: vec![] }));
            }
        }

        for use_item in module.borrow().uses.iter() {
            if let Some(found_module) = project.resolve_use(use_item) {
                if let Some(type_name) = check_function(
                    project,
                    found_module.clone(),
                    scope.clone(),
                    &name,
                    generic_parameters,
                    parameters,
                    &parameter_types,
                )? {
                    return Ok(Some(type_name));
                }
            }
        }

        Ok(None)
    }

    if let Some(type_name) = check_function(
        project,
        module.clone(),
        scope.clone(),
        &name,
        generic_parameters,
        parameters,
        &parameter_types,
    )? {
        return Ok(Some(type_name));
    }

    panic!(
        "Failed to find function or variable `{}({})` in scope",
        path_expr.to_string(),
        parameter_types
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(", "),
    )
}

#[inline(always)]
fn get_member_access_function_call_type(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    member_access: &sway::MemberAccess,
    function_generic_parameters: Option<&sway::GenericParameterList>,
    parameters: &[sway::Expression],
) -> Result<sway::TypeName, Error> {
    //
    // TODO: check generic parameters!
    //

    let mut container_type = get_expression_type(
        project,
        module.clone(),
        scope.clone(),
        &member_access.expression,
    )?;

    container_type = get_underlying_type(project, module.clone(), &container_type);

    if container_type.is_identity() {
        match member_access.member.as_str() {
            "as_address" if parameters.len() == 0 => {
                return Ok(sway::TypeName::Identifier {
                    name: "Option".into(),
                    generic_parameters: Some(sway::GenericParameterList {
                        entries: vec![sway::GenericParameter {
                            type_name: sway::TypeName::Identifier {
                                name: "Address".into(),
                                generic_parameters: None,
                            },
                            implements: None,
                        }],
                    }),
                });
            }

            "as_contract_id" if parameters.len() == 0 => {
                return Ok(sway::TypeName::Identifier {
                    name: "Option".into(),
                    generic_parameters: Some(sway::GenericParameterList {
                        entries: vec![sway::GenericParameter {
                            type_name: sway::TypeName::Identifier {
                                name: "ContractId".into(),
                                generic_parameters: None,
                            },
                            implements: None,
                        }],
                    }),
                });
            }

            "bits" if parameters.len() == 0 => {
                return Ok(sway::TypeName::Identifier {
                    name: "b256".into(),
                    generic_parameters: None,
                });
            }

            "is_address" if parameters.len() == 0 => {
                return Ok(sway::TypeName::Identifier {
                    name: "bool".into(),
                    generic_parameters: None,
                });
            }

            "is_contract_id" if parameters.len() == 0 => {
                return Ok(sway::TypeName::Identifier {
                    name: "bool".into(),
                    generic_parameters: None,
                });
            }

            _ => {}
        }
    }

    match &container_type {
        sway::TypeName::Undefined => panic!("Undefined type name"),

        sway::TypeName::Identifier {
            name,
            generic_parameters,
        } => match (name.as_str(), generic_parameters.as_ref()) {
            ("b256", None) => match member_access.member.as_str() {
                "as_u256" => Ok(sway::TypeName::Identifier {
                    name: "u256".into(),
                    generic_parameters: None,
                }),

                "to_be_bytes" => Ok(sway::TypeName::Array {
                    type_name: Box::new(sway::TypeName::Identifier {
                        name: "u8".into(),
                        generic_parameters: None,
                    }),
                    length: 32,
                }),

                "to_le_bytes" => Ok(sway::TypeName::Array {
                    type_name: Box::new(sway::TypeName::Identifier {
                        name: "u8".into(),
                        generic_parameters: None,
                    }),
                    length: 32,
                }),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("Bytes", None) => match member_access.member.as_str() {
                "as_raw_slice" => Ok(sway::TypeName::Identifier {
                    name: "raw_slice".into(),
                    generic_parameters: None,
                }),

                "get" => Ok(sway::TypeName::Identifier {
                    name: "Option".into(),
                    generic_parameters: Some(sway::GenericParameterList {
                        entries: vec![sway::GenericParameter {
                            type_name: sway::TypeName::Identifier {
                                name: "u8".into(),
                                generic_parameters: None,
                            },
                            implements: None,
                        }],
                    }),
                }),

                "len" => Ok(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }),

                "ptr" => Ok(sway::TypeName::Identifier {
                    name: "raw_ptr".into(),
                    generic_parameters: None,
                }),

                "split_at" => Ok(sway::TypeName::Tuple {
                    type_names: vec![
                        sway::TypeName::Identifier {
                            name: "Bytes".into(),
                            generic_parameters: None,
                        },
                        sway::TypeName::Identifier {
                            name: "Bytes".into(),
                            generic_parameters: None,
                        },
                    ],
                }),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("I8", None) => match member_access.member.as_str() {
                "wrapping_neg" => Ok(sway::TypeName::Identifier {
                    name: "I8".into(),
                    generic_parameters: None,
                }),

                "underlying" => Ok(sway::TypeName::Identifier {
                    name: "u8".into(),
                    generic_parameters: None,
                }),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("I16", None) => match member_access.member.as_str() {
                "wrapping_neg" => Ok(sway::TypeName::Identifier {
                    name: "I16".into(),
                    generic_parameters: None,
                }),

                "underlying" => Ok(sway::TypeName::Identifier {
                    name: "u16".into(),
                    generic_parameters: None,
                }),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("I32", None) => match member_access.member.as_str() {
                "wrapping_neg" => Ok(sway::TypeName::Identifier {
                    name: "I32".into(),
                    generic_parameters: None,
                }),

                "underlying" => Ok(sway::TypeName::Identifier {
                    name: "u32".into(),
                    generic_parameters: None,
                }),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("I64", None) => match member_access.member.as_str() {
                "wrapping_neg" => Ok(sway::TypeName::Identifier {
                    name: "I64".into(),
                    generic_parameters: None,
                }),

                "underlying" => Ok(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("I128", None) => match member_access.member.as_str() {
                "wrapping_neg" => Ok(sway::TypeName::Identifier {
                    name: "I128".into(),
                    generic_parameters: None,
                }),

                "underlying" => Ok(sway::TypeName::Identifier {
                    name: "U128".into(),
                    generic_parameters: None,
                }),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("I256", None) => match member_access.member.as_str() {
                "wrapping_neg" => Ok(sway::TypeName::Identifier {
                    name: "I256".into(),
                    generic_parameters: None,
                }),

                "underlying" => Ok(sway::TypeName::Identifier {
                    name: "u256".into(),
                    generic_parameters: None,
                }),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("Option", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                match member_access.member.as_str() {
                    "is_none" | "is_some" if parameters.is_empty() => {
                        Ok(sway::TypeName::Identifier {
                            name: "bool".into(),
                            generic_parameters: None,
                        })
                    }

                    "unwrap" => Ok(generic_parameters.entries[0].type_name.clone()),
                    "unwrap_or" => Ok(generic_parameters.entries[0].type_name.clone()),

                    _ => todo!(
                        "get type of function call expression: {}",
                        sway::TabbedDisplayer(member_access)
                    ),
                }
            }

            ("Result", Some(generic_parameters)) if generic_parameters.entries.len() == 2 => {
                match member_access.member.as_str() {
                    "unwrap" => Ok(generic_parameters.entries[0].type_name.clone()),

                    "unwrap_or" => Ok(generic_parameters.entries[0].type_name.clone()),

                    _ => todo!(
                        "get type of function call expression: {}",
                        sway::TabbedDisplayer(member_access)
                    ),
                }
            }

            ("raw_ptr", None) => match member_access.member.as_str() {
                "add" => Ok(sway::TypeName::Identifier {
                    name: "raw_ptr".into(),
                    generic_parameters: None,
                }),

                "read" => Ok(function_generic_parameters.unwrap().entries[0]
                    .type_name
                    .clone()),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("raw_slice", None) => match member_access.member.as_str() {
                "ptr" => Ok(sway::TypeName::Identifier {
                    name: "raw_ptr".into(),
                    generic_parameters: None,
                }),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("Secp256k1", None) => match member_access.member.as_str() {
                // Result<Address, SignatureError>
                "address" if parameters.len() == 1 => {
                    module
                        .borrow_mut()
                        .ensure_use_declared("std::crypto::signature_error::SignatureError");

                    Ok(sway::TypeName::Identifier {
                        name: "Result".into(),
                        generic_parameters: Some(sway::GenericParameterList {
                            entries: vec![
                                sway::GenericParameter {
                                    type_name: sway::TypeName::Identifier {
                                        name: "Address".into(),
                                        generic_parameters: None,
                                    },
                                    implements: None,
                                },
                                sway::GenericParameter {
                                    type_name: sway::TypeName::Identifier {
                                        name: "SignatureError".into(),
                                        generic_parameters: None,
                                    },
                                    implements: None,
                                },
                            ],
                        }),
                    })
                }

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("StorageKey", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                match member_access.member.as_str() {
                    "clear" => Ok(sway::TypeName::Identifier {
                        name: "bool".into(),
                        generic_parameters: None,
                    }),

                    "read" => Ok(generic_parameters.entries[0].type_name.clone()),

                    "try_read" => Ok(sway::TypeName::Identifier {
                        name: "Option".into(),
                        generic_parameters: Some(sway::GenericParameterList {
                            entries: vec![sway::GenericParameter {
                                type_name: generic_parameters.entries[0].type_name.clone(),
                                implements: None,
                            }],
                        }),
                    }),

                    "write" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                    _ => match &generic_parameters.entries[0].type_name {
                        sway::TypeName::Identifier {
                            name,
                            generic_parameters,
                        } => match (name.as_str(), generic_parameters.as_ref()) {
                            ("StorageBytes", None) => match member_access.member.as_str() {
                                "clear" => Ok(sway::TypeName::Identifier {
                                    name: "bool".into(),
                                    generic_parameters: None,
                                }),

                                "len" => Ok(sway::TypeName::Identifier {
                                    name: "u64".into(),
                                    generic_parameters: None,
                                }),

                                "read_slice" => Ok(sway::TypeName::Identifier {
                                    name: "Option".into(),
                                    generic_parameters: Some(sway::GenericParameterList {
                                        entries: vec![sway::GenericParameter {
                                            type_name: sway::TypeName::Identifier {
                                                name: "Bytes".into(),
                                                generic_parameters: None,
                                            },
                                            implements: None,
                                        }],
                                    }),
                                }),

                                "write_slice" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                _ => todo!(
                                    "get type of function call expression: {}",
                                    sway::TabbedDisplayer(member_access)
                                ),
                            },

                            ("StorageMap", Some(generic_parameters))
                                if generic_parameters.entries.len() == 2 =>
                            {
                                match member_access.member.as_str() {
                                    "get" => Ok(sway::TypeName::Identifier {
                                        name: "StorageKey".into(),
                                        generic_parameters: Some(sway::GenericParameterList {
                                            entries: vec![sway::GenericParameter {
                                                type_name: generic_parameters.entries[1]
                                                    .type_name
                                                    .clone(),
                                                implements: None,
                                            }],
                                        }),
                                    }),

                                    "insert" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                    "remove" => Ok(sway::TypeName::Identifier {
                                        name: "bool".into(),
                                        generic_parameters: None,
                                    }),

                                    "try_insert" => Ok(sway::TypeName::Identifier {
                                        name: "Result".into(),
                                        generic_parameters: Some(sway::GenericParameterList {
                                            entries: vec![
                                                sway::GenericParameter {
                                                    type_name: generic_parameters.entries[0]
                                                        .type_name
                                                        .clone(),
                                                    implements: None,
                                                },
                                                sway::GenericParameter {
                                                    type_name: sway::TypeName::Identifier {
                                                        name: "StorageMapError".into(),
                                                        generic_parameters: Some(
                                                            sway::GenericParameterList {
                                                                entries: vec![
                                                                    sway::GenericParameter {
                                                                        type_name:
                                                                            generic_parameters
                                                                                .entries[0]
                                                                                .type_name
                                                                                .clone(),
                                                                        implements: None,
                                                                    },
                                                                ],
                                                            },
                                                        ),
                                                    },
                                                    implements: None,
                                                },
                                            ],
                                        }),
                                    }),

                                    _ => todo!(
                                        "get type of function call expression: {}",
                                        sway::TabbedDisplayer(member_access)
                                    ),
                                }
                            }

                            ("StorageString", None) => match member_access.member.as_str() {
                                "clear" => Ok(sway::TypeName::Identifier {
                                    name: "bool".into(),
                                    generic_parameters: None,
                                }),

                                "len" => Ok(sway::TypeName::Identifier {
                                    name: "u64".into(),
                                    generic_parameters: None,
                                }),

                                "read_slice" => Ok(sway::TypeName::Identifier {
                                    name: "Option".into(),
                                    generic_parameters: Some(sway::GenericParameterList {
                                        entries: vec![sway::GenericParameter {
                                            type_name: sway::TypeName::Identifier {
                                                name: "String".into(),
                                                generic_parameters: None,
                                            },
                                            implements: None,
                                        }],
                                    }),
                                }),

                                "write_slice" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                _ => todo!(
                                    "get type of function call expression: {}",
                                    sway::TabbedDisplayer(member_access)
                                ),
                            },

                            ("StorageVec", Some(generic_parameters))
                                if generic_parameters.entries.len() == 1 =>
                            {
                                match member_access.member.as_str() {
                                    "fill" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                    "first" => Ok(sway::TypeName::Identifier {
                                        name: "Option".into(),
                                        generic_parameters: Some(sway::GenericParameterList {
                                            entries: vec![sway::GenericParameter {
                                                type_name: sway::TypeName::Identifier {
                                                    name: "StorageKey".into(),
                                                    generic_parameters: Some(
                                                        sway::GenericParameterList {
                                                            entries: vec![sway::GenericParameter {
                                                                type_name: generic_parameters
                                                                    .entries[0]
                                                                    .type_name
                                                                    .clone(),
                                                                implements: None,
                                                            }],
                                                        },
                                                    ),
                                                },
                                                implements: None,
                                            }],
                                        }),
                                    }),

                                    "get" => Ok(sway::TypeName::Identifier {
                                        name: "Option".into(),
                                        generic_parameters: Some(sway::GenericParameterList {
                                            entries: vec![sway::GenericParameter {
                                                type_name: sway::TypeName::Identifier {
                                                    name: "StorageKey".into(),
                                                    generic_parameters: Some(
                                                        sway::GenericParameterList {
                                                            entries: vec![sway::GenericParameter {
                                                                type_name: generic_parameters
                                                                    .entries[0]
                                                                    .type_name
                                                                    .clone(),
                                                                implements: None,
                                                            }],
                                                        },
                                                    ),
                                                },
                                                implements: None,
                                            }],
                                        }),
                                    }),

                                    "insert" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                    "is_empty" => Ok(sway::TypeName::Identifier {
                                        name: "bool".into(),
                                        generic_parameters: None,
                                    }),

                                    "last" => Ok(sway::TypeName::Identifier {
                                        name: "Option".into(),
                                        generic_parameters: Some(sway::GenericParameterList {
                                            entries: vec![sway::GenericParameter {
                                                type_name: sway::TypeName::Identifier {
                                                    name: "StorageKey".into(),
                                                    generic_parameters: Some(
                                                        sway::GenericParameterList {
                                                            entries: vec![sway::GenericParameter {
                                                                type_name: generic_parameters
                                                                    .entries[0]
                                                                    .type_name
                                                                    .clone(),
                                                                implements: None,
                                                            }],
                                                        },
                                                    ),
                                                },
                                                implements: None,
                                            }],
                                        }),
                                    }),

                                    "len" => Ok(sway::TypeName::Identifier {
                                        name: "u64".into(),
                                        generic_parameters: None,
                                    }),

                                    "load_vec" => Ok(sway::TypeName::Identifier {
                                        name: "Vec".into(),
                                        generic_parameters: Some(sway::GenericParameterList {
                                            entries: vec![sway::GenericParameter {
                                                type_name: generic_parameters.entries[0]
                                                    .type_name
                                                    .clone(),
                                                implements: None,
                                            }],
                                        }),
                                    }),

                                    "pop" => Ok(sway::TypeName::Identifier {
                                        name: "Option".into(),
                                        generic_parameters: Some(sway::GenericParameterList {
                                            entries: vec![sway::GenericParameter {
                                                type_name: generic_parameters.entries[0]
                                                    .type_name
                                                    .clone(),
                                                implements: None,
                                            }],
                                        }),
                                    }),

                                    "push" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                    "remove" => Ok(generic_parameters.entries[0].type_name.clone()),

                                    "resize" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                    "reverse" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                    "set" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                    "store_vec" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                    "swap_remove" => {
                                        Ok(generic_parameters.entries[0].type_name.clone())
                                    }

                                    "swap" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                                    _ => todo!(
                                        "get type of function call expression: {}",
                                        sway::TabbedDisplayer(member_access)
                                    ),
                                }
                            }

                            (name, _) => todo!(
                                "get type of {name}::{} function call member_access: {}",
                                member_access.member,
                                sway::TabbedDisplayer(member_access)
                            ),
                        },

                        _ => todo!(
                            "get type of function call expression: {}",
                            sway::TabbedDisplayer(member_access)
                        ),
                    },
                }
            }

            ("StorageMap", Some(generic_parameters)) if generic_parameters.entries.len() == 2 => {
                match member_access.member.as_str() {
                    "get" if parameters.len() == 1 => Ok(sway::TypeName::Identifier {
                        name: "StorageKey".to_string(),
                        generic_parameters: Some(sway::GenericParameterList {
                            entries: vec![generic_parameters.entries[1].clone()],
                        }),
                    }),
                    _ => todo!(
                        "get type of function call expression: {}",
                        sway::TabbedDisplayer(member_access)
                    ),
                }
            }

            ("String", None) => match member_access.member.as_str() {
                "as_bytes" => Ok(sway::TypeName::Identifier {
                    name: "Bytes".into(),
                    generic_parameters: None,
                }),

                "capacity" => Ok(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }),

                "clear" => Ok(sway::TypeName::Tuple { type_names: vec![] }),

                "from_ascii" => Ok(sway::TypeName::Identifier {
                    name: "String".into(),
                    generic_parameters: None,
                }),

                "from_ascii_str" => Ok(sway::TypeName::Identifier {
                    name: "String".into(),
                    generic_parameters: None,
                }),

                "is_empty" => Ok(sway::TypeName::Identifier {
                    name: "bool".into(),
                    generic_parameters: None,
                }),

                "new" => Ok(sway::TypeName::Identifier {
                    name: "String".into(),
                    generic_parameters: None,
                }),

                "with_capacity" => Ok(sway::TypeName::Identifier {
                    name: "String".into(),
                    generic_parameters: None,
                }),

                "ptr" => Ok(sway::TypeName::Identifier {
                    name: "raw_ptr".into(),
                    generic_parameters: None,
                }),

                "as_str" => Ok(sway::TypeName::Identifier {
                    name: "str".into(),
                    generic_parameters: None,
                }),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("u8", None) => match member_access.member.as_str() {
                "as_u16" => Ok(sway::TypeName::Identifier {
                    name: "u16".into(),
                    generic_parameters: None,
                }),

                "as_u32" => Ok(sway::TypeName::Identifier {
                    name: "u32".into(),
                    generic_parameters: None,
                }),

                "as_u64" => Ok(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }),

                "as_u256" => Ok(sway::TypeName::Identifier {
                    name: "u256".into(),
                    generic_parameters: None,
                }),

                "pow" => Ok(sway::TypeName::Identifier {
                    name: "u8".into(),
                    generic_parameters: None,
                }),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("u16", None) => match member_access.member.as_str() {
                "as_u32" => Ok(sway::TypeName::Identifier {
                    name: "u32".into(),
                    generic_parameters: None,
                }),

                "as_u64" => Ok(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }),

                "as_u256" => Ok(sway::TypeName::Identifier {
                    name: "u256".into(),
                    generic_parameters: None,
                }),

                "pow" => Ok(sway::TypeName::Identifier {
                    name: "u16".into(),
                    generic_parameters: None,
                }),

                "to_be_bytes" => Ok(sway::TypeName::Array {
                    type_name: Box::new(sway::TypeName::Identifier {
                        name: "u8".into(),
                        generic_parameters: None,
                    }),
                    length: 2,
                }),

                "to_le_bytes" => Ok(sway::TypeName::Array {
                    type_name: Box::new(sway::TypeName::Identifier {
                        name: "u8".into(),
                        generic_parameters: None,
                    }),
                    length: 2,
                }),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("u32", None) => match member_access.member.as_str() {
                "as_u64" => Ok(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }),

                "as_u256" => Ok(sway::TypeName::Identifier {
                    name: "u256".into(),
                    generic_parameters: None,
                }),

                "pow" => Ok(sway::TypeName::Identifier {
                    name: "u32".into(),
                    generic_parameters: None,
                }),

                "to_be_bytes" => Ok(sway::TypeName::Array {
                    type_name: Box::new(sway::TypeName::Identifier {
                        name: "u8".into(),
                        generic_parameters: None,
                    }),
                    length: 4,
                }),

                "to_le_bytes" => Ok(sway::TypeName::Array {
                    type_name: Box::new(sway::TypeName::Identifier {
                        name: "u8".into(),
                        generic_parameters: None,
                    }),
                    length: 4,
                }),

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("u64", None) => match member_access.member.as_str() {
                "as_u256" => Ok(sway::TypeName::Identifier {
                    name: "u256".into(),
                    generic_parameters: None,
                }),

                "pow" => Ok(sway::TypeName::Identifier {
                    name: "u64".into(),
                    generic_parameters: None,
                }),

                "to_be_bytes" => Ok(sway::TypeName::Array {
                    type_name: Box::new(sway::TypeName::Identifier {
                        name: "u8".into(),
                        generic_parameters: None,
                    }),
                    length: 8,
                }),

                "to_le_bytes" => Ok(sway::TypeName::Array {
                    type_name: Box::new(sway::TypeName::Identifier {
                        name: "u8".into(),
                        generic_parameters: None,
                    }),
                    length: 8,
                }),

                "try_as_u8" => Ok(sway::TypeName::Identifier {
                    name: "Option".into(),
                    generic_parameters: Some(sway::GenericParameterList {
                        entries: vec![sway::GenericParameter {
                            type_name: sway::TypeName::Identifier {
                                name: "u8".into(),
                                generic_parameters: None,
                            },
                            implements: None,
                        }],
                    }),
                }),

                "try_as_u16" => Ok(sway::TypeName::Identifier {
                    name: "Option".into(),
                    generic_parameters: Some(sway::GenericParameterList {
                        entries: vec![sway::GenericParameter {
                            type_name: sway::TypeName::Identifier {
                                name: "u16".into(),
                                generic_parameters: None,
                            },
                            implements: None,
                        }],
                    }),
                }),

                "try_as_u32" => Ok(sway::TypeName::Identifier {
                    name: "Option".into(),
                    generic_parameters: Some(sway::GenericParameterList {
                        entries: vec![sway::GenericParameter {
                            type_name: sway::TypeName::Identifier {
                                name: "u32".into(),
                                generic_parameters: None,
                            },
                            implements: None,
                        }],
                    }),
                }),

                "wrapping_neg" => {
                    module.borrow_mut().ensure_dependency_declared(
                        "sway_libs = { git = \"https://github.com/FuelLabs/sway-libs\", tag = \"v0.25.2\" }"
                    );

                    Ok(sway::TypeName::Identifier {
                        name: {
                            module
                                .borrow_mut()
                                .ensure_use_declared("sway_libs::signed_integers::i64::*");
                            "I64".into()
                        },
                        generic_parameters: None,
                    })
                }

                _ => todo!(
                    "get type of function call expression: {}",
                    sway::TabbedDisplayer(member_access)
                ),
            },

            ("u256", None) => match member_access.member.as_str() {
                "as_b256" => Ok(sway::TypeName::Identifier {
                    name: "b256".into(),
                    generic_parameters: None,
                }),

                "pow" => Ok(sway::TypeName::Identifier {
                    name: "u256".into(),
                    generic_parameters: None,
                }),

                "to_be_bytes" => Ok(sway::TypeName::Array {
                    type_name: Box::new(sway::TypeName::Identifier {
                        name: "u8".into(),
                        generic_parameters: None,
                    }),
                    length: 32,
                }),

                "to_le_bytes" => Ok(sway::TypeName::Array {
                    type_name: Box::new(sway::TypeName::Identifier {
                        name: "u8".into(),
                        generic_parameters: None,
                    }),
                    length: 32,
                }),

                name => {
                    // Check to see if we are using a function from the using library
                    if module
                        .borrow()
                        .using_directives
                        .iter()
                        .any(|using| using.functions.iter().any(|fnc| fnc == name))
                    {
                        return Ok(sway::TypeName::Identifier {
                            name: "u256".into(),
                            generic_parameters: None,
                        });
                    }

                    todo!(
                        "get type of function call expression: {}",
                        sway::TabbedDisplayer(member_access)
                    )
                }
            },

            ("Vec", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                match member_access.member.as_str() {
                    "get" => Ok(sway::TypeName::Identifier {
                        name: "Option".into(),
                        generic_parameters: Some(sway::GenericParameterList {
                            entries: vec![generic_parameters.entries.first().unwrap().clone()],
                        }),
                    }),

                    "len" => Ok(sway::TypeName::Identifier {
                        name: "u64".into(),
                        generic_parameters: None,
                    }),

                    _ => todo!(
                        "get type of function call expression: {}",
                        sway::TabbedDisplayer(member_access)
                    ),
                }
            }

            (name, None) => {
                if let Some(contract) = project.find_contract(module.clone(), name) {
                    if let Some(function_definition) = contract
                        .borrow()
                        .abi
                        .functions
                        .iter()
                        .find(|f| f.name == member_access.member)
                    {
                        return Ok(function_definition
                            .return_type
                            .clone()
                            .unwrap_or_else(|| sway::TypeName::Tuple { type_names: vec![] }));
                    }
                }

                todo!(
                    "get type of {name} function call member_access: {}",
                    sway::TabbedDisplayer(member_access)
                )
            }

            _ => todo!(
                "get type of {name} function call member_access: {}",
                sway::TabbedDisplayer(member_access)
            ),
        },

        sway::TypeName::StringSlice => match member_access.member.as_str() {
            "as_ptr" => Ok(sway::TypeName::Identifier {
                name: "raw_ptr".into(),
                generic_parameters: None,
            }),

            "len" => Ok(sway::TypeName::Identifier {
                name: "u64".into(),
                generic_parameters: None,
            }),

            _ => todo!(
                "get type of function call expression: {}",
                sway::TabbedDisplayer(member_access)
            ),
        },

        sway::TypeName::Array { .. } => match member_access.member.as_str() {
            "len" => Ok(sway::TypeName::Identifier {
                name: "u64".into(),
                generic_parameters: None,
            }),

            value => todo!("{value}"),
        },

        _ => todo!(
            "get type of {container_type} function call member_access: {}",
            sway::TabbedDisplayer(member_access)
        ),
    }
}

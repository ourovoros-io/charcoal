use crate::{error::Error, project::Project, sway, translate::*};
use num_bigint::BigUint;
use num_traits::{Num, One, Zero};
use solang_parser::{helpers::CodeLocation, pt as solidity};
use std::{cell::RefCell, rc::Rc};

pub fn translate_address_type_cast_function_call(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: &Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
    argument: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    match argument {
        solidity::Expression::HexNumberLiteral(_, _, _)
        | solidity::Expression::NumberLiteral(_, _, _, _) => {
            Ok(sway::Expression::create_function_calls(
                None,
                &[(
                    "Identity::Address",
                    Some((
                        None,
                        vec![sway::Expression::create_function_calls(
                            None,
                            &[(
                                "Address::from",
                                Some((
                                    None,
                                    vec![sway::Expression::from(match argument {
                                        solidity::Expression::HexNumberLiteral(_, value, _) => {
                                            let value = BigUint::from_str_radix(
                                                value.as_str().trim_start_matches("0x"),
                                                16,
                                            )
                                            .unwrap();

                                            if value.is_zero() {
                                                sway::Expression::create_function_calls(
                                                    None,
                                                    &[("b256::zero", Some((None, vec![])))],
                                                )
                                            } else {
                                                sway::Expression::from(sway::Literal::HexInt(
                                                    value,
                                                    Some("b256".into()),
                                                ))
                                            }
                                        }

                                        solidity::Expression::NumberLiteral(_, value, _, _) => {
                                            let value: BigUint = value.parse().unwrap();

                                            if value.is_zero() {
                                                sway::Expression::create_function_calls(
                                                    None,
                                                    &[("b256::zero", Some((None, vec![])))],
                                                )
                                            } else {
                                                sway::Expression::from(sway::Literal::DecInt(
                                                    value,
                                                    Some("b256".into()),
                                                ))
                                            }
                                        }

                                        _ => unreachable!(),
                                    })],
                                )),
                            )],
                        )],
                    )),
                )],
            ))
        }

        solidity::Expression::Variable(solidity::Identifier { name, .. }) if name == "this" => {
            // address(this) => Identity::from(ContractId::this())
            Ok(sway::Expression::create_function_calls(
                None,
                &[(
                    "Identity::ContractId",
                    Some((
                        None,
                        vec![sway::Expression::create_function_calls(
                            None,
                            &[("ContractId::this", Some((None, vec![])))],
                        )],
                    )),
                )],
            ))
        }

        value => {
            let value = translate_expression(project, module.clone(), scope, value)?;
            let value_type_name = module.borrow_mut().get_expression_type(scope, &value)?;

            match &value_type_name {
                // No reason to cast if it's already an Identity
                sway::TypeName::Identifier {
                    name,
                    generic_parameters,
                } => match (name.as_str(), generic_parameters.as_ref()) {
                    ("Identity", None) => Ok(value),

                    ("u256", None) => Ok(sway::Expression::create_function_calls(
                        None,
                        &[(
                            "Identity::Address",
                            Some((
                                None,
                                vec![sway::Expression::create_function_calls(
                                    None,
                                    &[("Address::from", Some((None, vec![value])))],
                                )],
                            )),
                        )],
                    )),

                    _ => panic!(
                        "translate cast from {value_type_name} to address: {expression} - {value:#?}"
                    ),
                },

                sway::TypeName::Array {
                    type_name: element_type_name,
                    length,
                } => match element_type_name.as_ref() {
                    sway::TypeName::Identifier {
                        name,
                        generic_parameters,
                    } => match (name.as_str(), generic_parameters.as_ref()) {
                        ("u8", None) => {
                            module
                                .borrow_mut()
                                .ensure_use_declared("std::array_conversions::u256::*");

                            let mut elements = (0..*length)
                                .map(|i| {
                                    sway::Expression::from(sway::ArrayAccess {
                                        expression: value.clone(),
                                        index: sway::Expression::from(sway::Literal::DecInt(
                                            i.into(),
                                            None,
                                        )),
                                    })
                                })
                                .collect::<Vec<_>>();

                            if *length < 32 {
                                elements.extend((0..(32 - *length)).map(|_| {
                                    sway::Expression::from(sway::Literal::DecInt(
                                        BigUint::zero(),
                                        None,
                                    ))
                                }));
                            }

                            Ok(sway::Expression::create_function_calls(
                                None,
                                &[(
                                    "Identity::Address",
                                    Some((
                                        None,
                                        vec![sway::Expression::create_function_calls(
                                            None,
                                            &[(
                                                "Address::from",
                                                Some((
                                                    None,
                                                    vec![sway::Expression::create_function_calls(
                                                        None,
                                                        &[(
                                                            "u256::from_be_bytes",
                                                            Some((
                                                                None,
                                                                vec![sway::Expression::from(
                                                                    sway::Array { elements },
                                                                )],
                                                            )),
                                                        )],
                                                    )],
                                                )),
                                            )],
                                        )],
                                    )),
                                )],
                            ))
                        }

                        _ => panic!(
                            "{}translate cast from {value_type_name} to address: {expression} - {value_type_name:#?}",
                            match project
                                .loc_to_line_and_column(module.clone(), &argument.loc())
                            {
                                Some((line, col)) => format!(
                                    "{}:{}:{}: ",
                                    project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy(),
                                    line,
                                    col
                                ),
                                None => format!("{}: ", project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy()),
                            },
                        ),
                    },

                    _ => panic!(
                        "{}translate cast from {value_type_name} to address: {expression} - {value_type_name:#?}",
                        match project.loc_to_line_and_column(module.clone(), &argument.loc())
                        {
                            Some((line, col)) => format!(
                                "{}:{}:{}: ",
                                project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy(),
                                line,
                                col
                            ),
                            None => format!("{}: ", project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy()),
                        },
                    ),
                },

                _ => panic!(
                    "{}translate cast from {value_type_name} to address: {expression} - {value_type_name:#?}",
                    match project.loc_to_line_and_column(module.clone(), &argument.loc()) {
                        Some((line, col)) => format!(
                            "{}:{}:{}: ",
                            project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy(),
                            line,
                            col
                        ),
                        None => format!("{}: ", project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy()),
                    },
                ),
            }
        }
    }
}

// payable(x) => x
pub fn translate_payable_type_cast_function_call(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: &Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
    arguments: &[solidity::Expression],
) -> Result<sway::Expression, Error> {
    let parameters = arguments
        .iter()
        .map(|a| translate_expression(project, module.clone(), scope, a))
        .collect::<Result<Vec<_>, _>>()?;

    if parameters.len() != 1 {
        panic!("Malformed payable cast: {expression} - {expression:#?}");
    }

    Ok(parameters[0].clone())
}

pub fn translate_int_types_cast_function_call(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: &Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
    argument: &solidity::Expression,
    bits: usize,
) -> Result<sway::Expression, Error> {
    let value_expression = translate_expression(project, module.clone(), scope, argument)?;
    let value_type_name = module
        .borrow_mut()
        .get_expression_type(scope, &value_expression)?;
    let value_type_name = module.borrow().get_underlying_type(&value_type_name);

    let create_int_try_from_unwrap_expression = |from_bits: usize,
                                                 to_bits: usize,
                                                 value: sway::Expression|
     -> Result<sway::Expression, Error> {
        if from_bits == to_bits {
            return Ok(value);
        }

        if from_bits < to_bits {
            return Ok(sway::Expression::create_function_calls(
                None,
                &[(
                    format!("I{to_bits}::from").as_str(),
                    Some((
                        None,
                        vec![sway::Expression::create_function_calls(
                            Some(value),
                            &[(format!("as_u{to_bits}").as_str(), Some((None, vec![])))],
                        )],
                    )),
                )],
            ));
        }

        Ok(sway::Expression::create_function_calls(
            None,
            &[(
                format!("I{to_bits}::from").as_str(),
                Some((
                    None,
                    vec![sway::Expression::create_function_calls(
                        None,
                        &[
                            (
                                format!("u{to_bits}::try_from").as_str(),
                                Some((None, vec![value])),
                            ),
                            ("unwrap", Some((None, vec![]))),
                        ],
                    )],
                )),
            )],
        ))
    };

    let Some(bits) = match_bits(bits, true) else {
        panic!("Invalid int type: {expression:#?}")
    };

    match &value_type_name {
        sway::TypeName::Identifier {
            name,
            generic_parameters: None,
        } => match (name.as_str(), bits) {
            ("I8", 8 | 16 | 32 | 64 | 128 | 256) => {
                create_int_try_from_unwrap_expression(8, bits, value_expression)
            }
            ("I16", 8 | 16 | 32 | 64 | 128 | 256) => {
                create_int_try_from_unwrap_expression(16, bits, value_expression)
            }
            ("I32", 8 | 16 | 32 | 64 | 128 | 256) => {
                create_int_try_from_unwrap_expression(32, bits, value_expression)
            }
            ("I64", 8 | 16 | 32 | 64 | 128 | 256) => {
                create_int_try_from_unwrap_expression(64, bits, value_expression)
            }
            ("I128", 8 | 16 | 32 | 64 | 128 | 256) => {
                create_int_try_from_unwrap_expression(128, bits, value_expression)
            }
            ("I256", 8 | 16 | 32 | 64 | 128 | 256) => {
                create_int_try_from_unwrap_expression(256, bits, value_expression)
            }

            ("u8", 32) => Ok(sway::Expression::create_function_calls(
                None,
                &[(
                    "I32::from_uint",
                    Some((
                        None,
                        vec![sway::Expression::create_function_calls(
                            Some(value_expression),
                            &[("as_u32", Some((None, vec![])))],
                        )],
                    )),
                )],
            )),

            ("u64", 256) => Ok(sway::Expression::create_function_calls(
                None,
                &[(
                    "I256::from_uint",
                    Some((
                        None,
                        vec![sway::Expression::create_function_calls(
                            Some(value_expression),
                            &[("as_u256", Some((None, vec![])))],
                        )],
                    )),
                )],
            )),

            ("u256", 256) => Ok(sway::Expression::create_function_calls(
                None,
                &[("I256::from_uint", Some((None, vec![value_expression])))],
            )),

            _ => todo!(
                "translate type cast from {value_type_name} to I{bits}: {expression} - {expression:#?}"
            ),
        },

        _ => todo!(
            "translate type cast from {value_type_name} to I{bits}: {expression} - {expression:#?}"
        ),
    }
}

pub fn translate_uint_types_cast_function_call(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: &Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
    argument: &solidity::Expression,
    bits: usize,
) -> Result<sway::Expression, Error> {
    let value_expression = translate_expression(project, module.clone(), scope, argument)?;
    let value_type_name = module
        .borrow_mut()
        .get_expression_type(scope, &value_expression)?;
    let value_type_name = module.borrow().get_underlying_type(&value_type_name);

    if value_type_name.is_int() {
        match argument {
            solidity::Expression::Negate(_, expr) => match expr.as_ref() {
                solidity::Expression::NumberLiteral(_, value, _, _) => {
                    let value = value
                        .parse::<BigUint>()
                        .map_err(|e| Error::Wrapped(Box::new(e)))?;
                    let max = if bits == 256 {
                        BigUint::from_str_radix(
                            "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
                            16,
                        )
                        .unwrap()
                    } else {
                        (BigUint::one() << bits) - BigUint::one()
                    };

                    return Ok(sway::Expression::from(sway::Literal::HexInt(
                        (max - value) + BigUint::one(),
                        Some("u256".to_string()),
                    )));
                }
                _ => {}
            },
            _ => {}
        }
    }

    let create_uint_try_from_unwrap_expression = |from_bits: usize,
                                                  to_bits: usize,
                                                  value: sway::Expression|
     -> Result<sway::Expression, Error> {
        if from_bits == to_bits {
            return Ok(value);
        }

        if from_bits < to_bits {
            return Ok(sway::Expression::create_function_calls(
                Some(value),
                &[(format!("as_u{to_bits}").as_str(), Some((None, vec![])))],
            ));
        }

        Ok(sway::Expression::create_function_calls(
            None,
            &[
                (
                    format!("u{to_bits}::try_from").as_str(),
                    Some((None, vec![value])),
                ),
                ("unwrap", Some((None, vec![]))),
            ],
        ))
    };

    let Some(bits) = match_bits(bits, false) else {
        panic!("Invalid uint type: {expression:#?}")
    };

    match &value_type_name {
        sway::TypeName::Identifier { name, .. } => match (name.as_str(), bits) {
            ("u8", 8 | 16 | 32 | 64 | 256) => {
                create_uint_try_from_unwrap_expression(8, bits, value_expression)
            }

            ("u16", 8 | 16 | 32 | 64 | 256) => {
                create_uint_try_from_unwrap_expression(16, bits, value_expression)
            }

            ("u32", 8 | 16 | 32 | 64 | 256) => {
                create_uint_try_from_unwrap_expression(32, bits, value_expression)
            }

            ("u64", 8 | 16 | 32 | 64 | 256) => {
                create_uint_try_from_unwrap_expression(64, bits, value_expression)
            }

            ("u256", 8 | 16 | 32 | 64 | 256) => {
                create_uint_try_from_unwrap_expression(256, bits, value_expression)
            }

            ("u8" | "u16" | "u32" | "u64", 128) => {
                module.borrow_mut().ensure_use_declared("std::u128::U128");

                Ok(sway::Expression::create_function_calls(
                    None,
                    &[("U128::from", Some((None, vec![value_expression])))],
                ))
            }

            ("u256", 128) => {
                // use std::u128::U128;
                // {
                //     let parts = asm(r1: value) {
                //         r1: (u64, u64, u64, u64)
                //     };
                //
                //     if parts.0 != 0 || parts.1 != 0 {
                //         revert();
                //     }
                //
                //     U128::from((parts.2, parts.3))
                // }

                module.borrow_mut().ensure_use_declared("std::u128::U128");

                let unique_variable_name = scope.borrow().generate_unique_variable_name("parts");

                Ok(sway::Expression::from(sway::Block {
                    statements: vec![
                        sway::Statement::from(sway::Let {
                            pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                is_mutable: false,
                                name: unique_variable_name.clone(),
                            }),
                            type_name: None,
                            value: sway::Expression::from(sway::AsmBlock {
                                registers: vec![sway::AsmRegister {
                                    name: "r1".to_string(),
                                    value: Some(value_expression),
                                }],
                                instructions: vec![],
                                final_expression: Some(sway::AsmFinalExpression {
                                    register: "r1".to_string(),
                                    type_name: Some(sway::TypeName::Tuple {
                                        type_names: vec![
                                            sway::TypeName::Identifier {
                                                name: "u64".to_string(),
                                                generic_parameters: None,
                                            },
                                            sway::TypeName::Identifier {
                                                name: "u64".to_string(),
                                                generic_parameters: None,
                                            },
                                            sway::TypeName::Identifier {
                                                name: "u64".to_string(),
                                                generic_parameters: None,
                                            },
                                            sway::TypeName::Identifier {
                                                name: "u64".to_string(),
                                                generic_parameters: None,
                                            },
                                        ],
                                    }),
                                }),
                            }),
                        }),
                        sway::Statement::from(sway::Expression::from(sway::If {
                            condition: Some(sway::Expression::from(sway::BinaryExpression {
                                operator: "||".to_string(),
                                lhs: sway::Expression::from(sway::BinaryExpression {
                                    operator: "!=".to_string(),
                                    lhs: sway::Expression::from(sway::MemberAccess {
                                        expression: sway::Expression::create_identifier(
                                            unique_variable_name.clone(),
                                        ),
                                        member: "0".to_string(),
                                    }),
                                    rhs: sway::Expression::from(sway::Literal::DecInt(
                                        BigUint::zero(),
                                        None,
                                    )),
                                }),
                                rhs: sway::Expression::from(sway::BinaryExpression {
                                    operator: "!=".to_string(),
                                    lhs: sway::Expression::from(sway::MemberAccess {
                                        expression: sway::Expression::create_identifier(
                                            unique_variable_name.clone(),
                                        ),
                                        member: "1".to_string(),
                                    }),
                                    rhs: sway::Expression::from(sway::Literal::DecInt(
                                        BigUint::zero(),
                                        None,
                                    )),
                                }),
                            })),
                            then_body: sway::Block {
                                statements: vec![sway::Statement::from(
                                    sway::Expression::create_function_calls(
                                        None,
                                        &[("revert", Some((None, vec![])))],
                                    ),
                                )],
                                final_expr: None,
                            },
                            else_if: None,
                        })),
                    ],
                    final_expr: Some(sway::Expression::create_function_calls(
                        None,
                        &[(
                            "U128::from",
                            Some((
                                None,
                                vec![sway::Expression::Tuple(vec![
                                    sway::Expression::from(sway::MemberAccess {
                                        expression: sway::Expression::create_identifier(
                                            unique_variable_name.clone(),
                                        ),
                                        member: "2".to_string(),
                                    }),
                                    sway::Expression::from(sway::MemberAccess {
                                        expression: sway::Expression::create_identifier(
                                            unique_variable_name.clone(),
                                        ),
                                        member: "3".to_string(),
                                    }),
                                ])],
                            )),
                        )],
                    )),
                }))
            }

            // Direct signed-to-unsigned conversion
            ("I8", 8) | ("I16", 16) | ("I32", 32) | ("I64", 64) | ("I128", 128) | ("I256", 256) => {
                Ok(sway::Expression::create_function_calls(
                    Some(value_expression),
                    &[("underlying", Some((None, vec![])))],
                ))
            }

            // Indirect signed-to-unsigned conversion
            // NOTE: this isn't converting between bits correctly
            //       we'll have to fix this eventually...
            ("I8", _) | ("I16", _) | ("I32", _) | ("I64", _) | ("I128", _) | ("I256", _) => {
                Ok(sway::Expression::create_function_calls(
                    Some(value_expression),
                    &[("underlying", Some((None, vec![])))],
                ))
            }

            ("b256", 256) => Ok(sway::Expression::create_function_calls(
                Some(value_expression),
                &[("as_u256", Some((None, vec![])))],
            )),

            ("Identity", 256) => {
                // if x.is_address() {
                //     b256::from(x.as_address().unwrap()).as_u256()
                // } else {
                //     b256::from(x.as_contract_id().unwrap()).as_u256()
                // }

                Ok(sway::Expression::from(sway::If {
                    condition: Some(sway::Expression::create_function_calls(
                        Some(value_expression.clone()),
                        &[("is_address", Some((None, vec![])))],
                    )),
                    then_body: sway::Block {
                        statements: vec![],
                        final_expr: Some(sway::Expression::create_function_calls(
                            None,
                            &[
                                (
                                    "b256::from",
                                    Some((
                                        None,
                                        vec![sway::Expression::create_function_calls(
                                            Some(value_expression.clone()),
                                            &[
                                                ("as_address", Some((None, vec![]))),
                                                ("unwrap", Some((None, vec![]))),
                                            ],
                                        )],
                                    )),
                                ),
                                ("as_u256", Some((None, vec![]))),
                            ],
                        )),
                    },
                    else_if: Some(Box::new(sway::If {
                        condition: None,
                        then_body: sway::Block {
                            statements: vec![],
                            final_expr: Some(sway::Expression::create_function_calls(
                                None,
                                &[
                                    (
                                        "b256::from",
                                        Some((
                                            None,
                                            vec![sway::Expression::create_function_calls(
                                                Some(value_expression.clone()),
                                                &[
                                                    ("as_contract_id", Some((None, vec![]))),
                                                    ("unwrap", Some((None, vec![]))),
                                                ],
                                            )],
                                        )),
                                    ),
                                    ("as_u256", Some((None, vec![]))),
                                ],
                            )),
                        },
                        else_if: None,
                    })),
                }))
            }

            ("todo!", _) => Ok(value_expression),

            _ => panic!(
                "{}translate from {value_type_name} to u{bits}: {value_expression:#?}",
                match project.loc_to_line_and_column(module.clone(), &argument.loc()) {
                    Some((line, col)) => format!(
                        "{}:{}:{}: ",
                        project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy(),
                        line,
                        col
                    ),
                    None => format!("{}: ", project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy()),
                },
            ),
        },

        sway::TypeName::Array {
            type_name: element_type_name,
            length,
        } => match element_type_name.as_ref() {
            sway::TypeName::Identifier {
                name,
                generic_parameters,
            } => match (name.as_str(), generic_parameters.as_ref()) {
                ("u8", None) if bits == 8 && *length == 1 => {
                    Ok(sway::Expression::from(sway::ArrayAccess {
                        expression: value_expression,
                        index: sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                    }))
                }

                ("u8", None) if bits == 256 && *length == 32 => {
                    module
                        .borrow_mut()
                        .ensure_use_declared("std::array_conversions::u256::*");
                    Ok(sway::Expression::create_function_calls(
                        None,
                        &[("u256::from_be_bytes", Some((None, vec![value_expression])))],
                    ))
                }

                ("u8", None) if *length < (bits / 8) => {
                    module.borrow_mut().ensure_use_declared(
                        format!("std::array_conversions::u{bits}::*").as_str(),
                    );

                    let mut elements = (0..*length)
                        .map(|i| {
                            sway::Expression::from(sway::ArrayAccess {
                                expression: value_expression.clone(),
                                index: sway::Expression::from(sway::Literal::DecInt(
                                    i.into(),
                                    None,
                                )),
                            })
                        })
                        .collect::<Vec<_>>();

                    elements.extend((0..((bits / 8) - *length)).map(|_| {
                        sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None))
                    }));

                    Ok(sway::Expression::create_function_calls(
                        None,
                        &[(
                            format!("u{bits}::from_be_bytes").as_str(),
                            Some((None, vec![sway::Expression::from(sway::Array { elements })])),
                        )],
                    ))
                }

                _ => {
                    todo!(
                        "translate cast from {value_type_name} to u{bits}: {} - {expression:#?}",
                        expression
                    )
                }
            },

            _ => todo!(
                "translate {value_type_name} type cast: {} - {expression:#?}",
                expression
            ),
        },

        _ => todo!(
            "translate {value_type_name} type cast: {} - {expression:#?}",
            expression
        ),
    }
}

/// Note: function argument is only needed for the loc debug in the end of the function
/// should be removed at some point
// bytesN(x) => ???
pub fn translate_bytes_type_cast_function_call(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: &Rc<RefCell<TranslationScope>>,
    argument: &solidity::Expression,
    byte_count: usize,
    function: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    let value_expression = translate_expression(project, module.clone(), scope, argument)?;
    let value_type_name = module
        .borrow_mut()
        .get_expression_type(scope, &value_expression)?;

    match &value_type_name {
        sway::TypeName::Undefined => panic!("Undefined type name"),

        sway::TypeName::Identifier {
            name,
            generic_parameters,
        } => match (name.as_str(), generic_parameters.as_ref()) {
            ("todo!", None) => {
                // HACK: don't try to do anything with todo! expressions...
                Ok(value_expression)
            }

            ("b256", None) => {
                // Ensure `std::bytes::Bytes` is imported
                module.borrow_mut().ensure_use_declared("std::bytes::Bytes");

                // Generate a unique name for our variable
                let variable_name = scope.borrow_mut().generate_unique_variable_name("bytes");

                Ok(sway::Expression::from(sway::Block {
                    statements: vec![
                        sway::Statement::from(sway::Let {
                            pattern: sway::LetPattern::from(sway::LetIdentifier {
                                is_mutable: false,
                                name: variable_name.clone(),
                            }),
                            type_name: None,
                            value: sway::Expression::create_function_calls(
                                None,
                                &[("Bytes::from", Some((None, vec![value_expression.clone()])))],
                            ),
                        }),
                        sway::Statement::from(sway::Let {
                            pattern: sway::LetPattern::from(vec![
                                sway::LetIdentifier {
                                    is_mutable: false,
                                    name: variable_name.clone(),
                                },
                                sway::LetIdentifier {
                                    is_mutable: false,
                                    name: "_".into(),
                                },
                            ]),
                            type_name: None,
                            value: sway::Expression::create_function_calls(
                                None,
                                &[
                                    (variable_name.as_str(), None),
                                    (
                                        "split_at",
                                        Some((
                                            None,
                                            vec![sway::Expression::from(sway::Literal::DecInt(
                                                BigUint::from(byte_count),
                                                None,
                                            ))],
                                        )),
                                    ),
                                ],
                            ),
                        }),
                    ],
                    final_expr: Some(sway::Expression::from(sway::Array {
                        elements: (0..byte_count)
                            .map(|index| {
                                sway::Expression::create_function_calls(
                                    None,
                                    &[
                                        (variable_name.as_str(), None),
                                        (
                                            "get",
                                            Some((
                                                None,
                                                vec![sway::Expression::from(
                                                    sway::Literal::DecInt(index.into(), None),
                                                )],
                                            )),
                                        ),
                                        ("unwrap", Some((None, vec![]))),
                                    ],
                                )
                            })
                            .collect(),
                    })),
                }))
            }

            ("u64", None) => {
                match value_expression {
                    sway::Expression::Literal(
                        sway::Literal::DecInt(value, None) | sway::Literal::HexInt(value, None),
                    ) if value.is_zero() => Ok(sway::Expression::from(sway::Array {
                        elements: (0..byte_count)
                            .map(|_| {
                                sway::Expression::from(sway::Literal::DecInt(
                                    0u8.into(),
                                    Some("u8".into()),
                                ))
                            })
                            .collect(),
                    })),

                    _ => {
                        // {
                        //     let b = x.to_be_bytes();
                        //     [b.get(0).unwrap(), b.get(1).unwrap(), ..., 0, 0, 0]
                        // }

                        let variable_name = scope.borrow_mut().generate_unique_variable_name("b");

                        Ok(sway::Expression::from(sway::Block {
                            statements: vec![sway::Statement::from(sway::Let {
                                pattern: sway::LetPattern::from(sway::LetIdentifier {
                                    is_mutable: false,
                                    name: variable_name.clone(),
                                }),
                                type_name: None,
                                value: sway::Expression::create_function_calls(
                                    Some(value_expression.clone()),
                                    &[("to_be_bytes", Some((None, vec![])))],
                                ),
                            })],
                            final_expr: Some(sway::Expression::from(sway::Array {
                                elements: (0..byte_count)
                                    .map(|index| {
                                        if index < 8 {
                                            sway::Expression::from(sway::ArrayAccess {
                                                expression: sway::Expression::create_identifier(
                                                    variable_name.clone(),
                                                ),
                                                index: sway::Expression::from(
                                                    sway::Literal::DecInt(index.into(), None),
                                                ),
                                            })
                                        } else {
                                            sway::Expression::from(sway::Literal::DecInt(
                                                0u8.into(),
                                                None,
                                            ))
                                        }
                                    })
                                    .collect(),
                            })),
                        }))
                    }
                }
            }

            ("u256", None) => {
                module
                    .borrow_mut()
                    .ensure_use_declared("std::array_conversions::u256::*");
                Ok(sway::Expression::create_function_calls(
                    Some(value_expression),
                    &[("to_be_bytes", Some((None, vec![])))],
                ))
            }

            ("Bytes", None) => Ok(sway::Expression::from(sway::Array {
                elements: (0..byte_count)
                    .map(|index| {
                        sway::Expression::create_function_calls(
                            Some(value_expression.clone()),
                            &[
                                (
                                    "get",
                                    Some((
                                        None,
                                        vec![sway::Expression::from(sway::Literal::DecInt(
                                            index.into(),
                                            None,
                                        ))],
                                    )),
                                ),
                                (
                                    "unwrap_or",
                                    Some((
                                        None,
                                        vec![sway::Expression::from(sway::Literal::DecInt(
                                            0u8.into(),
                                            Some("u8".into()),
                                        ))],
                                    )),
                                ),
                            ],
                        )
                    })
                    .collect(),
            })),

            ("raw_slice", None) => {
                let variable_name = scope.borrow_mut().generate_unique_variable_name("b");

                Ok(sway::Expression::from(sway::Block {
                    statements: vec![sway::Statement::from(sway::Let {
                        pattern: sway::LetPattern::from(sway::LetIdentifier {
                            is_mutable: false,
                            name: variable_name.clone(),
                        }),
                        type_name: None,
                        value: sway::Expression::create_function_calls(
                            None,
                            &[("Bytes::from", Some((None, vec![value_expression])))],
                        ),
                    })],
                    final_expr: Some(sway::Expression::from(sway::Array {
                        elements: (0..byte_count)
                            .map(|index| {
                                sway::Expression::create_function_calls(
                                    None,
                                    &[
                                        (variable_name.as_str(), None),
                                        (
                                            "get",
                                            Some((
                                                None,
                                                vec![sway::Expression::from(
                                                    sway::Literal::DecInt(index.into(), None),
                                                )],
                                            )),
                                        ),
                                        (
                                            "unwrap_or",
                                            Some((
                                                None,
                                                vec![sway::Expression::from(
                                                    sway::Literal::DecInt(
                                                        0u8.into(),
                                                        Some("u8".into()),
                                                    ),
                                                )],
                                            )),
                                        ),
                                    ],
                                )
                            })
                            .collect(),
                    })),
                }))
            }

            _ => panic!(
                "{}TODO: translate from {value_type_name} to bytes{byte_count}",
                match project.loc_to_line_and_column(module.clone(), &function.loc()) {
                    Some((line, col)) => format!(
                        "{}:{}:{}: ",
                        project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy(),
                        line,
                        col
                    ),
                    None => format!("{}: ", project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy()),
                },
            ),
        },

        sway::TypeName::StringSlice => match &value_expression {
            sway::Expression::Literal(sway::Literal::String(s)) => {
                let bytes = s.bytes().collect::<Vec<u8>>();

                Ok(sway::Expression::from(sway::Array {
                    elements: (0..byte_count)
                        .map(|index| {
                            sway::Expression::from(sway::Literal::HexInt(
                                if index < bytes.len() {
                                    bytes[index].into()
                                } else {
                                    0u8.into()
                                },
                                Some("u8".into()),
                            ))
                        })
                        .collect(),
                }))
            }

            _ => todo!("translate from {value_type_name} to bytes{byte_count}"),
        },

        _ => todo!("translate from {value_type_name} to bytes{byte_count}"),
    }
}

// bytes(x) => ???
pub fn translate_dynamic_bytes_type_cast_function_call(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: &Rc<RefCell<TranslationScope>>,
    argument: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    let value_expression = translate_expression(project, module.clone(), scope, argument)?;
    let value_type_name = module
        .borrow_mut()
        .get_expression_type(scope, &value_expression)?;

    match &value_type_name {
        sway::TypeName::Undefined => panic!("Undefined type name"),

        sway::TypeName::Identifier {
            name,
            generic_parameters,
        } => match (name.as_str(), generic_parameters.as_ref()) {
            ("String", None) => Ok(sway::Expression::create_function_calls(
                Some(value_expression),
                &[("as_bytes", Some((None, vec![])))],
            )),

            _ => todo!("translate from {value_type_name} to bytes"),
        },

        sway::TypeName::StringSlice | sway::TypeName::StringArray { .. } => {
            // Ensure `std::bytes::Bytes` is imported
            module.borrow_mut().ensure_use_declared("std::bytes::Bytes");

            // Generate a unique name for our variable
            let variable_name = scope.borrow_mut().generate_unique_variable_name("s");

            let value_expression = match &value_type_name {
                sway::TypeName::StringArray { .. } => sway::Expression::create_function_calls(
                    None,
                    &[("from_str_array", Some((None, vec![value_expression])))],
                ),

                _ => value_expression,
            };

            if let Some(variable_name) = value_expression.as_identifier() {
                return Ok(sway::Expression::create_function_calls(
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
                                                None,
                                                &[
                                                    (variable_name, None),
                                                    ("as_ptr", Some((None, vec![]))),
                                                ],
                                            ),
                                            sway::Expression::create_function_calls(
                                                None,
                                                &[
                                                    (variable_name, None),
                                                    ("len", Some((None, vec![]))),
                                                ],
                                            ),
                                        ],
                                    )),
                                )],
                            )],
                        )),
                    )],
                ));
            }

            Ok(sway::Expression::from(sway::Block {
                statements: vec![sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::from(sway::LetIdentifier {
                        is_mutable: false,
                        name: variable_name.clone(),
                    }),
                    type_name: None,
                    value: value_expression.clone(),
                })],
                final_expr: Some(sway::Expression::create_function_calls(
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
                                                None,
                                                &[
                                                    (variable_name.as_str(), None),
                                                    ("as_ptr", Some((None, vec![]))),
                                                ],
                                            ),
                                            sway::Expression::create_function_calls(
                                                None,
                                                &[
                                                    (variable_name.as_str(), None),
                                                    ("len", Some((None, vec![]))),
                                                ],
                                            ),
                                        ],
                                    )),
                                )],
                            )],
                        )),
                    )],
                )),
            }))
        }

        _ => todo!("translate from {value_type_name} to bytes"),
    }
}

// string(x) => ???
pub fn translate_string_type_cast_function_call(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: &Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
    argument: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    let value_expression = translate_expression(project, module.clone(), scope, argument)?;
    let value_type_name = module
        .borrow_mut()
        .get_expression_type(scope, &value_expression)?;

    match &value_type_name {
        sway::TypeName::Identifier {
            name,
            generic_parameters,
        } => match (name.as_str(), generic_parameters.as_ref()) {
            ("Bytes", None) => {
                // Ensure `std::string::*` is imported
                module.borrow_mut().ensure_use_declared("std::string::*");

                Ok(sway::Expression::create_function_calls(
                    None,
                    &[("String::from_ascii", Some((None, vec![value_expression])))],
                ))
            }

            _ => todo!(
                "translate {value_type_name} type cast: {} - {expression:#?}",
                expression
            ),
        },

        _ => todo!(
            "translate {value_type_name} type cast: {} - {expression:#?}",
            expression
        ),
    }
}

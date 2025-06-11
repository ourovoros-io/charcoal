use crate::{ir, project::Project, sway};
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
mod type_definitions;
mod type_names;
pub use assembly::*;
pub use contracts::*;
pub use enums::*;
pub use expressions::*;
pub use functions::*;
pub use import_directives::*;
pub use statements::*;
pub use storage::*;
pub use structs::*;
pub use type_definitions::*;
pub use type_names::*;

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

/// Coerces a argument type to a parameter type
pub fn coerce_expression(
    expression: &sway::Expression,
    from_type_name: &sway::TypeName,
    to_type_name: &sway::TypeName,
) -> Option<sway::Expression> {
    // println!(
    //     "Coercing from `{from_type_name}` to `{to_type_name}`: {}",
    //     sway::TabbedDisplayer(expression)
    // );

    if from_type_name.is_compatible_with(to_type_name) {
        return Some(expression.clone());
    }

    let is_uint = from_type_name.is_uint();
    let is_int = from_type_name.is_int();

    let mut expression = expression.clone();

    //
    // If `to_type_name` is `Identity`, but `expression` is an abi cast expression,
    // then we need to de-cast it, so `expression` turns into the 2nd parameter of the abi cast,
    // and `from_type_name` turns into `Identity`.
    //
    if let (
        _,
        sway::TypeName::Identifier {
            name,
            generic_parameters,
        },
    ) = (from_type_name, to_type_name)
    {
        match (name.as_str(), generic_parameters.as_ref()) {
            ("Identity", None) => match &expression {
                sway::Expression::FunctionCall(f) => {
                    if let Some(ident) = f.function.as_identifier() {
                        if ident == "abi" && f.parameters.len() == 2 {
                            let rhs = f.parameters[1].clone();
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
                                                                return Some(e.expression.clone());
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                _ => {}
            },

            _ => {}
        }
    }

    match (from_type_name, to_type_name) {
        (sway::TypeName::Undefined, sway::TypeName::Undefined) => {}

        (
            sway::TypeName::Identifier {
                name: lhs_name,
                generic_parameters: lhs_generic_parameters,
            },
            sway::TypeName::Identifier {
                name: rhs_name,
                generic_parameters: rhs_generic_parameters,
            },
        ) => {
            if lhs_generic_parameters.is_some() != rhs_generic_parameters.is_some() {
                // From `StorageKey<T>` to `T`
                if let Some(storage_key_type) = from_type_name.storage_key_type() {
                    if !storage_key_type.is_compatible_with(to_type_name) {
                        return None;
                    }

                    return Some(sway::Expression::create_function_calls(
                        Some(expression.clone()),
                        &[("read", Some((None, vec![])))],
                    ));
                }

                return None;
            }

            if let (Some(lhs_generic_parameters), Some(rhs_generic_parameters)) = (
                lhs_generic_parameters.as_ref(),
                rhs_generic_parameters.as_ref(),
            ) {
                if lhs_generic_parameters.entries.len() != rhs_generic_parameters.entries.len() {
                    return None;
                }
            }

            if lhs_name == rhs_name {
                return Some(expression.clone());
            }

            // {
            //     let mut v = Vec::new();
            //     let len = storage.storageVec.len();
            //     let mut i = 0;
            //     while i < len {
            //         v.push(storage.storageVec.get(i).unwrap().read())
            //         i += 1;
            //     }
            //     v
            // }
            if let Some(storage_key_type) = from_type_name.storage_key_type() {
                if let Some(storage_vec_type) = storage_key_type.storage_vec_type() {
                    if let Some(vec_type) = to_type_name.vec_type() {
                        // let unique_variable_name =
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
                        let element_expression =
                            coerce_expression(&get_expression, &storage_vec_type, &vec_type)
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
                                            sway::Statement::from(
                                                sway::Expression::create_function_calls(
                                                    None,
                                                    &[
                                                        ("v", None),
                                                        (
                                                            "push",
                                                            Some((
                                                                None,
                                                                vec![element_expression.clone()],
                                                            )),
                                                        ),
                                                    ],
                                                ),
                                            ),
                                            sway::Statement::from(sway::Expression::from(
                                                sway::BinaryExpression {
                                                    operator: "+=".to_string(),
                                                    lhs: sway::Expression::create_identifier(
                                                        "i".to_string(),
                                                    ),
                                                    rhs: sway::Expression::from(
                                                        sway::Literal::DecInt(BigUint::one(), None),
                                                    ),
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

            // From uint to int
            if from_type_name.is_uint() && !to_type_name.is_uint() {
                if to_type_name.is_int() {
                    let lhs_bits: usize = lhs_name
                        .trim_start_matches('u')
                        .trim_start_matches('U')
                        .trim_start_matches('I')
                        .parse()
                        .unwrap();
                    let rhs_bits: usize = rhs_name
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

                    expression = sway::Expression::create_function_calls(
                        None,
                        &[(
                            format!("I{rhs_bits}::from_uint").as_str(),
                            Some((None, vec![expression.clone()])),
                        )],
                    );
                } else {
                    return None;
                }
            }
            // From int to uint
            else if is_int && !to_type_name.is_int() {
                if to_type_name.is_uint() {
                    let lhs_bits: usize = lhs_name
                        .trim_start_matches('u')
                        .trim_start_matches('U')
                        .trim_start_matches('I')
                        .parse()
                        .unwrap();
                    let rhs_bits: usize = rhs_name
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

                    expression = sway::Expression::create_function_calls(
                        None,
                        &[(
                            format!("I{rhs_bits}::from_uint").as_str(),
                            Some((None, vec![expression.clone()])),
                        )],
                    );
                } else {
                    return None;
                }
            }
            // From uint/int of different bit lengths
            else if (is_uint && to_type_name.is_uint()) || (is_int && to_type_name.is_int()) {
                let lhs_bits: usize = lhs_name
                    .trim_start_matches('u')
                    .trim_start_matches('U')
                    .trim_start_matches('I')
                    .parse()
                    .unwrap();
                let rhs_bits: usize = rhs_name
                    .trim_start_matches('u')
                    .trim_start_matches('U')
                    .trim_start_matches('I')
                    .parse()
                    .unwrap();

                match &expression {
                    sway::Expression::Literal(sway::Literal::DecInt(i, suffix)) => {
                        if suffix.is_none() {
                            expression = sway::Expression::Literal(sway::Literal::DecInt(
                                i.clone(),
                                Some(format!("{}{}", rhs_name.chars().nth(0).unwrap(), rhs_bits)),
                            ));
                        } else {
                            if lhs_bits > rhs_bits {
                                // x.as_u256()
                                // u64::try_from(x).unwrap()
                                expression = sway::Expression::create_function_calls(
                                    None,
                                    &[
                                        (
                                            format!("{to_type_name}::try_from").as_str(),
                                            Some((None, vec![expression.clone()])),
                                        ),
                                        ("unwrap", Some((None, vec![]))),
                                    ],
                                );
                            } else if lhs_bits < rhs_bits {
                                expression = sway::Expression::create_function_calls(
                                    Some(expression.clone()),
                                    &[(
                                        format!("as_{to_type_name}").as_str(),
                                        Some((None, vec![])),
                                    )],
                                );
                            }
                        }
                    }
                    _ => {
                        if lhs_bits > rhs_bits {
                            // x.as_u256()
                            // u64::try_from(x).unwrap()
                            expression = sway::Expression::create_function_calls(
                                None,
                                &[
                                    (
                                        format!("{to_type_name}::try_from").as_str(),
                                        Some((None, vec![expression.clone()])),
                                    ),
                                    ("unwrap", Some((None, vec![]))),
                                ],
                            );
                        } else if lhs_bits < rhs_bits {
                            expression = sway::Expression::create_function_calls(
                                Some(expression.clone()),
                                &[(format!("as_{to_type_name}").as_str(), Some((None, vec![])))],
                            );
                        }
                    }
                }
            }
            // From StorageString to String
            else if lhs_name == "StorageString" && rhs_name == "String" {
                if let sway::Expression::FunctionCall(f) = expression {
                    if let sway::Expression::MemberAccess(member_access) = f.function {
                        if member_access.member == "read" {
                            expression = sway::Expression::create_function_calls(
                                Some(member_access.expression),
                                &[
                                    ("read_slice", Some((None, vec![]))),
                                    ("unwrap", Some((None, vec![]))),
                                ],
                            )
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                } else {
                    return None;
                }
            }
            // Do not allow incompatible types
            else if !from_type_name.is_compatible_with(to_type_name) {
                return None;
            }
        }

        (
            sway::TypeName::Array {
                type_name: lhs_type_name,
                length: lhs_len,
            },
            sway::TypeName::Array {
                type_name: rhs_type_name,
                length: rhs_len,
            },
        ) => {
            if lhs_len != rhs_len || !lhs_type_name.is_compatible_with(rhs_type_name) {
                todo!("Handle conversion from {from_type_name} to {to_type_name}")
            }

            match expression {
                sway::Expression::Array(array) => {
                    expression = sway::Expression::from(sway::Array {
                        elements: array
                            .elements
                            .iter()
                            .map(|e| coerce_expression(e, lhs_type_name, rhs_type_name).unwrap())
                            .collect(),
                    });
                }

                _ => {
                    expression = sway::Expression::from(sway::Array {
                        elements: (0..*rhs_len)
                            .map(|i| {
                                coerce_expression(
                                    &sway::Expression::from(sway::ArrayAccess {
                                        expression: expression.clone(),
                                        index: sway::Expression::from(sway::Literal::DecInt(
                                            i.into(),
                                            None,
                                        )),
                                    }),
                                    lhs_type_name,
                                    rhs_type_name,
                                )
                                .unwrap()
                            })
                            .collect(),
                    });
                }
            }
        }

        (
            sway::TypeName::Tuple {
                type_names: lhs_type_names,
            },
            sway::TypeName::Tuple {
                type_names: rhs_type_names,
            },
        ) => match &expression {
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
                        coerce_expression(&expr, &lhs_type_names[i], &rhs_type_names[i])
                    })
                    .collect::<Vec<_>>();

                if exprs.iter().any(|x| x.is_none()) {
                    return None;
                }

                expression = sway::Expression::from(sway::Block {
                    statements: vec![let_stmt],
                    final_expr: Some(sway::Expression::Tuple(
                        exprs.iter().flatten().cloned().collect(),
                    )),
                });
            }

            sway::Expression::Tuple(expressions) => {
                let mut expressions = expressions.clone();

                if expressions.len() != rhs_type_names.len() {
                    return None;
                }

                for (i, (lhs, rhs)) in lhs_type_names.iter().zip(rhs_type_names).enumerate() {
                    match coerce_expression(&expressions[i], lhs, rhs) {
                        Some(expr) => expressions[i] = expr,
                        None => return None,
                    }
                }

                expression = sway::Expression::Tuple(expressions);
            }

            _ => {
                return None;
            }
        },

        (sway::TypeName::StringSlice, sway::TypeName::StringSlice) => {}

        (
            sway::TypeName::StringArray { length: lhs_len },
            sway::TypeName::StringArray { length: rhs_len },
        ) => {
            if lhs_len != rhs_len {
                todo!("Handle coersion from str[{lhs_len}] to str[{rhs_len}]")
            }

            // otherwise it's the same length and we don't need to do anything
        }

        (
            sway::TypeName::Function {
                generic_parameters: _lhs_generic_parameters,
                parameters: _lhs_parameters_list,
                return_type: _lhs_return_type,
                ..
            },
            sway::TypeName::Function {
                generic_parameters: _rhs_generic_parameters,
                parameters: _rhs_parameters_list,
                return_type: _rhs_return_type,
                ..
            },
        ) => todo!(),

        (
            _,
            sway::TypeName::Identifier {
                name,
                generic_parameters,
            },
        ) => match (name.as_str(), generic_parameters.as_ref()) {
            ("Bytes", None) => match from_type_name {
                sway::TypeName::StringSlice => {
                    // Bytes::from(raw_slice::from_parts::<u8>((s.as_ptr(), s.len())))
                    expression = sway::Expression::create_function_calls(
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
                    );
                }

                _ => return None,
            },

            ("String", None) => match from_type_name {
                sway::TypeName::Identifier {
                    name,
                    generic_parameters,
                } => match (name.as_str(), generic_parameters.as_ref()) {
                    ("todo!", None) => {}
                    ("String", None) => {}
                    _ => todo!(),
                },

                // String::from_ascii_str(x)
                sway::TypeName::StringSlice => {
                    expression = sway::Expression::create_function_calls(
                        None,
                        &[(
                            "String::from_ascii_str",
                            Some((None, vec![expression.clone()])),
                        )],
                    );
                }

                // String::from_ascii_str(from_str_array(x))
                sway::TypeName::StringArray { .. } => {
                    expression = sway::Expression::create_function_calls(
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
                    );
                }

                _ => todo!("{}", sway::TabbedDisplayer(&to_type_name)),
            },

            _ => return None,
        },

        _ => return None,
    }

    Some(expression)
}

pub enum Symbol {
    TypeDefinition(String),
    Event(String),
    Enum(String),
    Error(String),
    Struct(String),
    Function(String),
    Abi(String),
}

pub fn resolve_symbol(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    symbol: Symbol,
) -> Option<Box<dyn std::any::Any>> {
    match &symbol {
        Symbol::TypeDefinition(name) => todo!(),
        Symbol::Event(name) => {
            if let Some(event_enum) = module
                .borrow()
                .events_enums
                .iter()
                .find(|e| e.0.borrow().variants.iter().any(|v| v.name == *name))
            {
                let variant = event_enum
                    .0
                    .borrow()
                    .variants
                    .iter()
                    .find(|v| v.name == *name)
                    .cloned()
                    .unwrap();

                return Some(Box::new((event_enum.0.borrow().name.clone(), variant)));
            }
        }
        Symbol::Enum(name) => todo!(),
        Symbol::Error(name) => todo!(),
        Symbol::Struct(name) => todo!(),
        Symbol::Function(name) => todo!(),
        Symbol::Abi(name) => todo!(),
    }

    for use_expr in module.borrow().uses.iter() {
        if let Some(imported_module) = project.resolve_use(use_expr) {
            return resolve_symbol(project, imported_module.clone(), symbol);
        }
    }

    None
}

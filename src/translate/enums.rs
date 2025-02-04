use std::{cell::RefCell, rc::Rc};

use super::{translate_type_name, TranslatedDefinition, TranslatedEnum};
use crate::{project::Project, sway, Error};
use convert_case::Case;
use num_bigint::BigUint;
use solang_parser::pt as solidity;

#[inline]
pub fn translate_enum_definition(
    _project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    enum_definition: &solidity::EnumDefinition,
) -> Result<(), Error> {
    // Create the enum's type definition
    let type_definition = sway::TypeDefinition {
        is_public: false,
        name: sway::TypeName::Identifier {
            name: enum_definition.name.as_ref().unwrap().name.clone(),
            generic_parameters: None,
        },
        underlying_type: Some(sway::TypeName::Identifier {
            name: "u8".into(),
            generic_parameters: None,
        }),
    };
    
    // Create the enum's variants impl block
    let mut variants_impl = sway::Impl {
        generic_parameters: None,
        type_name: type_definition.name.clone(),
        for_type_name: None,
        items: vec![],
    };

    // Add each variant to the variants impl block
    for (i, value) in enum_definition.values.iter().enumerate() {
        variants_impl.items.push(sway::ImplItem::Constant(sway::Constant {
            is_public: false,
            name: crate::translate_naming_convention(value.as_ref().unwrap().name.as_str(), Case::ScreamingSnake),
            type_name: type_definition.name.clone(),
            value: Some(sway::Expression::from(sway::Literal::DecInt(BigUint::from(i), None))),
        }));
    }

    // Add the translated enum to the translated definition
    translated_definition.enums.push(TranslatedEnum {
        type_definition,
        variants_impl,
    });

    Ok(())
}

#[inline]
pub fn translate_event_definition(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    event_definition: &solidity::EventDefinition,
) -> Result<(), Error> {
    let events_enum_name = format!("{}Event", translated_definition.name);

    let type_name = if event_definition.fields.len() == 1 {
        match translate_type_name(project, translated_definition, &event_definition.fields[0].ty, false, false) {
            sway::TypeName::Identifier { name, .. } if project.find_definition_with_abi(name.as_str()).is_some() => {
                sway::TypeName::Identifier {
                    name: "Identity".into(),
                    generic_parameters: None,
                }
            }

            type_name => type_name,
        }
    } else {
        sway::TypeName::Tuple {
            type_names: event_definition.fields.iter().map(|f| {
                match translate_type_name(project, translated_definition, &f.ty, false, false) {
                    sway::TypeName::Identifier { name, .. } if project.find_definition_with_abi(name.as_str()).is_some() => {
                        sway::TypeName::Identifier {
                            name: "Identity".into(),
                            generic_parameters: None,
                        }
                    }

                    type_name => type_name,
                }
            }).collect(),
        }
    };

    let (events_enum, _) = {
        if !translated_definition.events_enums.iter().any(|(e, _)| e.borrow().name == events_enum_name) {
            translated_definition.events_enums.push((
                Rc::new(RefCell::new(sway::Enum {
                    name: events_enum_name.clone(),
                    ..Default::default()
                })),
                Rc::new(RefCell::new(sway::Impl {
                    type_name: sway::TypeName::Identifier {
                        name: "AbiEncode".into(),
                        generic_parameters: None,
                    },
                    for_type_name: Some(sway::TypeName::Identifier {
                        name: events_enum_name.clone(),
                        generic_parameters: None,
                    }),
                    ..Default::default()
                }))
            ));
        }

        translated_definition.events_enums.iter_mut().find(|(e, _)| e.borrow().name == events_enum_name).unwrap()
    };

    let variant = sway::EnumVariant {
        name: event_definition.name.as_ref().unwrap().name.clone(),
        type_name,
    };

    if !events_enum.borrow().variants.contains(&variant) {
        events_enum.borrow_mut().variants.push(variant);
    }

    Ok(())
}

#[inline]
pub fn translate_error_definition(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    error_definition: &solidity::ErrorDefinition,
) -> Result<(), Error> {
    let errors_enum_name = format!("{}Error", translated_definition.name);

    let type_name = if error_definition.fields.len() == 1 {
        translate_type_name(project, translated_definition, &error_definition.fields[0].ty, false, false)
    } else {
        sway::TypeName::Tuple {
            type_names: error_definition.fields.iter().map(|f| {
                translate_type_name(project, translated_definition, &f.ty, false, false)
            }).collect(),
        }
    };

    let (errors_enum, _) = {
        if !translated_definition.errors_enums.iter().any(|(e, _)| e.borrow().name == errors_enum_name) {
            translated_definition.errors_enums.push((
                Rc::new(RefCell::new(sway::Enum {
                    name: errors_enum_name.clone(),
                    ..Default::default()
                })),
                Rc::new(RefCell::new(sway::Impl {
                    type_name: sway::TypeName::Identifier {
                        name: "AbiEncode".into(),
                        generic_parameters: None,
                    },
                    for_type_name: Some(sway::TypeName::Identifier {
                        name: errors_enum_name.clone(),
                        generic_parameters: None,
                    }),
                    ..Default::default()
                }))
            ));
        }

        translated_definition.errors_enums.iter_mut().find(|(e, _)| e.borrow().name == errors_enum_name).unwrap()
    };

    let variant = sway::EnumVariant {
        name: error_definition.name.as_ref().unwrap().name.clone(),
        type_name,
    };

    if !errors_enum.borrow().variants.contains(&variant) {
        errors_enum.borrow_mut().variants.push(variant);
    }

    Ok(())
}

#[inline]
pub fn generate_enum_abi_encode_function(
    _project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    sway_enum: Rc<RefCell<sway::Enum>>,
    abi_encode_impl: Rc<RefCell<sway::Impl>>,
) -> Result<(), Error> {
    let mut match_expr = sway::Match {
        expression: sway::Expression::Identifier("self".into()),
        branches: vec![],
    };

    for variant in sway_enum.borrow().variants.clone() {
        let mut block = sway::Block::default();

        let mut add_encode_statement_to_block = |name: &str, type_name: &sway::TypeName| {
            block.statements.push(sway::Statement::Let(sway::Let {
                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                    is_mutable: false,
                    name: "buffer".into(),
                }),
                type_name: None,
                value: match type_name {
                    sway::TypeName::Identifier { name: type_name, .. } => match type_name.as_str() {
                        "bool" | "I8" | "I16" | "I32" | "I64" | "I128" | "I256" | "u8" | "u16" | "u32" | "u64" | "u256" | "b256" | "Bytes" | "Vec" => {
                            sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::from(sway::MemberAccess {
                                    expression: sway::Expression::Identifier(name.into()),
                                    member: "abi_encode".into(),
                                }),
                                generic_parameters: None,
                                parameters: vec![
                                    sway::Expression::Identifier("buffer".into()),
                                ],
                            })
                        }
    
                        "Identity" => {
                            let identity_variant_branch = |name: &str| -> sway::MatchBranch {
                                sway::MatchBranch {
                                    pattern: sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::Identifier(format!("Identity::{name}")),
                                        generic_parameters: None,
                                        parameters: vec![
                                            sway::Expression::Identifier("x".into()),
                                        ],
                                    }),
                                    value: sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::from(sway::MemberAccess {
                                            expression: sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::from(sway::MemberAccess {
                                                    expression: sway::Expression::Identifier("x".into()),
                                                    member: "bits".into(),
                                                }),
                                                generic_parameters: None,
                                                parameters: vec![],
                                            }),
                                            member: "abi_encode".into(),
                                        }),
                                        generic_parameters: None,
                                        parameters: vec![
                                            sway::Expression::Identifier("buffer".into())
                                        ],
                                    }),
                                }
                            };
    
                            sway::Expression::from(sway::Match {
                                expression: sway::Expression::Identifier(name.into()),
                                branches: vec![
                                    identity_variant_branch("Address"),
                                    identity_variant_branch("ContractId"),
                                ],
                            })
                        },
    
                        _ => todo!("encode enum member type: {type_name}"),
                    }
                    
                    sway::TypeName::Array { .. } | sway::TypeName::StringSlice => sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::from(sway::MemberAccess {
                            expression: sway::Expression::Identifier(name.into()),
                            member: "abi_encode".into(),
                        }),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::Identifier("buffer".into()),
                        ],
                    }),
    
                    _ => todo!("ABI encoding for enum parameter type: {type_name}"),
                },
            }));
        };

        let parameter_count = match &variant.type_name {
            sway::TypeName::Tuple { type_names } => type_names.len(),
            _ => 1,
        };

        let parameter_names: Vec<String> = ('a'..='z').enumerate()
            .take_while(|(i, _)| *i < parameter_count)
            .map(|(_, c)| c.into())
            .collect();

        match &variant.type_name {
            sway::TypeName::Undefined => panic!("Undefined type name"),
            
            sway::TypeName::Identifier { .. } => {
                let type_name = translated_definition.get_underlying_type(&variant.type_name);
                add_encode_statement_to_block(&parameter_names[0], &type_name);
            }
            
            sway::TypeName::Tuple { type_names } => {
                for (name, type_name) in parameter_names.iter().zip(type_names) {
                    let type_name = translated_definition.get_underlying_type(type_name);
                    add_encode_statement_to_block(name.as_str(), &type_name);
                }
            }

            sway::TypeName::StringSlice => {
                let type_name = translated_definition.get_underlying_type(&variant.type_name);
                add_encode_statement_to_block(&parameter_names[0], &type_name);
            }

            sway::TypeName::Array { type_name, .. } => {
                let type_name = translated_definition.get_underlying_type(type_name.as_ref());
                add_encode_statement_to_block(&parameter_names[0], &type_name);
            }

            type_name => todo!("ABI encoding for enum parameter type: {type_name}"),
        }

        if block.statements.len() == 1 {
            let Some(sway::Statement::Let(let_stmt)) = block.statements.pop() else { unreachable!() };
            block.final_expr = Some(let_stmt.value);
        } else {
            block.final_expr = Some(sway::Expression::Identifier("buffer".into()));
        }
        
        block.statements.insert(0, sway::Statement::from(sway::Let { 
            pattern: sway::LetPattern::Identifier(sway::LetIdentifier { 
                is_mutable: false,
                name: "buffer".into()
            }),
            type_name: None,
            value: sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::from(sway::MemberAccess {
                    expression: sway::Expression::from(sway::Literal::String(variant.name.clone())),
                    member: "abi_encode".into(),
                }),
                generic_parameters: None,
                parameters: vec![
                    sway::Expression::Identifier("buffer".into()),
                ],
            }),
        }));

        match_expr.branches.push(sway::MatchBranch {
            pattern: sway::Expression::Identifier(format!(
                "{}::{}{}",
                sway_enum.borrow().name,
                variant.name,
                if parameter_count == 0 {
                    String::new()
                } else if parameter_count == 1 {
                    format!("({})", parameter_names[0])
                } else {
                    format!("(({}))", parameter_names.join(", "))
                },
            )),
            value: sway::Expression::from(block),
        });
    }

    // Add the `abi_encode` function to the `core::codec::AbiEncode` impl
    abi_encode_impl.borrow_mut().items.push(sway::ImplItem::Function(sway::Function {
        attributes: None,
        is_public: false,
        name: "abi_encode".into(),
        generic_parameters: None,
        parameters: sway::ParameterList {
            entries: vec![
                sway::Parameter {
                    name: "self".into(),
                    type_name: None,
                    ..Default::default()
                },
                sway::Parameter {
                    is_ref: false,
                    is_mut: false,
                    name: "buffer".into(),
                    type_name: Some(sway::TypeName::Identifier {
                        name: "Buffer".into(),
                        generic_parameters: None,
                    }),
                },
            ],
        },
        return_type: Some(sway::TypeName::Identifier {
            name: "Buffer".into(),
            generic_parameters: None,
        }),
        body: Some(sway::Block {
            statements: vec![
                sway::Statement::Let(sway::Let {
                    pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                        is_mutable: false,
                        name: "buffer".into(),
                    }),
                    type_name: None,
                    value: sway::Expression::from(match_expr),
                }),
            ],
            final_expr: Some(sway::Expression::Identifier("buffer".into())),
        }),
    }));

    Ok(())
}

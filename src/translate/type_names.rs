use crate::{project::Project, sway, translate::*};
use solang_parser::{helpers::CodeLocation, pt as solidity};
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_return_type_name(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
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

        _ => {
            todo!();
            // Check if the parameter's type is an ABI and make it an Identity
            // if let sway::TypeName::Identifier { name, generic_parameters: None } = &type_name {
            //     if project.find_definition_with_abi(name.as_str()).is_some() {
            //         return sway::TypeName::Identifier {
            //             name: "Identity".into(),
            //             generic_parameters: None,
            //         };
            //     }
            // }

            type_name.clone()
        }
    }
}

pub fn translate_type_name(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    type_name: &solidity::Expression,
    is_storage: bool,
    is_parameter: bool,
) -> sway::TypeName {
    match type_name {
        solidity::Expression::Type(_, type_expression) => match type_expression {
            solidity::Type::Address => sway::TypeName::Identifier {
                name: "Identity".into(),
                generic_parameters: None,
            },

            // TODO: should we note that this address was marked payable?
            solidity::Type::AddressPayable => sway::TypeName::Identifier {
                name: "Identity".into(),
                generic_parameters: None,
            },

            solidity::Type::Payable => todo!("payable types (used for casting)"),

            solidity::Type::Bool => sway::TypeName::Identifier {
                name: "bool".into(),
                generic_parameters: None,
            },

            solidity::Type::String => {
                if is_storage {
                    // Ensure `std::storage::storage_string::*` is imported
                    module
                        .borrow_mut()
                        .ensure_use_declared("std::storage::storage_string::*");

                    sway::TypeName::Identifier {
                        name: "StorageString".into(),
                        generic_parameters: None,
                    }
                } else {
                    sway::TypeName::StringSlice
                }
            }

            solidity::Type::Int(bits) => {
                module.borrow_mut().ensure_dependency_declared(
                    "sway_libs = { git = \"https://github.com/FuelLabs/sway-libs\", tag = \"v0.25.2\" }"
                );

                sway::TypeName::Identifier {
                    name: match *bits {
                        0..=8 => {
                            if *bits != 8 {
                                eprintln!(
                                    "WARNING: unsupported signed integer type `int{bits}`, using `I8`..."
                                );
                            }
                            module
                                .borrow_mut()
                                .ensure_use_declared("sway_libs::signed_integers::i8::*");
                            "I8".into()
                        }
                        9..=16 => {
                            if *bits != 16 {
                                eprintln!(
                                    "WARNING: unsupported signed integer type `int{bits}`, using `I16`..."
                                );
                            }
                            module
                                .borrow_mut()
                                .ensure_use_declared("sway_libs::signed_integers::i16::*");
                            "I16".into()
                        }
                        17..=32 => {
                            if *bits != 32 {
                                eprintln!(
                                    "WARNING: unsupported signed integer type `int{bits}`, using `I32`..."
                                );
                            }
                            module
                                .borrow_mut()
                                .ensure_use_declared("sway_libs::signed_integers::i32::*");
                            "I32".into()
                        }
                        33..=64 => {
                            if *bits != 64 {
                                eprintln!(
                                    "WARNING: unsupported signed integer type `int{bits}`, using `I64`..."
                                );
                            }
                            module
                                .borrow_mut()
                                .ensure_use_declared("sway_libs::signed_integers::i64::*");
                            "I64".into()
                        }
                        65..=128 => {
                            if *bits != 128 {
                                eprintln!(
                                    "WARNING: unsupported signed integer type `int{bits}`, using `I128`..."
                                );
                            }
                            module
                                .borrow_mut()
                                .ensure_use_declared("sway_libs::signed_integers::i128::*");
                            "I128".into()
                        }
                        129..=256 => {
                            if *bits != 256 {
                                eprintln!(
                                    "WARNING: unsupported signed integer type `int{bits}`, using `I256`..."
                                );
                            }
                            module
                                .borrow_mut()
                                .ensure_use_declared("sway_libs::signed_integers::i256::*");
                            "I256".into()
                        }
                        _ => panic!("Invalid uint type: {bits}"),
                    },
                    generic_parameters: None,
                }
            }

            solidity::Type::Uint(bits) => sway::TypeName::Identifier {
                name: match *bits {
                    0..=8 => {
                        if *bits != 8 {
                            eprintln!(
                                "WARNING: unsupported unsigned integer type `uint{bits}`, using `u8`..."
                            );
                        }
                        "u8".into()
                    }
                    9..=16 => {
                        if *bits != 16 {
                            eprintln!(
                                "WARNING: unsupported unsigned integer type `uint{bits}`, using `u16`..."
                            );
                        }
                        "u16".into()
                    }
                    17..=32 => {
                        if *bits != 32 {
                            eprintln!(
                                "WARNING: unsupported unsigned integer type `uint{bits}`, using `u32`..."
                            );
                        }
                        "u32".into()
                    }
                    33..=64 => {
                        if *bits != 64 {
                            eprintln!(
                                "WARNING: unsupported unsigned integer type `uint{bits}`, using `u64`..."
                            );
                        }
                        "u64".into()
                    }
                    65..=256 => {
                        if *bits != 256 {
                            eprintln!(
                                "WARNING: unsupported unsigned integer type `uint{bits}`, using `u256`..."
                            );
                        }
                        "u256".into()
                    }
                    _ => panic!("Invalid uint type: {bits}"),
                },
                generic_parameters: None,
            },

            solidity::Type::Bytes(length) => match *length {
                // HACK: bytes32 => b256
                32 => sway::TypeName::Identifier {
                    name: "b256".into(),
                    generic_parameters: None,
                },

                _ => sway::TypeName::Array {
                    type_name: Box::new(sway::TypeName::Identifier {
                        name: "u8".into(),
                        generic_parameters: None,
                    }),
                    length: *length as usize,
                },
            },

            solidity::Type::Rational => todo!("rational types"),

            solidity::Type::DynamicBytes => sway::TypeName::Identifier {
                name: {
                    // Ensure `std::bytes::Bytes` is imported
                    module.borrow_mut().ensure_use_declared("std::bytes::Bytes");

                    "Bytes".into() // TODO: is this ok?
                },
                generic_parameters: None,
            },

            solidity::Type::Mapping { key, value, .. } => {
                // Ensure `std::hash::Hash` is imported
                module.borrow_mut().ensure_use_declared("std::hash::Hash");

                if is_parameter {
                    sway::TypeName::Identifier {
                        name: "StorageKey".into(),
                        generic_parameters: Some(sway::GenericParameterList {
                            entries: vec![sway::GenericParameter {
                                type_name: sway::TypeName::Identifier {
                                    name: "StorageMap".into(),
                                    generic_parameters: Some(sway::GenericParameterList {
                                        entries: vec![
                                            sway::GenericParameter {
                                                type_name: translate_type_name(
                                                    project,
                                                    module.clone(),
                                                    key.as_ref(),
                                                    is_storage,
                                                    is_parameter,
                                                ),
                                                implements: None,
                                            },
                                            sway::GenericParameter {
                                                type_name: translate_type_name(
                                                    project,
                                                    module,
                                                    value.as_ref(),
                                                    is_storage,
                                                    is_parameter,
                                                ),
                                                implements: None,
                                            },
                                        ],
                                    }),
                                },
                                implements: None,
                            }],
                        }),
                    }
                } else {
                    sway::TypeName::Identifier {
                        name: "StorageMap".into(),
                        generic_parameters: Some(sway::GenericParameterList {
                            entries: vec![
                                sway::GenericParameter {
                                    type_name: translate_type_name(
                                        project,
                                        module.clone(),
                                        key.as_ref(),
                                        is_storage,
                                        is_parameter,
                                    ),
                                    implements: None,
                                },
                                sway::GenericParameter {
                                    type_name: translate_type_name(
                                        project,
                                        module,
                                        value.as_ref(),
                                        is_storage,
                                        is_parameter,
                                    ),
                                    implements: None,
                                },
                            ],
                        }),
                    }
                }
            }

            solidity::Type::Function {
                params, returns, ..
            } => sway::TypeName::Function {
                generic_parameters: None,
                parameters: sway::ParameterList {
                    entries: params
                        .iter()
                        .map(|(_, p)| {
                            p.as_ref()
                                .map(|p| sway::Parameter {
                                    is_ref: false,
                                    is_mut: false,
                                    name: p.name.as_ref().map_or("_".into(), |n| n.name.clone()),
                                    type_name: Some(translate_type_name(
                                        project,
                                        module.clone(),
                                        &p.ty,
                                        false,
                                        true,
                                    )),
                                })
                                .unwrap_or_else(|| sway::Parameter {
                                    is_ref: false,
                                    is_mut: false,
                                    name: "_".into(),
                                    type_name: None,
                                })
                        })
                        .collect(),
                },
                return_type: {
                    let returns: Option<Vec<sway::TypeName>> = returns.as_ref().map(|r| {
                        r.0.iter()
                            .map(|(_, p)| match p.as_ref() {
                                Some(p) => {
                                    translate_type_name(project, module.clone(), &p.ty, false, true)
                                }
                                None => sway::TypeName::Identifier {
                                    name: "_".into(),
                                    generic_parameters: None,
                                },
                            })
                            .collect()
                    });

                    match returns.as_ref() {
                        Some(returns) => match returns.len() {
                            0 => None,
                            1 => Some(Box::new(returns[0].clone())),
                            _ => Some(Box::new(sway::TypeName::Tuple {
                                type_names: returns.clone(),
                            })),
                        },
                        None => None,
                    }
                },
            },
        },

        solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
            // Check if type is a type definition
            if module.borrow().type_definitions.iter().any(|t| matches!(&t.implementation.as_ref().unwrap().name, sway::TypeName::Identifier { name: type_name, generic_parameters: None } if type_name == name)) {
                return sway::TypeName::Identifier {
                    name: name.clone(),
                    generic_parameters: None,
                };
            }

            // Check if type is a struct
            if module
                .borrow()
                .structs
                .iter()
                .any(|n| n.implementation.as_ref().unwrap().borrow().name == *name)
            {
                return sway::TypeName::Identifier {
                    name: name.clone(),
                    generic_parameters: None,
                };
            }

            // Check if type is an enum
            if module.borrow().enums.iter().any(|t| {
                match &t.implementation.as_ref().unwrap().type_definition.name {
                    sway::TypeName::Identifier {
                        name: type_name,
                        generic_parameters: None,
                    } => type_name == name,
                    _ => false,
                }
            }) {
                return sway::TypeName::Identifier {
                    name: name.clone(),
                    generic_parameters: None,
                };
            }

            // Check if type is an ABI
            // if let Some(external_definition) = project.find_definition_with_abi(name.as_str()) {
            //     // Ensure the ABI is added to the current definition
            //     if !module.borrow().abis.iter().any(|a| a.name == *name) {
            //         module.borrow_mut().abis.push(external_definition.abi.as_ref().unwrap().clone());
            //     }

            //     return sway::TypeName::Identifier {
            //         name: external_definition.name.clone(),
            //         generic_parameters: None,
            //     };
            // }

            todo!(
                "{} - translate variable type expression: {} - {type_name:#?}",
                match project.loc_to_line_and_column(module.clone(), &type_name.loc()) {
                    Some((line, col)) => format!(
                        "{}:{}:{}",
                        module.borrow().path.to_string_lossy(),
                        line,
                        col
                    ),
                    None => format!("{}", module.borrow().path.to_string_lossy()),
                },
                type_name.to_string(),
            )
        }

        solidity::Expression::ArraySubscript(_, type_name, length) => match length.as_ref() {
            Some(length) => sway::TypeName::Array {
                type_name: Box::new(translate_type_name(
                    project,
                    module,
                    type_name,
                    is_storage,
                    is_parameter,
                )),
                length: {
                    // Create an empty scope to translate the array length expression
                    todo!()
                    // let scope = Rc::new(RefCell::new(TranslationScope {
                    //     parent: Some(module.toplevel_scope.clone()),
                    //     ..Default::default()
                    // }));

                    // match translate_expression(project, module, &scope, length.as_ref()) {
                    //     Ok(sway::Expression::Literal(sway::Literal::DecInt(length, _) | sway::Literal::HexInt(length, _))) => length.try_into().unwrap(),
                    //     Ok(_) => panic!("Invalid array length expression: {length:#?}"),
                    //     Err(e) => panic!("Failed to translate array length expression: {e}"),
                    // }
                },
            },

            None => sway::TypeName::Identifier {
                name: if is_storage {
                    // Ensure that `std::storage::storage_vec::*` is imported
                    module
                        .borrow_mut()
                        .ensure_use_declared("std::storage::storage_vec::*");

                    "StorageVec".into()
                } else {
                    "Vec".into()
                },
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![sway::GenericParameter {
                        type_name: translate_type_name(
                            project,
                            module,
                            type_name,
                            is_storage,
                            is_parameter,
                        ),
                        implements: None,
                    }],
                }),
            },
        },

        solidity::Expression::MemberAccess(_, container, member) => match container.as_ref() {
            solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
                let mut result = None;
                let mut translated_enum = None;
                let mut translated_struct = None;
                let mut translated_type = None;

                let mut check_definition = |external_definition: Rc<RefCell<TranslatedModule>>| {
                    // Check to see if member is an enum
                    if let Some(external_enum) =
                        external_definition.borrow().enums.iter().find(|e| {
                            let sway::TypeName::Identifier {
                                name,
                                generic_parameters: None,
                            } = &e.implementation.as_ref().unwrap().type_definition.name
                            else {
                                panic!(
                                    "Expected Identifier type name, found {:#?}",
                                    e.implementation.as_ref().unwrap().type_definition.name
                                );
                            };

                            *name == member.name
                        })
                    {
                        translated_enum =
                            Some(external_enum.implementation.as_ref().unwrap().clone());
                        result = Some(
                            external_enum
                                .implementation
                                .as_ref()
                                .unwrap()
                                .type_definition
                                .name
                                .clone(),
                        );
                    }
                    // Check to see if member is a struct
                    else if let Some(external_struct) =
                        external_definition.borrow().structs.iter().find(|s| {
                            s.implementation.as_ref().unwrap().borrow().name == member.name
                        })
                    {
                        translated_struct =
                            Some(external_struct.implementation.as_ref().unwrap().clone());
                        result = Some(sway::TypeName::Identifier {
                            name: external_struct
                                .implementation
                                .as_ref()
                                .unwrap()
                                .borrow()
                                .name
                                .clone(),
                            generic_parameters: None,
                        });
                    }
                    // Check to see if member is a user-defined type
                    else if let Some(external_type) = external_definition
                        .borrow()
                        .type_definitions
                        .iter()
                        .find(|t| match &t.implementation.as_ref().unwrap().name {
                            sway::TypeName::Identifier {
                                name,
                                generic_parameters: None,
                            } => *name == member.name,
                            _ => false,
                        })
                    {
                        translated_type =
                            Some(external_type.implementation.as_ref().unwrap().clone());
                        result = Some(external_type.implementation.as_ref().unwrap().name.clone());
                    }
                };

                // Check to see if container is referring to the current definition
                if name == &module.borrow().name {
                    check_definition(module.clone());
                }
                // Check to see if container is referring to an external definition
                else if let Some(external_definition) = project
                    .translated_modules
                    .iter()
                    .find(|d| d.borrow().name == *name)
                {
                    check_definition(external_definition.clone());
                }

                // if let Some(type_name) = result {
                //     if let Some(translated_enum) = translated_enum {
                //         module.add_enum(&translated_enum);
                //     } else if let Some(translated_struct) = translated_struct {
                //         module.ensure_struct_included(project, &translated_struct);
                //     } else if let Some(translated_type) = translated_type {
                //         if !module.type_definitions.contains(&translated_type) {
                //             module.type_definitions.push(translated_type);
                //         }
                //     }

                //     return type_name;
                // }

                todo!(
                    "{} - member access type name expression: {type_name}",
                    match project.loc_to_line_and_column(module.clone(), &type_name.loc()) {
                        Some((line, col)) => format!(
                            "{}:{}:{}",
                            module.borrow().path.to_string_lossy(),
                            line,
                            col
                        ),
                        None => format!("{}", module.borrow().path.to_string_lossy()),
                    },
                )
            }

            _ => todo!(
                "{} - member access type name expression: {type_name}",
                match project.loc_to_line_and_column(module.clone(), &type_name.loc()) {
                    Some((line, col)) => format!(
                        "{}:{}:{}",
                        module.borrow().path.to_string_lossy(),
                        line,
                        col
                    ),
                    None => format!("{}", module.borrow().path.to_string_lossy()),
                },
            ),
        },

        _ => unimplemented!(
            "{} - type name expression: {type_name}",
            match project.loc_to_line_and_column(module.clone(), &type_name.loc()) {
                Some((line, col)) => format!(
                    "{}:{}:{}",
                    module.borrow().path.to_string_lossy(),
                    line,
                    col
                ),
                None => format!("{}", module.borrow().path.to_string_lossy()),
            },
        ),
    }
}

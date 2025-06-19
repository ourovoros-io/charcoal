use crate::{ir, project::Project, sway, translate::*};
use solang_parser::{helpers::CodeLocation, pt as solidity};
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_type_definition(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    contract_name: Option<&str>,
    type_definition: &solidity::TypeDefinition,
) -> Result<sway::TypeDefinition, Error> {
    let scope = Rc::new(RefCell::new(ir::Scope::new(contract_name, None)));

    let underlying_type =
        translate_type_name(project, module.clone(), scope, &type_definition.ty, None);

    Ok(sway::TypeDefinition {
        is_public: true,
        name: sway::TypeName::Identifier {
            name: type_definition.name.name.clone(),
            generic_parameters: None,
        },
        underlying_type: Some(underlying_type),
    })
}

pub fn translate_type_name(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    type_name: &solidity::Expression,
    storage_location: Option<&solidity::StorageLocation>,
) -> sway::TypeName {
    let mut type_name = match type_name {
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

            solidity::Type::Payable => {
                println!("WARNING: encountered payable cast, translating as Identity");

                sway::TypeName::Identifier {
                    name: "Identity".into(),
                    generic_parameters: None,
                }
            }

            solidity::Type::Bool => sway::TypeName::Identifier {
                name: "bool".into(),
                generic_parameters: None,
            },

            solidity::Type::String => {
                match storage_location {
                    Some(storage_location) => match storage_location {
                        solidity::StorageLocation::Memory(_)
                        | solidity::StorageLocation::Calldata(_) => {
                            // Ensure `std::string::*` is imported
                            module.borrow_mut().ensure_use_declared("std::string::*");

                            sway::TypeName::Identifier {
                                name: "String".into(),
                                generic_parameters: None,
                            }
                        }

                        solidity::StorageLocation::Storage(_) => {
                            // Ensure `std::storage::storage_string::*` is imported
                            module
                                .borrow_mut()
                                .ensure_use_declared("std::storage::storage_string::*");

                            sway::TypeName::Identifier {
                                name: "StorageString".into(),
                                generic_parameters: None,
                            }
                        }
                    },

                    None => sway::TypeName::StringSlice,
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
                                    "{}: WARNING: unsupported signed integer type `int{bits}`, using `I8`...",
                                    project.loc_to_file_location_string(
                                        module.clone(),
                                        &type_name.loc()
                                    ),
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
                                    "{}: WARNING: unsupported signed integer type `int{bits}`, using `I16`...",
                                    project.loc_to_file_location_string(
                                        module.clone(),
                                        &type_name.loc()
                                    ),
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
                                    "{}: WARNING: unsupported signed integer type `int{bits}`, using `I32`...",
                                    project.loc_to_file_location_string(
                                        module.clone(),
                                        &type_name.loc()
                                    ),
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
                                    "{}: WARNING: unsupported signed integer type `int{bits}`, using `I64`...",
                                    project.loc_to_file_location_string(
                                        module.clone(),
                                        &type_name.loc()
                                    ),
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
                                    "{}: WARNING: unsupported signed integer type `int{bits}`, using `I128`...",
                                    project.loc_to_file_location_string(
                                        module.clone(),
                                        &type_name.loc()
                                    ),
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
                                    "{}: WARNING: unsupported signed integer type `int{bits}`, using `I256`...",
                                    project.loc_to_file_location_string(
                                        module.clone(),
                                        &type_name.loc()
                                    ),
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
                                "{}: WARNING: unsupported unsigned integer type `uint{bits}`, using `u8`...",
                                project
                                    .loc_to_file_location_string(module.clone(), &type_name.loc()),
                            );
                        }
                        "u8".into()
                    }
                    9..=16 => {
                        if *bits != 16 {
                            eprintln!(
                                "{}: WARNING: unsupported unsigned integer type `uint{bits}`, using `u16`...",
                                project
                                    .loc_to_file_location_string(module.clone(), &type_name.loc()),
                            );
                        }
                        "u16".into()
                    }
                    17..=32 => {
                        if *bits != 32 {
                            eprintln!(
                                "{}: WARNING: unsupported unsigned integer type `uint{bits}`, using `u32`...",
                                project
                                    .loc_to_file_location_string(module.clone(), &type_name.loc()),
                            );
                        }
                        "u32".into()
                    }
                    33..=64 => {
                        if *bits != 64 {
                            eprintln!(
                                "{}: WARNING: unsupported unsigned integer type `uint{bits}`, using `u64`...",
                                project
                                    .loc_to_file_location_string(module.clone(), &type_name.loc()),
                            );
                        }
                        "u64".into()
                    }
                    65..=256 => {
                        if *bits != 256 {
                            eprintln!(
                                "{}: WARNING: unsupported unsigned integer type `uint{bits}`, using `u256`...",
                                project
                                    .loc_to_file_location_string(module.clone(), &type_name.loc()),
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

                sway::TypeName::Identifier {
                    name: "StorageMap".into(),
                    generic_parameters: Some(sway::GenericParameterList {
                        entries: vec![
                            sway::GenericParameter {
                                type_name: {
                                    let mut type_name = translate_type_name(
                                        project,
                                        module.clone(),
                                        scope.clone(),
                                        key.as_ref(),
                                        storage_location,
                                    );
                                    if let Some(storage_key_type) = type_name.storage_key_type() {
                                        type_name = storage_key_type;
                                    }
                                    type_name
                                },
                                implements: None,
                            },
                            sway::GenericParameter {
                                type_name: {
                                    let mut type_name = translate_type_name(
                                        project,
                                        module,
                                        scope.clone(),
                                        value.as_ref(),
                                        storage_location,
                                    );
                                    if let Some(storage_key_type) = type_name.storage_key_type() {
                                        type_name = storage_key_type;
                                    }
                                    type_name
                                },
                                implements: None,
                            },
                        ],
                    }),
                }
            }

            solidity::Type::Function {
                params, returns, ..
            } => sway::TypeName::Function {
                old_name: String::new(),
                new_name: String::new(),
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
                                        scope.clone(),
                                        &p.ty,
                                        p.storage.as_ref(),
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
                                Some(p) => translate_type_name(
                                    project,
                                    module.clone(),
                                    scope.clone(),
                                    &p.ty,
                                    p.storage.as_ref(),
                                ),
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
            if module.borrow().type_definitions.iter().any(|t| {
                let sway::TypeName::Identifier {
                    name: type_name,
                    generic_parameters: None,
                } = &t.signature
                else {
                    return false;
                };
                type_name == name
            }) {
                sway::TypeName::Identifier {
                    name: name.clone(),
                    generic_parameters: None,
                }
            }
            // Check if type is a struct
            else if module.borrow().structs.iter().any(|n| {
                let sway::TypeName::Identifier {
                    name: struct_name,
                    generic_parameters: None,
                } = &n.signature
                else {
                    return false;
                };

                struct_name == name
            }) {
                sway::TypeName::Identifier {
                    name: name.clone(),
                    generic_parameters: None,
                }
            }
            // Check if type is an enum
            else if module.borrow().enums.iter().any(|t| {
                let sway::TypeName::Identifier {
                    name: type_name,
                    generic_parameters: None,
                } = &t.signature
                else {
                    return false;
                };

                type_name == name
            }) {
                sway::TypeName::Identifier {
                    name: name.clone(),
                    generic_parameters: None,
                }
            }
            // Check if type is an ABI
            else if project.is_contract_declared(module.clone(), name) {
                sway::TypeName::Identifier {
                    name: name.clone(),
                    generic_parameters: None,
                }
            } else {
                todo!(
                    "{} - translate variable type expression: {} - {type_name:#?}",
                    project.loc_to_file_location_string(module.clone(), &type_name.loc()),
                    type_name.to_string(),
                )
            }
        }

        solidity::Expression::ArraySubscript(_, type_name, length) => match length.as_ref() {
            Some(length) => sway::TypeName::Array {
                type_name: Box::new({
                    let mut type_name = translate_type_name(
                        project,
                        module.clone(),
                        scope.clone(),
                        type_name,
                        storage_location,
                    );
                    if let Some(storage_key_type) = type_name.storage_key_type() {
                        type_name = storage_key_type;
                    }
                    type_name
                }),
                length: {
                    // Create an empty scope to translate the array length expression
                    let scope = Rc::new(RefCell::new(ir::Scope::new(
                        scope
                            .borrow()
                            .get_contract_name()
                            .as_ref()
                            .map(|s| s.as_str()),
                        Some(scope.clone()),
                    )));

                    match translate_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        length.as_ref(),
                    ) {
                        Ok(sway::Expression::Literal(
                            sway::Literal::DecInt(length, _) | sway::Literal::HexInt(length, _),
                        )) => length.try_into().unwrap(),

                        Ok(sway::Expression::PathExpr(path_expr)) => {
                            // Check to see if the expression is a constant
                            if let Some(ident) = path_expr.as_identifier() {
                                if let Some(constant) =
                                    module.borrow().constants.iter().find(|c| c.name == ident)
                                {
                                    if let Some(sway::Expression::Literal(
                                        sway::Literal::DecInt(value, _)
                                        | sway::Literal::HexInt(value, _),
                                    )) = constant.value.as_ref()
                                    {
                                        println!(
                                            "WARNING: Constants as array lengths are unsupported. Using `{}` instead of `{}`.",
                                            value, ident
                                        );
                                        value.clone().try_into().unwrap()
                                    } else {
                                        panic!("Invalid array length expression: {length:#?}")
                                    }
                                } else {
                                    panic!("Invalid array length expression: {length:#?}")
                                }
                            } else {
                                panic!("Invalid array length expression: {length:#?}")
                            }
                        }

                        Ok(_) => panic!("Invalid array length expression: {length:#?}"),
                        Err(e) => panic!("Failed to translate array length expression: {e}"),
                    }
                },
            },

            None => match storage_location {
                Some(storage_location) => match storage_location {
                    solidity::StorageLocation::Memory(_) => {
                        // Ensure that `std::vec::*` is imported
                        module.borrow_mut().ensure_use_declared("std::vec::*");

                        sway::TypeName::Identifier {
                            name: "Vec".into(),
                            generic_parameters: Some(sway::GenericParameterList {
                                entries: vec![sway::GenericParameter {
                                    type_name: translate_type_name(
                                        project,
                                        module,
                                        scope.clone(),
                                        type_name,
                                        None,
                                    ),
                                    implements: None,
                                }],
                            }),
                        }
                    }

                    solidity::StorageLocation::Storage(_) => {
                        // Ensure that `std::storage::storage_vec::*` is imported
                        module
                            .borrow_mut()
                            .ensure_use_declared("std::storage::storage_vec::*");

                        sway::TypeName::Identifier {
                            name: "StorageVec".into(),
                            generic_parameters: Some(sway::GenericParameterList {
                                entries: vec![sway::GenericParameter {
                                    type_name: {
                                        let mut type_name = translate_type_name(
                                            project,
                                            module,
                                            scope.clone(),
                                            type_name,
                                            Some(storage_location),
                                        );
                                        if let Some(storage_key_type) = type_name.storage_key_type()
                                        {
                                            type_name = storage_key_type;
                                        }
                                        type_name
                                    },
                                    implements: None,
                                }],
                            }),
                        }
                    }

                    solidity::StorageLocation::Calldata(_) => {
                        // Ensure that `std::vec::*` is imported
                        module.borrow_mut().ensure_use_declared("std::vec::*");

                        sway::TypeName::Identifier {
                            name: "Vec".into(),
                            generic_parameters: Some(sway::GenericParameterList {
                                entries: vec![sway::GenericParameter {
                                    type_name: translate_type_name(
                                        project,
                                        module,
                                        scope.clone(),
                                        type_name,
                                        None,
                                    ),
                                    implements: None,
                                }],
                            }),
                        }
                    }
                },

                None => {
                    // Ensure that `std::vec::*` is imported
                    module.borrow_mut().ensure_use_declared("std::vec::*");

                    sway::TypeName::Identifier {
                        name: "Vec".into(),
                        generic_parameters: Some(sway::GenericParameterList {
                            entries: vec![sway::GenericParameter {
                                type_name: translate_type_name(
                                    project,
                                    module,
                                    scope.clone(),
                                    type_name,
                                    None,
                                ),
                                implements: None,
                            }],
                        }),
                    }
                }
            },
        },

        solidity::Expression::MemberAccess(_, container, member) => match container.as_ref() {
            solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
                if let Some((external_module, external_contract)) =
                    project.find_module_with_contract(module.clone(), name.as_str())
                {
                    // Check if type refers to a type definition
                    if let Some(SymbolData::TypeDefinition(type_definition)) = resolve_symbol(
                        project,
                        external_module.clone(),
                        scope.clone(),
                        Symbol::TypeDefinition(member.name.clone()),
                    ) {
                        type_definition.name.clone()
                    }
                    // Check if type refers to an enum definition
                    else if let Some(SymbolData::Enum(enum_definition)) = resolve_symbol(
                        project,
                        external_module.clone(),
                        scope.clone(),
                        Symbol::Enum(member.name.clone()),
                    ) {
                        enum_definition.type_definition.name.clone()
                    }
                    // Check if type refers to a struct definition
                    else if let Some(SymbolData::Struct(struct_definition)) = resolve_symbol(
                        project,
                        external_module.clone(),
                        scope.clone(),
                        Symbol::Struct(member.name.clone()),
                    ) {
                        sway::TypeName::Identifier {
                            name: struct_definition.borrow().name.clone(),
                            generic_parameters: struct_definition
                                .borrow()
                                .generic_parameters
                                .clone(),
                        }
                    } else {
                        todo!(
                            "found contract `{}` in module `{}`",
                            external_contract.borrow().name,
                            external_module.borrow().path.to_string_lossy()
                        )
                    }
                } else {
                    todo!(
                        "{} - member access type name expression: {type_name}",
                        project.loc_to_file_location_string(module.clone(), &type_name.loc()),
                    )
                }
            }

            _ => todo!(
                "{} - member access type name expression: {type_name}",
                project.loc_to_file_location_string(module.clone(), &type_name.loc()),
            ),
        },

        _ => unimplemented!(
            "{} - type name expression: {type_name}",
            project.loc_to_file_location_string(module.clone(), &type_name.loc()),
        ),
    };

    if let Some(storage_location) = storage_location {
        match storage_location {
            solidity::StorageLocation::Memory(_) => {
                // TODO: determine if any memory pointers need to be wrapped in a special container
            }

            solidity::StorageLocation::Storage(_) => {
                // Wrap storage pointer types in a `StorageKey<T>`
                type_name = sway::TypeName::Identifier {
                    name: "StorageKey".into(),
                    generic_parameters: Some(sway::GenericParameterList {
                        entries: vec![sway::GenericParameter {
                            type_name,
                            implements: None,
                        }],
                    }),
                };
            }

            solidity::StorageLocation::Calldata(_) => {
                // TODO: determine if any calldata pointers need to be wrapped in a special container
            }
        }
    }

    type_name
}

use super::{translate_expression, TranslatedDefinition, TranslationScope};
use crate::{project::Project, sway};
use solang_parser::{helpers::CodeLocation, pt as solidity};
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_return_type_name(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    type_name: sway::TypeName,
) -> sway::TypeName {
    match type_name {
        sway::TypeName::StringSlice => {
            // Ensure `std::string::*` is imported
            translated_definition.ensure_use_declared("std::string::*");
    
            sway::TypeName::Identifier {
                name: "String".into(),
                generic_parameters: None,
            }
        },
        
        _ => {
            // Check if the parameter's type is an ABI and make it an Identity
            if let sway::TypeName::Identifier { name, generic_parameters: None } = &type_name {
                if project.find_definition_with_abi(name.as_str()).is_some() {
                    return sway::TypeName::Identifier {
                        name: "Identity".into(),
                        generic_parameters: None,
                    };
                }
            }

            type_name.clone()
        }
    }
}

pub fn translate_type_name(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
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

            solidity::Type::String => if is_storage {
                // Ensure `std::storage::storage_string::*` is imported
                translated_definition.ensure_use_declared("std::storage::storage_string::*");

                sway::TypeName::Identifier {
                    name: "StorageString".into(),
                    generic_parameters: None,
                }
            } else {
                sway::TypeName::StringSlice
            },

            solidity::Type::Int(bits) => {
                translated_definition.ensure_dependency_declared(
                    "sway_libs = { git = \"https://github.com/FuelLabs/sway-libs\", tag = \"v0.24.0\" }"
                );

                sway::TypeName::Identifier {
                    name: match *bits {
                        0..=8 => {
                            if *bits != 8 {
                                eprintln!("WARNING: unsupported signed integer type `int{bits}`, using `I8`...");
                            }
                            translated_definition.ensure_use_declared("sway_libs::signed_integers::i8::*");
                            "I8".into()
                        }
                        9..=16 => {
                            if *bits != 16 {
                                eprintln!("WARNING: unsupported signed integer type `int{bits}`, using `I16`...");
                            }
                            translated_definition.ensure_use_declared("sway_libs::signed_integers::i16::*");
                            "I16".into()
                        }
                        17..=32 => {
                            if *bits != 32 {
                                eprintln!("WARNING: unsupported signed integer type `int{bits}`, using `I32`...");
                            }
                            translated_definition.ensure_use_declared("sway_libs::signed_integers::i32::*");
                            "I32".into()
                        }
                        33..=64 => {
                            if *bits != 64 {
                                eprintln!("WARNING: unsupported signed integer type `int{bits}`, using `I64`...");
                            }
                            translated_definition.ensure_use_declared("sway_libs::signed_integers::i64::*");
                            "I64".into()
                        }
                        65..=128 => {
                            if *bits != 128 {
                                eprintln!("WARNING: unsupported signed integer type `int{bits}`, using `I128`...");
                            }
                            translated_definition.ensure_use_declared("sway_libs::signed_integers::i128::*");
                            "I128".into()
                        }
                        129..=256 => {
                            if *bits != 256 {
                                eprintln!("WARNING: unsupported signed integer type `int{bits}`, using `I256`...");
                            }
                            translated_definition.ensure_use_declared("sway_libs::signed_integers::i256::*");
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
                            eprintln!("WARNING: unsupported unsigned integer type `uint{bits}`, using `u8`...");
                        }
                        "u8".into()
                    }
                    9..=16 => {
                        if *bits != 16 {
                            eprintln!("WARNING: unsupported unsigned integer type `uint{bits}`, using `u16`...");
                        }
                        "u16".into()
                    }
                    17..=32 => {
                        if *bits != 32 {
                            eprintln!("WARNING: unsupported unsigned integer type `uint{bits}`, using `u32`...");
                        }
                        "u32".into()
                    }
                    33..=64 => {
                        if *bits != 64 {
                            eprintln!("WARNING: unsupported unsigned integer type `uint{bits}`, using `u64`...");
                        }
                        "u64".into()
                    }
                    65..=256 => {
                        if *bits != 256 {
                            eprintln!("WARNING: unsupported unsigned integer type `uint{bits}`, using `u256`...");
                        }
                        "u256".into()
                    }
                    _ => panic!("Invalid uint type: {bits}"),
                },
                generic_parameters: None,
            },

            solidity::Type::Bytes(length) => match *length {
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
                }
            },

            solidity::Type::Rational => todo!("rational types"),

            solidity::Type::DynamicBytes => sway::TypeName::Identifier {
                name: {
                    // Ensure `std::bytes::Bytes` is imported
                    translated_definition.ensure_use_declared("std::bytes::Bytes");

                    "Bytes".into() // TODO: is this ok?
                },
                generic_parameters: None,
            },

            solidity::Type::Mapping { key, value, .. } => {
                // Ensure `std::hash::Hash` is imported
                translated_definition.ensure_use_declared("std::hash::Hash");
        
                if is_parameter {
                    sway::TypeName::Identifier {
                        name: "StorageKey".into(),
                        generic_parameters: Some(sway::GenericParameterList {
                            entries: vec![
                                sway::GenericParameter {
                                    type_name: sway::TypeName::Identifier {
                                        name: "StorageMap".into(),
                                        generic_parameters: Some(sway::GenericParameterList {
                                            entries: vec![
                                                sway::GenericParameter {
                                                    type_name: translate_type_name(project, translated_definition, key.as_ref(), is_storage, is_parameter),
                                                    implements: None,
                                                },
                                                sway::GenericParameter {
                                                    type_name: translate_type_name(project, translated_definition, value.as_ref(), is_storage, is_parameter),
                                                    implements: None,
                                                },
                                            ],
                                        }),
                                    },
                                    implements: None,
                                },
                            ],
                        }),
                    }
                } else {
                    sway::TypeName::Identifier {
                        name: "StorageMap".into(),
                        generic_parameters: Some(sway::GenericParameterList {
                            entries: vec![
                                sway::GenericParameter {
                                    type_name: translate_type_name(project, translated_definition, key.as_ref(), is_storage, is_parameter),
                                    implements: None,
                                },
                                sway::GenericParameter {
                                    type_name: translate_type_name(project, translated_definition, value.as_ref(), is_storage, is_parameter),
                                    implements: None,
                                },
                            ],
                        }),
                    }
                }
            }

            solidity::Type::Function { .. } => {
                eprintln!(
                    "WARNING: function pointer types are not supported, using `raw_ptr` for `{type_name}` @ {}",
                    match project.loc_to_line_and_column(&translated_definition.path, &type_name.loc()) {
                        Some((line, col)) => format!("{}:{}:{}", translated_definition.path.to_string_lossy(), line, col),
                        None => format!("{} - ", translated_definition.path.to_string_lossy()),
                    },
                );
                
                sway::TypeName::Identifier {
                    name: "raw_ptr".into(),
                    generic_parameters: None,
                }
            }
        }

        solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
            // Check if type is a type definition
            if translated_definition.type_definitions.iter().any(|t| matches!(&t.name, sway::TypeName::Identifier { name: type_name, generic_parameters: None } if type_name == name)) {
                return sway::TypeName::Identifier {
                    name: name.clone(),
                    generic_parameters: None,
                };
            }
            
            // Check if type is a struct
            if translated_definition.struct_names.iter().any(|n| n == name) {
                return sway::TypeName::Identifier {
                    name: name.clone(),
                    generic_parameters: None,
                };
            }
            
            // Check if type is an enum
            if translated_definition.enums.iter().any(|t| match &t.type_definition.name {
                sway::TypeName::Identifier { name: type_name, generic_parameters: None } => type_name == name,
                _ => false,
            }) {
                return sway::TypeName::Identifier {
                    name: name.clone(),
                    generic_parameters: None,
                };
            }
            
            // Check if type is a contract that hasn't been defined yet
            if project.find_definition_with_abi(name.as_str()).is_none() && translated_definition.contract_names.iter().any(|n| n == name) {
                project.translate(Some(name), &translated_definition.path).unwrap();
            }

            // Check if type is an ABI
            if let Some(external_definition) = project.find_definition_with_abi(name.as_str()) {
                // Ensure the ABI is added to the current definition
                if !translated_definition.abis.iter().any(|a| a.name == *name) {
                    translated_definition.abis.push(external_definition.abi.as_ref().unwrap().clone());
                }

                return sway::TypeName::Identifier {
                    name: external_definition.name.clone(),
                    generic_parameters: None,
                };
            }

            todo!("translate variable type expression: {} - {type_name:#?}", type_name.to_string())
        }

        solidity::Expression::ArraySubscript(_, type_name, length) => match length.as_ref() {
            Some(length) => sway::TypeName::Array {
                type_name: Box::new(translate_type_name(project, translated_definition, type_name, is_storage, is_parameter)),
                length: {
                    // Create an empty scope to translate the array length expression
                    let scope = Rc::new(RefCell::new(TranslationScope {
                        parent: Some(translated_definition.toplevel_scope.clone()),
                        ..Default::default()
                    }));

                    match translate_expression(project, translated_definition, scope.clone(), length.as_ref()) {
                        Ok(sway::Expression::Literal(sway::Literal::DecInt(length, _) | sway::Literal::HexInt(length, _))) => length.try_into().unwrap(),
                        Ok(_) => panic!("Invalid array length expression: {length:#?}"),
                        Err(e) => panic!("Failed to translate array length expression: {e}"),
                    }
                },
            },

            None => sway::TypeName::Identifier {
                name: if is_storage {
                    // Ensure that `std::storage::storage_vec::*` is imported
                    translated_definition.ensure_use_declared("std::storage::storage_vec::*");

                    "StorageVec".into()
                } else {
                    "Vec".into()
                },
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![
                        sway::GenericParameter {
                            type_name: translate_type_name(project, translated_definition, type_name, is_storage, is_parameter),
                            implements: None,
                        },
                    ],
                }),
            },
        }

        solidity::Expression::MemberAccess(_, container, member) => match container.as_ref() {
            solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
                let mut result = None;
                let mut translated_enum = None;
                let mut translated_struct = None;

                if name == &translated_definition.name {
                    // Check to see if member is an enum
                    if let Some(external_enum) = translated_definition.enums.iter().find(|e| {
                        let sway::TypeName::Identifier { name, generic_parameters: None } = &e.type_definition.name else {
                            panic!("Expected Identifier type name, found {:#?}", e.type_definition.name);
                        };

                        *name == member.name
                    }) {
                        // Import the enum if we haven't already
                        if !translated_definition.enums.contains(external_enum) {
                            translated_enum = Some(external_enum.clone());
                        }

                        result = Some(external_enum.type_definition.name.clone());
                    }
                    // Check to see if member is a struct
                    else if let Some(external_struct) = translated_definition.structs.iter().find(|s| s.name == member.name) {
                        // Import the struct if we haven't already
                        if !translated_definition.structs.contains(external_struct) {
                            translated_struct = Some(external_struct.clone());
                        }

                        result = Some(sway::TypeName::Identifier {
                            name: external_struct.name.clone(),
                            generic_parameters: None,
                        });
                    }
                }

                // Check to see if container is an external definition
                else if let Some(external_definition) = project.translated_definitions.iter().find(|d| d.name == *name) {
                    // Check to see if member is an enum
                    if let Some(external_enum) = external_definition.enums.iter().find(|e| {
                        let sway::TypeName::Identifier { name, generic_parameters: None } = &e.type_definition.name else {
                            panic!("Expected Identifier type name, found {:#?}", e.type_definition.name);
                        };

                        *name == member.name
                    }) {
                        // Import the enum if we haven't already
                        if !translated_definition.enums.contains(external_enum) {
                            translated_enum = Some(external_enum.clone());
                        }

                        result = Some(external_enum.type_definition.name.clone());
                    }
                    // Check to see if member is a struct
                    else if let Some(external_struct) = external_definition.structs.iter().find(|s| s.name == member.name) {
                        // Import the struct if we haven't already
                        if !translated_definition.structs.contains(external_struct) {
                            translated_struct = Some(external_struct.clone());
                        }

                        result = Some(sway::TypeName::Identifier {
                            name: external_struct.name.clone(),
                            generic_parameters: None,
                        });
                    }
                }

                if let Some(type_name) = result {
                    if let Some(translated_enum) = translated_enum {
                        translated_definition.add_enum(&translated_enum);
                    } else if let Some(translated_struct) = translated_struct {
                        translated_definition.ensure_struct_included(project, &translated_struct);
                    }

                    return type_name;
                }

                todo!(
                    "{} - member access type name expression: {type_name}",
                    match project.loc_to_line_and_column(&translated_definition.path, &type_name.loc()) {
                        Some((line, col)) => format!("{}:{}:{}", translated_definition.path.to_string_lossy(), line, col),
                        None => format!("{}", translated_definition.path.to_string_lossy()),
                    },
                )
            }

            _ => todo!(
                "{} - member access type name expression: {type_name}",
                match project.loc_to_line_and_column(&translated_definition.path, &type_name.loc()) {
                    Some((line, col)) => format!("{}:{}:{}", translated_definition.path.to_string_lossy(), line, col),
                    None => format!("{}", translated_definition.path.to_string_lossy()),
                },
            )
        }

        _ => unimplemented!(
            "{} - type name expression: {type_name}",
            match project.loc_to_line_and_column(&translated_definition.path, &type_name.loc()) {
                Some((line, col)) => format!("{}:{}:{}", translated_definition.path.to_string_lossy(), line, col),
                None => format!("{}", translated_definition.path.to_string_lossy()),
            },
        ),
    }
}

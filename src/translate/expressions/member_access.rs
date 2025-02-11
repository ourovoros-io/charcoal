use std::{cell::RefCell, rc::Rc};
use convert_case::Case;
use num_bigint::BigUint;
use num_traits::Zero;
use solang_parser::{helpers::CodeLocation, pt as solidity};
use crate::{errors::Error, project::Project, sway, translate::{translate_type_name, TranslatedDefinition, TranslationScope}};
use super::translate_expression;

#[inline]
pub fn translate_member_access_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
    container: &solidity::Expression,
    member: &solidity::Identifier,
) -> Result<sway::Expression, Error> {
    match container {
        solidity::Expression::FunctionCall(_, x, args) => match x.as_ref() {
            solidity::Expression::Variable(solidity::Identifier { name, .. }) => match name.as_str() {
                "type" => {
                    if args.len() != 1 {
                        panic!("Invalid type name expression, expected 1 parameter, found {}: {}", args.len(), expression);
                    }

                    let type_name = translate_type_name(project, translated_definition, &args[0], false, false);
                    let type_name = translated_definition.get_underlying_type(&type_name);

                    match &type_name {
                        sway::TypeName::Identifier { name, .. } => match (name.as_str(), member.name.as_str()) {
                            ("I8" | "I16" | "I32" | "I64" | "I128" | "I256" | "u8" | "u16" | "u32" | "u64" | "u256", "min") => return Ok(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier(format!("{name}::min")),
                                generic_parameters: None,
                                parameters: vec![],
                            })),

                            ("I8" | "I16" | "I32" | "I64" | "I128" | "I256" | "u8" | "u16" | "u32" | "u64" | "u256", "max") => return Ok(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier(format!("{name}::max")),
                                generic_parameters: None,
                                parameters: vec![],
                            })),

                            (_, member_name) => match member_name {
                                "interfaceId" => {
                                    // TODO: type(X).interfaceId => ???
                                    return Ok(sway::Expression::create_todo(Some(expression.to_string())));
                                }

                                _ => {}
                            }
                        }

                        _ => {}
                    }
                }

                _ => {}
            }

            _ => {}
        }

        solidity::Expression::Variable(solidity::Identifier { name, .. }) => match (name.as_str(), member.name.as_str()) {
            ("block", "basefee") => {
                // block.basefee => /*unsupported: block.basefee; using:*/ 0
                return Ok(sway::Expression::Commented(
                    "unsupported: block.basefee; using:".into(),
                    Box::new(sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None))),
                ))
            }

            ("block", "blobbasefee") => {
                // block.blobbasefee => /*unsupported: block.blobbasefee; using:*/ 0
                return Ok(sway::Expression::Commented(
                    "unsupported: block.blobbasefee; using:".into(),
                    Box::new(sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None))),
                ))
            }

            ("block", "chainid") => {
                // block.chainid => asm(r1) {
                //    gm r1 i4;
                //    r1: u64
                // }.as_u256()

                return Ok(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression: sway::Expression::from(sway::AsmBlock {
                            registers: vec![
                                sway::AsmRegister {
                                    name: "r1".into(),
                                    value: None,
                                },
                            ],
                            instructions: vec![
                                sway::AsmInstruction {
                                    op_code: "gm".into(),
                                    args: vec![
                                        "r1".into(),
                                        "i4".into(),
                                    ],
                                }
                            ],
                            final_expression: Some(sway::AsmFinalExpression {
                                register: "r1".into(),
                                type_name: Some(sway::TypeName::Identifier {
                                    name: "u64".into(),
                                    generic_parameters: None,
                                }),
                            }),
                        }),
                        member: "as_u256".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![],
                }));
            }

            ("block", "coinbase") => {
                // block.coinbase => {
                //     let ptr = std::alloc::alloc(__size_of::<b256>());
                //     asm(r1: ptr) {
                //         cb r1;
                //     }
                //     Identity::from(ContractId::from(ptr.read::<b256>()))
                // }

                return Ok(sway::Expression::from(sway::Block {
                    statements: vec![
                        // let ptr = std::alloc::alloc(__size_of::<b256>());
                        sway::Statement::from(sway::Let {
                            pattern: sway::LetPattern::from(sway::LetIdentifier {
                                is_mutable: false,
                                name: "ptr".into(),
                            }),
                            type_name: None,
                            value: sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier("std::alloc::alloc".into()),
                                generic_parameters: None,
                                parameters: vec![
                                    sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::Identifier("__size_of".into()),
                                        generic_parameters: Some(sway::GenericParameterList {
                                            entries: vec![
                                                sway::GenericParameter {
                                                    type_name: sway::TypeName::Identifier {
                                                        name: "b256".into(),
                                                        generic_parameters: None,
                                                    },
                                                    implements: None,
                                                },
                                            ],
                                        }),
                                        parameters: vec![],
                                    }),
                                ],
                            }),
                        }),
                        
                        // asm(r1: ptr) {
                        //     cb r1;
                        // }
                        sway::Statement::from(sway::Expression::from(sway::AsmBlock {
                            registers: vec![
                                sway::AsmRegister {
                                    name: "r1".into(),
                                    value: Some(sway::Expression::Identifier("ptr".into())),
                                },
                            ],
                            instructions: vec![
                                sway::AsmInstruction {
                                    op_code: "cb".into(),
                                    args: vec![
                                        "r1".into(),
                                    ],
                                },
                            ],
                            final_expression: None,
                        })),
                    ],
                    
                    // Identity::from(ContractId::from(ptr.read::<b256>()))
                    final_expr: Some(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("Identity::from".into()),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier("ContractId::from".into()),
                                generic_parameters: None,
                                parameters: vec![
                                    sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::from(sway::MemberAccess {
                                            expression: sway::Expression::Identifier("ptr".into()),
                                            member: "read".into(),
                                        }),
                                        generic_parameters: Some(sway::GenericParameterList {
                                            entries: vec![
                                                sway::GenericParameter {
                                                    type_name: sway::TypeName::Identifier {
                                                        name: "b256".into(),
                                                        generic_parameters: None,
                                                    },
                                                    implements: None,
                                                },
                                            ]
                                        }),
                                        parameters: vec![],
                                    }),
                                ],
                            })
                        ],
                    })),
                }));
            }

            ("block", "difficulty") => {
                // block.difficulty => /*unsupported: block.difficulty; using:*/ 0
                return Ok(sway::Expression::Commented(
                    "unsupported: block.difficulty; using:".into(),
                    Box::new(sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None))),
                ))
            }

            // TODO: find out the appropriate sway version of `block.gaslimit`
            ("block", "gaslimit") => {
                // block.gaslimit => ???
                return Ok(sway::Expression::create_todo(Some("block.gaslimit".into())))
            }

            ("block", "number") => {
                // block.number => std::block::height()
                return Ok(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::Identifier("std::block::height".into()),
                    generic_parameters: None,
                    parameters: vec![],
                }))
            }

            ("block", "prevrandao") => {
                // block.prevrandao => /*unsupported: block.prevrandao; using:*/ 0
                return Ok(sway::Expression::Commented(
                    "unsupported: block.prevrandao; using:".into(),
                    Box::new(sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None))),
                ))
            }

            ("block", "timestamp") => {
                // block.timestamp => std::block::timestamp().as_u256()
                return Ok(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression: sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier("std::block::timestamp".into()),
                            generic_parameters: None,
                            parameters: vec![],
                        }),
                        member: "as_u256".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![],
                }))
            }

            ("msg", "data") => {
                // msg.data => std::inputs::input_message_data(0, 0).unwrap_or(Bytes::new())

                // Ensure `std::bytes::Bytes` is imported
                translated_definition.ensure_use_declared("std::bytes::Bytes");

                return Ok(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression: sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier("std::inputs::input_message_data".into()),
                            generic_parameters: None,
                            parameters: vec![
                                sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                                sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                            ],
                        }),
                        member: "unwrap_or".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![
                        sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier("Bytes::new".into()),
                            generic_parameters: None,
                            parameters: vec![],
                        }),
                    ],
                }))
            }

            ("msg", "sender") => {
                // msg.sender => msg_sender().unwrap()
                return Ok(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression: sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier("msg_sender".into()),
                            generic_parameters: None,
                            parameters: vec![],
                        }),
                        member: "unwrap".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![],
                }))
            }

            ("msg", "sig") => {
                // msg.sig => /*unsupported: msg.sig; using:*/ [0, 0, 0, 0]
                return Ok(sway::Expression::Commented(
                    "unsupported: msg.sig; using:".into(),
                    Box::new(sway::Expression::from(sway::Array {
                        elements: vec![
                            sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                            sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                            sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                            sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                        ],
                    })),
                ))
            }

            ("msg", "value") => {
                // msg.value => std::context::msg_amount()
                return Ok(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::Identifier("std::context::msg_amount".into()),
                    generic_parameters: None,
                    parameters: vec![],
                }))
            }

            ("tx", "gasprice") => {
                // tx.gasprice => std::tx::tx_gas_price().unwrap_or(0)
                return Ok(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression: sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier("std::tx::tx_gas_price".to_string()),
                            generic_parameters: None,
                            parameters: vec![],
                        }),
                        member: "unwrap_or".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![
                        sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                    ],
                }))
            }
            
            ("tx", "origin") => {
                // tx.origin => Identity::from(Address::from(/*unsupported: tx.origin; using:*/ ZERO_B256))

                // Ensure `std::constants::ZERO_B256` is imported
                translated_definition.ensure_use_declared("std::constants::ZERO_B256");

                return Ok(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::Identifier("Identity::Address".into()),
                    generic_parameters: None,
                    parameters: vec![
                        sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier("Address::from".into()),
                            generic_parameters: None,
                            parameters: vec![
                                sway::Expression::Commented(
                                    "unsupported: tx.origin; using:".into(),
                                    Box::new(sway::Expression::Identifier("ZERO_B256".into())),
                                ),
                            ],
                        }),
                    ],
                }))
            }

            (name, member) => {
                // Check to see if the variable is in scope
                if let Some(variable) = scope.borrow().get_variable_from_old_name(name) {
                    let variable = variable.borrow();

                    match &variable.type_name {
                        sway::TypeName::Identifier { name: type_name, generic_parameters } => match type_name.as_str() {
                            "StorageVec" | "Vec" if generic_parameters.is_some() => match member {
                                "length" => {
                                    return Ok(sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::from(sway::MemberAccess {
                                            expression: if variable.is_storage {
                                                sway::Expression::from(sway::MemberAccess {
                                                    expression: sway::Expression::Identifier("storage".into()),
                                                    member: variable.new_name.clone(),
                                                })
                                            } else {
                                                sway::Expression::Identifier(variable.new_name.clone())
                                            },
                                            member: "len".into(),
                                        }),
                                        generic_parameters: None,
                                        parameters: vec![],
                                    }));
                                }

                                _ => {}
                            }

                            _ => {}
                        }

                        _ => {}
                    }
                }

                // Check to see if the variable is an enum
                if let Some(translated_enum) = translated_definition.enums.iter().find(|e| match &e.type_definition.name {
                    sway::TypeName::Identifier { name: enum_name, .. } => enum_name == name,
                    _ => false
                }) {
                    let new_name = crate::translate_naming_convention(member, Case::ScreamingSnake);

                    // Check to see if member is part of translated enum
                    if let Some(sway::ImplItem::Constant(c)) = translated_enum.variants_impl.items.iter().find(|i| match i {
                        sway::ImplItem::Constant(c) => c.name == new_name,
                        _ => false,
                    }) {
                        return Ok(sway::Expression::Identifier(format!("{}::{}", name, c.name)));
                    }
                }

                // Check to see if the variable is an external definition
                if let Some(external_definition) = project.translated_definitions.iter().find(|d| d.name == name) {
                    let Some(variable) = external_definition.toplevel_scope.borrow().get_variable_from_old_name(member) else {
                        panic!(
                            "{}error: Variable not found in scope: \"{member}\"",
                            match project.loc_to_line_and_column(&translated_definition.path, &container.loc()) {
                                Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                                None => format!("{} - ", translated_definition.path.to_string_lossy()),
                            }
                        );
                    };

                    let variable = variable.borrow();
                
                    // If the variable is a constant, ensure it is added to the current definition
                    if variable.is_constant {
                        let constant = external_definition.constants.iter().find(|c| c.name == variable.new_name).unwrap();
                        
                        if !translated_definition.constants.contains(constant) {
                            translated_definition.constants.push(constant.clone());
                        }

                        if !translated_definition.toplevel_scope.borrow().variables.iter().any(|v| v.borrow().new_name == variable.new_name) {
                            translated_definition.toplevel_scope.borrow_mut().variables.push(Rc::new(RefCell::new(variable.clone())));
                        }

                        return Ok(sway::Expression::Identifier(variable.new_name.clone()));
                    }
                }
            }
        }

        solidity::Expression::MemberAccess(_, container1, member1) => match container1.as_ref() {
            solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
                // Check to see if expression is an explicit contract function selector
                if member.name == "selector" {
                    if name == "this" {
                        if translated_definition.toplevel_scope.borrow().find_function(|f| f.borrow().old_name == member1.name).is_some() {
                            return Ok(sway::Expression::create_todo(Some(expression.to_string())));
                        }
                    }
                    
                    if let Some(external_definition) = project.find_definition_with_abi(name) {
                        if external_definition.toplevel_scope.borrow().find_function(|f| f.borrow().old_name == member1.name).is_some() {
                            return Ok(sway::Expression::create_todo(Some(expression.to_string())));
                        }
                    }

                    todo!()
                }

                // Check to see if container is an external definition
                if let Some(external_definition) = project.translated_definitions.iter().find(|d| d.name == *name) {
                    // Check to see if member is an enum
                    if let Some(external_enum) = external_definition.enums.iter().find(|e| {
                        let sway::TypeName::Identifier { name, generic_parameters: None } = &e.type_definition.name else {
                            panic!("Expected Identifier type name, found {:#?}", e.type_definition.name);
                        };
    
                        *name == member1.name
                    }) {
                        let sway::TypeName::Identifier { name: enum_name, generic_parameters: None } = &external_enum.type_definition.name else {
                            panic!("Expected Identifier type name, found {:#?}", external_enum.type_definition.name);
                        };
    
                        let variant_name = crate::translate_naming_convention(member.name.as_str(), Case::ScreamingSnake);
    
                        // Ensure the variant exists
                        if external_enum.variants_impl.items.iter().any(|i| {
                            let sway::ImplItem::Constant(c) = i else { return false };
                            c.name == variant_name
                        }) {
                            translated_definition.add_enum(external_enum);
                            return Ok(sway::Expression::Identifier(format!("{enum_name}::{variant_name}")));
                        }
                    }
                }
            }

            _ => {}
        }

        _ => {}
    }

    let container = translate_expression(project, translated_definition, scope.clone(), container)?;
    
    let mut check_container = |container: &sway::Expression| -> Result<Option<sway::Expression>, Error> {
        let container_type_name = translated_definition.get_expression_type(scope.clone(), &container)?;
        let container_type_name_string = container_type_name.to_string();
    
        // Check if container is a struct
        if let Some(struct_definition) = translated_definition.structs.iter().find(|s| s.name == container_type_name_string) {
            let field_name = crate::translate_naming_convention(member.name.as_str(), Case::Snake);
    
            if struct_definition.fields.iter().any(|f| f.name == field_name) {
                return Ok(Some(sway::Expression::from(sway::MemberAccess {
                    expression: container.clone(),
                    member: field_name,
                })));
            }
        }
    
        // Check for fields of built-in solidity value types
        match container_type_name {
            sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                ("Bytes", None) => match member.name.as_str() {
                    "length" => return Ok(Some(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::from(sway::MemberAccess {
                            expression: container.clone(),
                            member: "len".into(),
                        }),
                        generic_parameters: None,
                        parameters: vec![],
                    }))),
    
                    _ => {}
                }
    
                ("Identity", None) => match member.name.as_str() {
                    "balance" => return Ok(Some(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("std::context::balance_of".into()),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::from(sway::MemberAccess {
                                    expression: sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::from(sway::MemberAccess {
                                            expression: container.clone(),
                                            member: "as_contract_id".into(),
                                        }),
                                        generic_parameters: None,
                                        parameters: vec![],
                                    }),
                                    member: "unwrap".into(),
                                }),
                                generic_parameters: None,
                                parameters: vec![],
                            }),
                            sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier("AssetId::default".into()),
                                generic_parameters: None,
                                parameters: vec![],
                            }),
                        ],
                    }))),
    
                    _ => {}
                }
    
                ("StorageKey", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => match &generic_parameters.entries[0].type_name {
                    sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                        ("StorageVec", Some(_)) => match member.name.as_str() {
                            "length" => {
                                return Ok(Some(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: container.clone(),
                                        member: "len".into(),
                                    }),
                                    generic_parameters: None,
                                    parameters: vec![],
                                })));
                            }
    
                            _ => {}
                        }
    
                        _ => {}
                    }
    
                    _ => {}
                }
    
                ("Vec", Some(_)) => match member.name.as_str() {
                    "length" => return Ok(Some(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::from(sway::MemberAccess {
                            expression: container.clone(),
                            member: "len".into(),
                        }),
                        generic_parameters: None,
                        parameters: vec![],
                    }))),
                    _ => {}
                }
                
                _ => {}
            }
    
            _ => {}
        }
    
        Ok(None)
    };

    if let Some(result) = check_container(&container)? {
        return Ok(result);
    }

    // HACK: try tacking `.read()` onto the end and checking again
    if let Ok(Some(result)) = check_container(&sway::Expression::from(sway::FunctionCall {
        function: sway::Expression::from(sway::MemberAccess {
            expression: container.clone(),
            member: "read".into(),
        }),
        generic_parameters: None,
        parameters: vec![],
    })) {
        return Ok(result);
    }

    let container_type_name = translated_definition.get_expression_type(scope.clone(), &container)?;
    let container_type_name_string = container_type_name.to_string();

    todo!("{}translate {container_type_name_string} member access expression: {expression} - {expression:#?}", match project.loc_to_line_and_column(&translated_definition.path, &expression.loc()) {
        Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
        None => format!("{} - ", translated_definition.path.to_string_lossy()),
    },)
}

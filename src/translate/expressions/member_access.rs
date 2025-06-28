use crate::{error::Error, project::Project, sway, translate::*};
use convert_case::Case;
use num_bigint::BigUint;
use num_traits::Zero;
use solang_parser::{helpers::CodeLocation, pt as solidity};
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_member_access_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &solidity::Expression,
    container: &solidity::Expression,
    member: &solidity::Identifier,
) -> Result<sway::Expression, Error> {
    match container {
        solidity::Expression::FunctionCall(_, function, parameters) => {
            if let Some(result) = translate_builtin_function_call_member_access_expression(
                project,
                module.clone(),
                scope.clone(),
                function,
                parameters,
                member.name.as_str(),
            )? {
                return Ok(result);
            }
        }

        solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
            if let Some(result) = translate_builtin_variable_member_access_expression(
                project,
                module.clone(),
                scope.clone(),
                name.as_str(),
                member.name.as_str(),
            )? {
                return Ok(result);
            }

            // Check to see if the variable is an enum
            if let Some(translated_enum) = project.find_enum(module.clone(), name) {
                let new_name = translate_naming_convention(member.name.as_str(), Case::Constant);

                // Check to see if member is part of translated enum
                if let Some(sway::ImplItem::Constant(c)) = translated_enum
                    .variants_impl
                    .items
                    .iter()
                    .find(|i| match i {
                        sway::ImplItem::Constant(c) => c.name == new_name,
                        _ => false,
                    })
                {
                    return Ok(sway::Expression::create_identifier(format!(
                        "{}::{}",
                        name, c.name
                    )));
                }
            }

            // Check to see if the variable is an external definition
            if let Some(module) = project.find_module_containing_contract(module.clone(), &name) {
                let scope = Rc::new(RefCell::new(ir::Scope::new(
                    Some(name),
                    None,
                    Some(scope.clone()),
                )));

                // Check to see if the variable exists in the external definition
                match resolve_symbol(
                    project,
                    module.clone(),
                    scope.clone(),
                    Symbol::ValueSource(member.to_string()),
                ) {
                    Some(symbol) => match symbol {
                        SymbolData::Variable(variable) => {
                            return Ok(sway::Expression::create_identifier(
                                variable.borrow().new_name.clone(),
                            ));
                        }
                        SymbolData::Constant(constant) => {
                            return Ok(sway::Expression::create_identifier(constant.name.clone()));
                        }
                        SymbolData::ConfigurableField(configurable_field) => {
                            return Ok(sway::Expression::create_identifier(
                                configurable_field.name.clone(),
                            ));
                        }
                        SymbolData::StorageField { namespace, field } => {
                            return Ok(sway::Expression::create_function_calls(
                                None,
                                &[
                                    (
                                        format!(
                                            "storage{}",
                                            namespace
                                                .as_ref()
                                                .map(|n| format!("::{n}"))
                                                .unwrap_or_default()
                                        )
                                        .as_str(),
                                        None,
                                    ),
                                    (field.name.as_str(), None),
                                ],
                            ));
                        }

                        _ => todo!(),
                    },
                    None => {}
                }

                // Check to see if the variable is a function pointer
                if let Some(function) = module.borrow().functions.iter().find(|f| {
                    let sway::TypeName::Function { old_name, .. } = &f.signature else {
                        unreachable!()
                    };

                    *old_name == member.name
                }) {
                    let sway::TypeName::Function { new_name, .. } = &function.signature else {
                        unreachable!()
                    };

                    return Ok(sway::Expression::create_identifier(new_name.clone()));
                }
            }
        }

        solidity::Expression::MemberAccess(_, container1, member1) => match container1.as_ref() {
            solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
                // Check to see if expression is an explicit contract function selector
                if member.name == "selector" {
                    if name == "this" {
                        // TODO
                        // if module.borrow().toplevel_scope.borrow().find_function(|f| f.borrow().old_name == member1.name).is_some() {
                        //     return Ok(sway::Expression::create_todo(Some(expression.to_string())));
                        // }
                    }

                    if let Some(external_contract) = project.find_contract(module.clone(), &name)
                        && external_contract
                            .borrow()
                            .abi
                            .functions
                            .iter()
                            .any(|f| f.old_name == member1.name)
                    {
                        return Ok(sway::Expression::create_todo(Some(expression.to_string())));
                    }

                    todo!()
                }

                //
                // TODO:
                //
                // // Check to see if container is an external definition
                // if let Some(external_definition) = project.translated_definitions.iter().find(|d| d.name == *name) {
                //     // Check to see if member is an enum
                //     if let Some(external_enum) = project.find_enum(module.clone(), &member1.name) {
                //         let sway::TypeName::Identifier { name: enum_name, generic_parameters: None } = &external_enum.type_definition.name else {
                //             panic!("Expected Identifier type name, found {:#?}", external_enum.type_definition.name);
                //         };
                //
                //         let variant_name = translate_naming_convention(member.name.as_str(), Case::Constant);
                //
                //         // Ensure the variant exists
                //         if external_enum.variants_impl.items.iter().any(|i| {
                //             let sway::ImplItem::Constant(c) = i else { return false };
                //             c.name == variant_name
                //         }) {
                //             module.borrow().add_enum(external_enum);
                //             return Ok(sway::Expression::create_identifier(format!("{enum_name}::{variant_name}")));
                //         }
                //     }
                // }
            }

            _ => {}
        },

        _ => {}
    }

    let check_container = |project: &mut Project,
                           container: &sway::Expression|
     -> Result<Option<sway::Expression>, Error> {
        let container_type_name =
            get_expression_type(project, module.clone(), scope.clone(), container)?;

        let container_type_name_string = container_type_name.to_string();
        let field_name = translate_naming_convention(member.name.as_str(), Case::Snake);

        // Check if container is a struct
        if let Some(struct_definition) =
            project.find_struct(module.clone(), scope.clone(), &container_type_name_string)
            && struct_definition
                .borrow()
                .fields
                .iter()
                .any(|f| f.name == field_name)
        {
            return Ok(Some(sway::Expression::from(sway::MemberAccess {
                expression: container.clone(),
                member: field_name,
            })));
        }

        if container_type_name.is_identity() {
            match member.name.as_str() {
                "balance" => {
                    return Ok(Some(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::create_identifier(
                            "std::context::balance_of".into(),
                        ),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::create_function_calls(
                                Some(container.clone()),
                                &[
                                    ("as_contract_id", Some((None, vec![]))),
                                    ("unwrap", Some((None, vec![]))),
                                ],
                            ),
                            sway::Expression::create_function_calls(
                                None,
                                &[("AssetId::default", Some((None, vec![])))],
                            ),
                        ],
                    })));
                }

                _ => {}
            }
        }

        if container_type_name.is_bytes() {
            match member.name.as_str() {
                "length" => {
                    return Ok(Some(sway::Expression::create_function_calls(
                        Some(container.clone()),
                        &[("len", Some((None, vec![])))],
                    )));
                }

                _ => {}
            }
        }

        if let Some(storage_key_type) = container_type_name.storage_key_type() {
            match &storage_key_type {
                sway::TypeName::Identifier {
                    name,
                    generic_parameters,
                } => match (name.as_str(), generic_parameters.as_ref()) {
                    ("StorageVec", Some(_)) => match member.name.as_str() {
                        "length" => {
                            return Ok(Some(sway::Expression::create_function_calls(
                                Some(container.clone()),
                                &[("len", Some((None, vec![])))],
                            )));
                        }

                        _ => {}
                    },

                    _ => {}
                },

                _ => {}
            }
        }

        if container_type_name.is_vec() {
            match member.name.as_str() {
                "length" => {
                    return Ok(Some(sway::Expression::create_function_calls(
                        Some(container.clone()),
                        &[("len", Some((None, vec![])))],
                    )));
                }

                _ => {}
            }
        }

        if container_type_name.is_array() {
            match member.name.as_str() {
                "length" => {
                    return Ok(Some(sway::Expression::create_function_calls(
                        Some(container.clone()),
                        &[("len", Some((None, vec![])))],
                    )));
                }

                _ => {}
            }
        }

        Ok(None)
    };

    let mut container = translate_expression(project, module.clone(), scope.clone(), container)?;

    // HACK: remove read if present
    if let sway::Expression::FunctionCall(f) = &container
        && let sway::Expression::MemberAccess(m) = &f.function
        && m.member == "read"
        && f.parameters.is_empty()
    {
        container = m.expression.clone();
    }

    let container_type_name =
        get_expression_type(project, module.clone(), scope.clone(), &container)?;

    let container_type_name_string = container_type_name.to_string();

    if let Some(result) = check_container(project, &container)? {
        return Ok(result);
    }

    if container_type_name_string == "todo!" {
        let sway::Expression::FunctionCall(f) = &container else {
            unreachable!()
        };
        let Some(ident) = f.function.as_identifier() else {
            unreachable!()
        };
        let "todo!" = ident else { unreachable!() };
        let sway::Expression::Literal(sway::Literal::String(s)) = &f.parameters[0] else {
            unreachable!()
        };
        return Ok(sway::Expression::create_todo(Some(format!("{s}.{member}"))));
    }

    // HACK: try tacking `.read()` onto the end and checking again
    if container_type_name.is_storage_key()
        && let Ok(Some(result)) = check_container(
            project,
            &sway::Expression::create_function_calls(
                Some(container.clone()),
                &[("read", Some((None, vec![])))],
            ),
        )
    {
        return Ok(result);
    }

    todo!(
        "{}: TODO: translate {container_type_name_string} member access expression: {expression} - {expression:#?}",
        project.loc_to_file_location_string(module.clone(), &expression.loc()),
    )
}

#[inline]
fn translate_builtin_function_call_member_access_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    function: &solidity::Expression,
    parameters: &[solidity::Expression],
    member: &str,
) -> Result<Option<sway::Expression>, Error> {
    let solidity::Expression::Variable(solidity::Identifier { name, .. }) = function else {
        return Ok(None);
    };

    match name.as_str() {
        "type" => {
            if parameters.len() != 1 {
                panic!(
                    "Invalid type name expression, expected 1 parameter, found {}: {}({})",
                    parameters.len(),
                    function,
                    parameters
                        .iter()
                        .map(|p| p.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                );
            }

            let type_name =
                translate_type_name(project, module.clone(), scope.clone(), &parameters[0], None);

            let type_name = get_underlying_type(project, module.clone(), &type_name);

            match &type_name {
                sway::TypeName::Identifier { name, .. } => {
                    match (name.as_str(), member) {
                        (
                            "I8" | "I16" | "I32" | "I64" | "I128" | "I256" | "u8" | "u16" | "u32"
                            | "u64" | "u256",
                            "min",
                        ) => {
                            return Ok(Some(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::create_identifier(format!(
                                    "{name}::min"
                                )),
                                generic_parameters: None,
                                parameters: vec![],
                            })));
                        }

                        (
                            "I8" | "I16" | "I32" | "I64" | "I128" | "I256" | "u8" | "u16" | "u32"
                            | "u64" | "u256",
                            "max",
                        ) => {
                            return Ok(Some(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::create_identifier(format!(
                                    "{name}::max"
                                )),
                                generic_parameters: None,
                                parameters: vec![],
                            })));
                        }

                        (_, member_name) => match member_name {
                            "interfaceId" => {
                                // TODO: type(X).interfaceId => ???
                                return Ok(Some(sway::Expression::create_todo(Some(format!(
                                    "{}({})",
                                    function,
                                    parameters
                                        .iter()
                                        .map(|p| p.to_string())
                                        .collect::<Vec<_>>()
                                        .join(", ")
                                )))));
                            }

                            _ => {}
                        },
                    }
                }

                _ => {}
            }
        }

        _ => {}
    }

    Ok(None)
}

#[inline]
fn translate_builtin_variable_member_access_expression(
    _project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    _scope: Rc<RefCell<ir::Scope>>,
    name: &str,
    member: &str,
) -> Result<Option<sway::Expression>, Error> {
    match (name, member) {
        ("block", "basefee") => {
            // block.basefee => /*unsupported: block.basefee; using:*/ 0
            Ok(Some(sway::Expression::Commented(
                "unsupported: block.basefee; using:".into(),
                Box::new(sway::Expression::from(sway::Literal::DecInt(
                    BigUint::zero(),
                    None,
                ))),
            )))
        }

        ("block", "blobbasefee") => {
            // block.blobbasefee => /*unsupported: block.blobbasefee; using:*/ 0
            Ok(Some(sway::Expression::Commented(
                "unsupported: block.blobbasefee; using:".into(),
                Box::new(sway::Expression::from(sway::Literal::DecInt(
                    BigUint::zero(),
                    None,
                ))),
            )))
        }

        ("block", "chainid") => {
            // block.chainid => asm(r1) {
            //    gm r1 i4;
            //    r1: u64
            // }.as_u256()

            Ok(Some(sway::Expression::create_function_calls(
                Some(sway::Expression::from(sway::AsmBlock {
                    registers: vec![sway::AsmRegister {
                        name: "r1".into(),
                        value: None,
                    }],
                    instructions: vec![sway::AsmInstruction {
                        op_code: "gm".into(),
                        args: vec!["r1".into(), "i4".into()],
                    }],
                    final_expression: Some(sway::AsmFinalExpression {
                        register: "r1".into(),
                        type_name: Some(sway::TypeName::Identifier {
                            name: "u64".into(),
                            generic_parameters: None,
                        }),
                    }),
                })),
                &[("as_u256", Some((None, vec![])))],
            )))
        }

        ("block", "coinbase") => {
            // block.coinbase => {
            //     let ptr = std::alloc::alloc(__size_of::<b256>());
            //     asm(r1: ptr) {
            //         cb r1;
            //     }
            //     Identity::from(ContractId::from(ptr.read::<b256>()))
            // }

            Ok(Some(sway::Expression::from(sway::Block {
                statements: vec![
                    // let ptr = std::alloc::alloc(__size_of::<b256>());
                    sway::Statement::from(sway::Let {
                        pattern: sway::LetPattern::from(sway::LetIdentifier {
                            is_mutable: false,
                            name: "ptr".into(),
                        }),
                        type_name: None,
                        value: sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::create_identifier(
                                "std::alloc::alloc".into(),
                            ),
                            generic_parameters: None,
                            parameters: vec![sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::create_identifier("__size_of".into()),
                                generic_parameters: Some(sway::GenericParameterList {
                                    entries: vec![sway::GenericParameter {
                                        type_name: sway::TypeName::Identifier {
                                            name: "b256".into(),
                                            generic_parameters: None,
                                        },
                                        implements: None,
                                    }],
                                }),
                                parameters: vec![],
                            })],
                        }),
                    }),
                    // asm(r1: ptr) {
                    //     cb r1;
                    // }
                    sway::Statement::from(sway::Expression::from(sway::AsmBlock {
                        registers: vec![sway::AsmRegister {
                            name: "r1".into(),
                            value: Some(sway::Expression::create_identifier("ptr".into())),
                        }],
                        instructions: vec![sway::AsmInstruction {
                            op_code: "cb".into(),
                            args: vec!["r1".into()],
                        }],
                        final_expression: None,
                    })),
                ],

                // Identity::from(ContractId::from(ptr.read::<b256>()))
                final_expr: Some(sway::Expression::create_function_calls(
                    None,
                    &[(
                        "Identity::from",
                        Some((
                            None,
                            vec![sway::Expression::create_function_calls(
                                None,
                                &[(
                                    "ContractId::from",
                                    Some((
                                        None,
                                        vec![sway::Expression::create_function_calls(
                                            None,
                                            &[
                                                ("ptr", None),
                                                (
                                                    "read",
                                                    Some((
                                                        Some(sway::GenericParameterList {
                                                            entries: vec![sway::GenericParameter {
                                                                type_name:
                                                                    sway::TypeName::Identifier {
                                                                        name: "b256".into(),
                                                                        generic_parameters: None,
                                                                    },
                                                                implements: None,
                                                            }],
                                                        }),
                                                        vec![],
                                                    )),
                                                ),
                                            ],
                                        )],
                                    )),
                                )],
                            )],
                        )),
                    )],
                )),
            })))
        }

        ("block", "difficulty") => {
            // block.difficulty => /*unsupported: block.difficulty; using:*/ 0
            Ok(Some(sway::Expression::Commented(
                "unsupported: block.difficulty; using:".into(),
                Box::new(sway::Expression::from(sway::Literal::DecInt(
                    BigUint::zero(),
                    None,
                ))),
            )))
        }

        // TODO: find out the appropriate sway version of `block.gaslimit`
        ("block", "gaslimit") => {
            // block.gaslimit => ???
            Ok(Some(sway::Expression::create_todo(Some(
                "block.gaslimit".into(),
            ))))
        }

        ("block", "number") => {
            // block.number => std::block::height()
            Ok(Some(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::create_identifier("std::block::height".into()),
                generic_parameters: None,
                parameters: vec![],
            })))
        }

        ("block", "prevrandao") => {
            // block.prevrandao => /*unsupported: block.prevrandao; using:*/ 0
            Ok(Some(sway::Expression::Commented(
                "unsupported: block.prevrandao; using:".into(),
                Box::new(sway::Expression::from(sway::Literal::DecInt(
                    BigUint::zero(),
                    None,
                ))),
            )))
        }

        ("block", "timestamp") => {
            // block.timestamp => std::block::timestamp().as_u256()
            Ok(Some(sway::Expression::create_function_calls(
                None,
                &[
                    ("std::block::timestamp", Some((None, vec![]))),
                    ("as_u256", Some((None, vec![]))),
                ],
            )))
        }

        ("msg", "data") => {
            // msg.data => std::inputs::input_message_data(0, 0).unwrap_or(Bytes::new())

            // Ensure `std::bytes::Bytes` is imported
            module.borrow_mut().ensure_use_declared("std::bytes::Bytes");

            Ok(Some(sway::Expression::create_function_calls(
                None,
                &[
                    (
                        "std::inputs::input_message_data",
                        Some((
                            None,
                            vec![
                                sway::Expression::from(sway::Literal::DecInt(
                                    BigUint::zero(),
                                    None,
                                )),
                                sway::Expression::from(sway::Literal::DecInt(
                                    BigUint::zero(),
                                    None,
                                )),
                            ],
                        )),
                    ),
                    (
                        "unwrap_or",
                        Some((
                            None,
                            vec![sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::create_identifier("Bytes::new".into()),
                                generic_parameters: None,
                                parameters: vec![],
                            })],
                        )),
                    ),
                ],
            )))
        }

        ("msg", "sender") => {
            // msg.sender => msg_sender().unwrap()
            Ok(Some(sway::Expression::create_function_calls(
                None,
                &[
                    ("msg_sender", Some((None, vec![]))),
                    ("unwrap", Some((None, vec![]))),
                ],
            )))
        }

        ("msg", "sig") => {
            // msg.sig => /*unsupported: msg.sig; using:*/ [0, 0, 0, 0]
            Ok(Some(sway::Expression::Commented(
                "unsupported: msg.sig; using:".into(),
                Box::new(sway::Expression::from(sway::Array {
                    elements: vec![
                        sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                        sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                        sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                        sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                    ],
                })),
            )))
        }

        ("msg", "value") => {
            // msg.value => std::context::msg_amount()
            Ok(Some(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::create_identifier("std::context::msg_amount".into()),
                generic_parameters: None,
                parameters: vec![],
            })))
        }

        ("tx", "gasprice") => {
            // tx.gasprice => std::tx::tx_gas_price().unwrap_or(0)
            Ok(Some(sway::Expression::create_function_calls(
                None,
                &[
                    ("std::tx::tx_gas_price", Some((None, vec![]))),
                    (
                        "unwrap_or",
                        Some((
                            None,
                            vec![sway::Expression::from(sway::Literal::DecInt(
                                BigUint::zero(),
                                None,
                            ))],
                        )),
                    ),
                ],
            )))
        }

        ("tx", "origin") => {
            // tx.origin => Identity::from(Address::from(/*unsupported: tx.origin; using:*/ ZERO_B256))

            Ok(Some(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::create_identifier("Identity::Address".into()),
                generic_parameters: None,
                parameters: vec![sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::create_identifier("Address::from".into()),
                    generic_parameters: None,
                    parameters: vec![sway::Expression::Commented(
                        "unsupported: tx.origin; using:".into(),
                        Box::new(sway::Expression::create_function_calls(
                            None,
                            &[("b256::zero", Some((None, vec![])))],
                        )),
                    )],
                })],
            })))
        }

        _ => Ok(None),
    }
}

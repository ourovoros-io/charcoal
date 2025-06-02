use crate::{error::Error, project::Project, sway, translate::*};
use num_bigint::BigUint;
use num_traits::Zero;
use solang_parser::{helpers::CodeLocation, pt as solidity};
use std::{cell::RefCell, rc::Rc};

/// Translates solidity build in types
/// Note: function argument is only needed for the loc debug in the end of the function
/// should be removed at some point
pub fn translate_builtin_function_call(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: Rc<RefCell<TranslationScope>>,
    function: &solidity::Expression,
    named_arguments: Option<&[solidity::NamedArgument]>,
    name: &str,
    expression: &solidity::Expression,
    mut parameters: Vec<sway::Expression>,
) -> Result<sway::Expression, Error> {
    match name {
        "blockhash" => {
            // blockhash(block_number) => std::block::block_header_hash(block_height).unwrap_or(0)

            if parameters.len() != 1 {
                panic!("Invalid blockhash call: {expression:#?}");
            }

            Ok(sway::Expression::create_function_calls(
                None,
                &[
                    ("std::block::block_header_hash", Some((None, parameters))),
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
            ))
        }

        "gasleft" => {
            // gasleft() => std::registers::global_gas()

            if !parameters.is_empty() {
                panic!("Invalid gasleft call: {expression:#?}");
            }

            Ok(sway::Expression::create_function_calls(
                None,
                &[("std::registers::global_gas", Some((None, parameters)))],
            ))
        }

        "addmod" => {
            // addmod(x, y, k) => (x + y) % k

            if parameters.len() != 3 {
                panic!("Invalid addmod call: {expression:#?}");
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "%".into(),
                lhs: sway::Expression::Tuple(vec![sway::Expression::from(
                    sway::BinaryExpression {
                        operator: "+".into(),
                        lhs: parameters[0].clone(),
                        rhs: parameters[1].clone(),
                    },
                )]),
                rhs: parameters[2].clone(),
            }))
        }

        "mulmod" => {
            // mulmod(x, y, k) => (x * y) % k

            if parameters.len() != 3 {
                panic!("Invalid mulmod call: {expression:#?}");
            }

            Ok(sway::Expression::from(sway::BinaryExpression {
                operator: "%".into(),
                lhs: sway::Expression::Tuple(vec![sway::Expression::from(
                    sway::BinaryExpression {
                        operator: "*".into(),
                        lhs: parameters[0].clone(),
                        rhs: parameters[1].clone(),
                    },
                )]),
                rhs: parameters[2].clone(),
            }))
        }

        "keccak256" => {
            // keccak256(value) => std::hash::keccak256(value)

            if parameters.len() != 1 {
                panic!("Invalid keccak256 call: {expression:#?}");
            }

            Ok(sway::Expression::create_function_calls(
                None,
                &[("std::hash::keccak256", Some((None, parameters)))],
            ))
        }

        "sha256" => {
            // sha256(value) => std::hash::sha256(value)

            if parameters.len() != 1 {
                panic!("Invalid sha256 call: {expression:#?}");
            }

            Ok(sway::Expression::create_function_calls(
                None,
                &[("std::hash::sha256", Some((None, parameters)))],
            ))
        }

        "ripemd160" => {
            // ripemd160() => /*unsupported: block.basefee; using:*/ 0

            Ok(sway::Expression::Commented(
                "unsupported: ripemd160(); using:".into(),
                Box::new(sway::Expression::from(sway::Literal::DecInt(
                    BigUint::zero(),
                    None,
                ))),
            ))
        }

        "ecrecover" => {
            // ecrecover(hash, v, r, s) => Identity::Address(Address::from(Secp256k1::from((r, s)).address(Message::from(hash)).unwrap()))
            if parameters.len() != 4 {
                panic!("Invalid ecrecover call: {expression:#?}");
            }

            module
                .borrow_mut()
                .ensure_use_declared("std::crypto::secp256k1::Secp256k1");
            module
                .borrow_mut()
                .ensure_use_declared("std::crypto::message::Message");

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
                                        &[
                                            (
                                                "Secp256k1::from",
                                                Some((
                                                    None,
                                                    vec![sway::Expression::Tuple(vec![
                                                        parameters[2].clone(),
                                                        parameters[3].clone(),
                                                    ])],
                                                )),
                                            ),
                                            (
                                                "address",
                                                Some((
                                                    None,
                                                    vec![sway::Expression::create_function_calls(
                                                        None,
                                                        &[(
                                                            "Message::from",
                                                            Some((
                                                                None,
                                                                vec![parameters[0].clone()],
                                                            )),
                                                        )],
                                                    )],
                                                )),
                                            ),
                                            ("unwrap", Some((None, vec![]))),
                                        ],
                                    )],
                                )),
                            )],
                        )],
                    )),
                )],
            ))
        }

        "selfdestruct" => {
            //
            // TODO: how should we handle this?
            //

            Ok(sway::Expression::create_unimplemented(Some(
                "selfdestruct is not supported in sway".into(),
            )))
        }

        "assert" => {
            // assert(x) => assert(x)

            if parameters.len() != 1 {
                panic!("Invalid assert call: {expression:#?}");
            }

            Ok(sway::Expression::create_function_calls(
                None,
                &[("assert", Some((None, parameters)))],
            ))
        }

        "require" => {
            // require(x) => require(x, "Requirement failed: x")
            // require(x, "msg") => require(x, "msg")

            if parameters.len() == 1 {
                parameters.push(sway::Expression::from(sway::Literal::String(format!(
                    "Requirement failed: {}",
                    sway::TabbedDisplayer(&parameters[0])
                ))));
            }

            if parameters.len() != 2 {
                panic!("Invalid require call: {expression:#?}");
            }

            let parameter_type = module
                .borrow_mut()
                .get_expression_type(scope.clone(), &parameters[0])?;

            parameters[0] = coerce_expression(
                &parameters[0],
                &parameter_type,
                &sway::TypeName::Identifier {
                    name: "bool".into(),
                    generic_parameters: None,
                },
            )
            .unwrap();

            Ok(sway::Expression::create_function_calls(
                None,
                &[("require", Some((None, parameters)))],
            ))
        }

        "revert" => {
            // revert() => revert(0)
            // revert("msg") => {
            //     log("msg");
            //     revert(0);
            // }

            if parameters.is_empty() {
                return Ok(sway::Expression::create_function_calls(
                    None,
                    &[(
                        "assert",
                        Some((
                            None,
                            vec![sway::Expression::from(sway::Literal::DecInt(
                                BigUint::zero(),
                                None,
                            ))],
                        )),
                    )],
                ));
            }

            if parameters.len() != 1 {
                panic!("Invalid revert call: {expression:#?}");
            }

            Ok(sway::Expression::from(sway::Block {
                statements: vec![
                    sway::Statement::from(sway::Expression::create_function_calls(
                        None,
                        &[("log", Some((None, parameters)))],
                    )),
                    sway::Statement::from(sway::Expression::create_function_calls(
                        None,
                        &[(
                            "revert",
                            Some((
                                None,
                                vec![sway::Expression::from(sway::Literal::DecInt(
                                    BigUint::zero(),
                                    None,
                                ))],
                            )),
                        )],
                    )),
                ],

                final_expr: None,
            }))
        }

        old_name => {
            let parameter_types = parameters
                .iter()
                .map(|p| module.borrow_mut().get_expression_type(scope.clone(), p))
                .collect::<Result<Vec<_>, _>>()?;

            // Check to see if the expression is a by-value struct constructor
            if let Some(result) = resolve_struct_constructor(
                project,
                module.clone(),
                scope.clone(),
                module.borrow().structs.as_slice(),
                old_name,
                named_arguments,
                parameters.clone(),
                parameter_types.clone(),
            )? {
                return Ok(result);
            }

            // Check to see if the expression is an ABI cast
            if parameters.len() == 1 {
                if let Some(_external_definition) = project.find_module_with_contract(old_name) {
                    match module
                        .borrow_mut()
                        .get_expression_type(scope.clone(), &parameters[0])?
                    {
                        sway::TypeName::Identifier {
                            name,
                            generic_parameters,
                        } => match (name.as_str(), generic_parameters.as_ref()) {
                            ("Identity", None) => {
                                //
                                // TODO: Ensure a use statement for the ABI is added to the current module
                                //

                                // abi(T, x.as_contract_id().unwrap().into())
                                return Ok(sway::Expression::create_function_calls(
                                    None,
                                    &[(
                                        "abi",
                                        Some((
                                            None,
                                            vec![
                                                sway::Expression::create_identifier(
                                                    old_name.into(),
                                                ),
                                                sway::Expression::create_function_calls(
                                                    Some(parameters[0].clone()),
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

                            ("u256" | "b256", None) => {
                                // Thing(x) => abi(Thing, Identity::from(ContractId::from(x)))

                                //
                                // TODO Ensure a use statement for the ABI is added to the current module
                                //

                                // abi(T, Identity::from(ContractId::from(x)))
                                return Ok(sway::Expression::create_function_calls(None, &[
                                    ("abi", Some((None, vec![
                                        sway::Expression::create_identifier(old_name.into()),
                                        sway::Expression::create_function_calls(None, &[
                                            ("Identity::from", Some((None, vec![
                                                // ContractId::from(x)
                                                sway::Expression::create_function_calls(None, &[("ContractId::from", Some((None, vec![parameters[0].clone()])))]),
                                            ]))),
                                        ]),
                                    ]))),
                                ]));
                            }

                            _ => {}
                        },

                        _ => {}
                    }
                }
            }

            // Try to resolve the function call
            if let Some(result) = resolve_function_call(
                project,
                module.clone(),
                scope.clone(),
                module.clone(),
                old_name,
                named_arguments,
                parameters.clone(),
                parameter_types.clone(),
            )? {
                return Ok(result);
            }

            //
            // TODO:
            // Check all of the module's `use` statements for crate-local imports,
            // find the module being imported, then check if the function lives there.
            //

            panic!(
                "{}error: Failed to find function `{old_name}({})` in scope: {function}({})",
                match project.loc_to_line_and_column(module.clone(), &function.loc()) {
                    Some((line, col)) => format!(
                        "{}:{}:{}: ",
                        project
                            .options
                            .input
                            .join(module.borrow().path.clone())
                            .with_extension("sol")
                            .to_string_lossy(),
                        line,
                        col
                    ),
                    None => format!(
                        "{}: ",
                        project
                            .options
                            .input
                            .join(module.borrow().path.clone())
                            .with_extension("sol")
                            .to_string_lossy()
                    ),
                },
                parameter_types
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                parameters
                    .iter()
                    .map(|t| sway::TabbedDisplayer(t).to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            )
        }
    }
}

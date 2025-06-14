use crate::{error::Error, project::Project, sway, translate::*};
use num_bigint::BigUint;
use num_traits::Zero;
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

/// Translates solidity build in types
/// Note: function argument is only needed for the loc debug in the end of the function
/// should be removed at some point
pub fn translate_builtin_function_call(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    name: &str,
    expression: &solidity::Expression,
    mut parameters: Vec<sway::Expression>,
) -> Result<Option<sway::Expression>, Error> {
    match name {
        "blockhash" => {
            // blockhash(block_number) => std::block::block_header_hash(block_height).unwrap_or(0)

            if parameters.len() != 1 {
                panic!("Invalid blockhash call: {expression:#?}");
            }

            Ok(Some(sway::Expression::create_function_calls(
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
            )))
        }

        "gasleft" => {
            // gasleft() => std::registers::global_gas()

            if !parameters.is_empty() {
                panic!("Invalid gasleft call: {expression:#?}");
            }

            Ok(Some(sway::Expression::create_function_calls(
                None,
                &[("std::registers::global_gas", Some((None, parameters)))],
            )))
        }

        "addmod" => {
            // addmod(x, y, k) => (x + y) % k

            if parameters.len() != 3 {
                panic!("Invalid addmod call: {expression:#?}");
            }

            Ok(Some(sway::Expression::from(sway::BinaryExpression {
                operator: "%".into(),
                lhs: sway::Expression::Tuple(vec![sway::Expression::from(
                    sway::BinaryExpression {
                        operator: "+".into(),
                        lhs: parameters[0].clone(),
                        rhs: parameters[1].clone(),
                    },
                )]),
                rhs: parameters[2].clone(),
            })))
        }

        "mulmod" => {
            // mulmod(x, y, k) => (x * y) % k

            if parameters.len() != 3 {
                panic!("Invalid mulmod call: {expression:#?}");
            }

            Ok(Some(sway::Expression::from(sway::BinaryExpression {
                operator: "%".into(),
                lhs: sway::Expression::Tuple(vec![sway::Expression::from(
                    sway::BinaryExpression {
                        operator: "*".into(),
                        lhs: parameters[0].clone(),
                        rhs: parameters[1].clone(),
                    },
                )]),
                rhs: parameters[2].clone(),
            })))
        }

        "keccak256" => {
            // keccak256(value) => std::hash::keccak256(value)

            if parameters.len() != 1 {
                panic!("Invalid keccak256 call: {expression:#?}");
            }

            Ok(Some(sway::Expression::create_function_calls(
                None,
                &[("std::hash::keccak256", Some((None, parameters)))],
            )))
        }

        "sha256" => {
            // sha256(value) => std::hash::sha256(value)

            if parameters.len() != 1 {
                panic!("Invalid sha256 call: {expression:#?}");
            }

            Ok(Some(sway::Expression::create_function_calls(
                None,
                &[("std::hash::sha256", Some((None, parameters)))],
            )))
        }

        "ripemd160" => {
            // ripemd160() => /*unsupported: block.basefee; using:*/ 0

            Ok(Some(sway::Expression::Commented(
                "unsupported: ripemd160(); using:".into(),
                Box::new(sway::Expression::from(sway::Literal::DecInt(
                    BigUint::zero(),
                    None,
                ))),
            )))
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

            Ok(Some(sway::Expression::create_function_calls(
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
            )))
        }

        "selfdestruct" => {
            //
            // TODO: how should we handle this?
            //

            Ok(Some(sway::Expression::create_unimplemented(Some(
                "selfdestruct is not supported in sway".into(),
            ))))
        }

        "assert" => {
            // assert(x) => assert(x)

            if parameters.len() != 1 {
                panic!("Invalid assert call: {expression:#?}");
            }

            Ok(Some(sway::Expression::create_function_calls(
                None,
                &[("assert", Some((None, parameters)))],
            )))
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

            let parameter_type =
                get_expression_type(project, module.clone(), scope.clone(), &parameters[0])?;

            parameters[0] = coerce_expression(
                &parameters[0],
                &parameter_type,
                &sway::TypeName::Identifier {
                    name: "bool".into(),
                    generic_parameters: None,
                },
            )
            .unwrap();

            Ok(Some(sway::Expression::create_function_calls(
                None,
                &[("require", Some((None, parameters)))],
            )))
        }

        "revert" => {
            // revert() => revert(0)
            // revert("msg") => {
            //     log("msg");
            //     revert(0);
            // }

            if parameters.is_empty() {
                return Ok(Some(sway::Expression::create_function_calls(
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
                )));
            }

            if parameters.len() != 1 {
                panic!("Invalid revert call: {expression:#?}");
            }

            Ok(Some(sway::Expression::from(sway::Block {
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
            })))
        }

        _ => Ok(None),
    }
}

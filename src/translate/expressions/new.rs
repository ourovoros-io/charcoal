use crate::{error::Error, project::Project, sway, translate::*};
use num_bigint::BigUint;
use num_traits::{One, Zero};
use solang_parser::{helpers::CodeLocation, pt as solidity};
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_new_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    let solidity::Expression::FunctionCall(_, mut expr, args) = expression.clone() else {
        todo!("translate new expression: {expression:#?}")
    };

    let block_fields = match expr.clone().as_ref() {
        solidity::Expression::FunctionCallBlock(_, function, block) => {
            expr = function.clone();

            let solidity::Statement::Args(_, block_args) = block.as_ref() else {
                panic!("Malformed function call block, expected args block, found: {block:#?}");
            };

            let mut fields = vec![];

            for block_arg in block_args.iter() {
                let value =
                    translate_expression(project, module.clone(), scope.clone(), &block_arg.expr)?;

                match block_arg.name.name.as_str() {
                    "value" => fields.push(sway::ConstructorField {
                        name: "coins".into(),
                        value,
                    }),

                    "gas" => fields.push(sway::ConstructorField {
                        name: "gas".into(),
                        value,
                    }),

                    arg => println!(
                        "{}: WARNING: unsupported function call block arg: {arg}",
                        project.loc_to_file_location_string(module.clone(), &block_arg.loc()),
                    ),
                }
            }

            Some(fields)
        }

        _ => None,
    };

    loop {
        let solidity::Expression::Parenthesis(_, expression) = expr.as_ref() else {
            break;
        };
        expr = expression.clone();
    }

    if let solidity::Expression::New(_, expression) = expr.as_ref() {
        expr = expression.clone();
    }

    let args = args
        .iter()
        .map(|e| translate_expression(project, module.clone(), scope.clone(), e))
        .collect::<Result<Vec<_>, _>>()?;

    match expr.as_ref() {
        solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
            if project.find_contract(&name).is_some() {
                // new Contract(...) => /*unsupported: new Contract(...); using:*/ abi(Contract, Identity::ContractId(ContractId::from(ZERO_B256)))

                return Ok(sway::Expression::Commented(
                    format!("unsupported: new {expression}; using:"),
                    Box::new(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::create_identifier("abi".into()),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::create_identifier(name.clone()),
                            sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::create_identifier(
                                    "Identity::ContractId".into(),
                                ),
                                generic_parameters: None,
                                parameters: vec![sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::create_identifier(
                                        "ContractId::from".into(),
                                    ),
                                    generic_parameters: None,
                                    parameters: vec![sway::Expression::create_function_calls(
                                        None,
                                        &[("b256::zero", Some((None, vec![])))],
                                    )],
                                })],
                            }),
                        ],
                    })),
                ));
            }
        }

        solidity::Expression::Type(_, type_name) => match &type_name {
            solidity::Type::DynamicBytes => {
                // {
                //     let mut v = Bytes::with_capacity(length);
                //     let mut i = 0;
                //     while i < length {
                //         v.push(0);
                //         i += 1;
                //     }
                //     v
                // }

                if block_fields.is_some() {
                    panic!(
                        "Invalid new array expression: expected no block args, found {block_fields:#?}"
                    );
                }

                if args.len() != 1 {
                    panic!(
                        "Invalid new array expression: expected 1 argument, found {}",
                        args.len()
                    );
                }

                // Ensure `std::bytes::Bytes` is imported
                module.borrow_mut().ensure_use_declared("std::bytes::Bytes");

                let length = &args[0];

                return Ok(sway::Expression::from(sway::Block {
                    statements: vec![
                        // let mut v = Vec::with_capacity(length);
                        sway::Statement::from(sway::Let {
                            pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                is_mutable: true,
                                name: "v".into(),
                            }),
                            type_name: None,
                            value: sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::create_identifier(
                                    "Bytes::with_capacity".into(),
                                ),
                                generic_parameters: None,
                                parameters: vec![length.clone()],
                            }),
                        }),
                        // let mut i = 0;
                        sway::Statement::from(sway::Let {
                            pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                is_mutable: true,
                                name: "i".into(),
                            }),
                            type_name: None,
                            value: sway::Expression::from(sway::Literal::DecInt(
                                BigUint::zero(),
                                None,
                            )),
                        }),
                        // while i < length {
                        //     v.push(0);
                        //     i += 1;
                        // }
                        sway::Statement::from(sway::Expression::from(sway::While {
                            // i < length
                            condition: sway::Expression::from(sway::BinaryExpression {
                                operator: "<".into(),
                                lhs: sway::Expression::create_identifier("i".into()),
                                rhs: length.clone(),
                            }),

                            body: sway::Block {
                                statements: vec![
                                    // v.push(0);
                                    sway::Statement::from(sway::Expression::create_function_calls(
                                        None,
                                        &[
                                            ("v", None),
                                            (
                                                "push",
                                                Some((
                                                    None,
                                                    vec![sway::Expression::from(
                                                        sway::Literal::DecInt(
                                                            BigUint::zero(),
                                                            None,
                                                        ),
                                                    )],
                                                )),
                                            ),
                                        ],
                                    )),
                                    // i += 1;
                                    sway::Statement::from(sway::Expression::from(
                                        sway::BinaryExpression {
                                            operator: "+=".into(),
                                            lhs: sway::Expression::create_identifier("i".into()),
                                            rhs: sway::Expression::from(sway::Literal::DecInt(
                                                BigUint::one(),
                                                None,
                                            )),
                                        },
                                    )),
                                ],
                                final_expr: None,
                            },
                        })),
                    ],

                    // v
                    final_expr: Some(sway::Expression::create_identifier("v".into())),
                }));
            }

            solidity::Type::String => {
                // {
                //     let mut v = Bytes::with_capacity(length);
                //     let mut i = 0;
                //     while i < length {
                //         v.push(0);
                //         i += 1;
                //     }
                //     String::from(v)
                // }

                if block_fields.is_some() {
                    panic!(
                        "Invalid new string expression: expected no block args, found {block_fields:#?}"
                    );
                }

                if args.len() != 1 {
                    panic!(
                        "Invalid new string expression: expected 1 argument, found {}",
                        args.len()
                    );
                }

                // Ensure `std::bytes::Bytes` is imported
                module.borrow_mut().ensure_use_declared("std::bytes::Bytes");

                let length = &args[0];

                return Ok(sway::Expression::from(sway::Block {
                    statements: vec![
                        // let mut v = Vec::with_capacity(length);
                        sway::Statement::from(sway::Let {
                            pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                is_mutable: true,
                                name: "v".into(),
                            }),
                            type_name: None,
                            value: sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::create_identifier(
                                    "Bytes::with_capacity".into(),
                                ),
                                generic_parameters: None,
                                parameters: vec![length.clone()],
                            }),
                        }),
                        // let mut i = 0;
                        sway::Statement::from(sway::Let {
                            pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                is_mutable: true,
                                name: "i".into(),
                            }),
                            type_name: None,
                            value: sway::Expression::from(sway::Literal::DecInt(
                                BigUint::zero(),
                                None,
                            )),
                        }),
                        // while i < length {
                        //     v.push(0);
                        //     i += 1;
                        // }
                        sway::Statement::from(sway::Expression::from(sway::While {
                            // i < length
                            condition: sway::Expression::from(sway::BinaryExpression {
                                operator: "<".into(),
                                lhs: sway::Expression::create_identifier("i".into()),
                                rhs: length.clone(),
                            }),

                            body: sway::Block {
                                statements: vec![
                                    // v.push(0);
                                    sway::Statement::from(sway::Expression::create_function_calls(
                                        None,
                                        &[
                                            ("v", None),
                                            (
                                                "push",
                                                Some((
                                                    None,
                                                    vec![sway::Expression::from(
                                                        sway::Literal::DecInt(
                                                            BigUint::zero(),
                                                            None,
                                                        ),
                                                    )],
                                                )),
                                            ),
                                        ],
                                    )),
                                    // i += 1;
                                    sway::Statement::from(sway::Expression::from(
                                        sway::BinaryExpression {
                                            operator: "+=".into(),
                                            lhs: sway::Expression::create_identifier("i".into()),
                                            rhs: sway::Expression::from(sway::Literal::DecInt(
                                                BigUint::one(),
                                                None,
                                            )),
                                        },
                                    )),
                                ],
                                final_expr: None,
                            },
                        })),
                    ],

                    // String::from(v)
                    final_expr: Some(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::create_identifier("String::from".into()),
                        generic_parameters: None,
                        parameters: vec![sway::Expression::create_identifier("v".into())],
                    })),
                }));
            }

            _ => todo!(
                "translate new {} expression: {expression} {expression:#?}",
                type_name.to_string()
            ),
        },

        solidity::Expression::ArraySubscript(_, _, None) => {
            assert!(args.len() == 1);
            return Ok(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::create_identifier("Vec::with_capacity".into()),
                generic_parameters: None,
                parameters: args,
            }));
        }

        _ => {
            // println!(
            //     "{}: WARNING: unsupported function call block arg: {expr}",
            //     project.loc_to_file_location_string(module.clone(), &expr.loc()),
            // );

            todo!("translate new expression: {expr:#?} - {expression:#?}")
        }
    }

    let name = match expr.as_ref() {
        solidity::Expression::ArraySubscript(_, ex, _) => {
            let solidity::Expression::Variable(solidity::Identifier { name, .. }) = ex.as_ref()
            else {
                todo!("translate new expression: {expression:#?}")
            };
            name
        }
        solidity::Expression::Variable(solidity::Identifier { name, .. }) => name,
        _ => todo!("translate new expression: {expression:#?}"),
    };

    match block_fields {
        Some(fields) => Ok(sway::Expression::from(sway::FunctionCallBlock {
            function: sway::Expression::create_identifier(name.clone()),
            generic_parameters: None,
            fields,
            parameters: vec![],
        })),

        None => Ok(sway::Expression::from(sway::FunctionCall {
            function: sway::Expression::create_identifier(name.clone()),
            generic_parameters: None,
            parameters: vec![],
        })),
    }
}

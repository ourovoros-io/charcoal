use std::{cell::RefCell, rc::Rc};
use num_bigint::BigUint;
use num_traits::Zero;
use crate::{errors::Error, project::Project, sway, translate::{TranslatedDefinition, TranslationScope}};

#[inline]
pub fn translate_address_call_expression(
    _project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    payload: sway::Expression,
    coins: Option<sway::Expression>,
    asset_id: Option<sway::Expression>,
    gas: Option<sway::Expression>,
) -> Result<sway::Expression, Error> {
    // to.call(memory) => {
    //     let return_ptr = asm(
    //         r1: payload.buf.ptr,
    //         r2: coins,
    //         r3: asset_id,
    //         r4: gas,
    //     ) {
    //         call r1 r2 r3 r4;
    //         ret: raw_ptr
    //     };
    //     let return_length = asm() {
    //         retl: u64
    //     };
    //     let result_ptr = std::alloc::alloc_bytes(return_length);
    //     return_ptr.copy_to::<u8>(result_ptr, return_length);
    //     (true, Bytes::from(raw_slice::from_parts::<u8>(result_ptr, return_length)))
    // }

    // Ensure `std::bytes::Bytes` is imported
    translated_definition.ensure_use_declared("std::bytes::Bytes");

    // Create unique variable names
    let return_ptr_name = scope.borrow_mut().generate_unique_variable_name("return_ptr");
    let return_length_name = scope.borrow_mut().generate_unique_variable_name("return_length");
    let result_ptr_name = scope.borrow_mut().generate_unique_variable_name("result_ptr");

    Ok(sway::Expression::from(sway::Block {
        statements: vec![
            // let return_ptr = asm(
            //     r1: payload.buf.ptr,
            //     r2: coins,
            //     r3: asset_id,
            //     r4: gas,
            // ) {
            //     call r1 r2 r3 r4;
            //     ret: raw_ptr
            // };
            sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::from(sway::LetIdentifier {
                    is_mutable: false,
                    name: return_ptr_name.clone(),
                }),
                type_name: None,
                value: sway::Expression::from(sway::AsmBlock {
                    registers: vec![
                        sway::AsmRegister {
                            name: "r1".into(),
                            value: Some(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::from(sway::MemberAccess {
                                    expression: payload.clone(),
                                    member: "ptr".into(),
                                }),
                                generic_parameters: None,
                                parameters: vec![],
                            })),
                        },
                        sway::AsmRegister {
                            name: "r2".into(),
                            value: Some(coins.unwrap_or_else(|| sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier("std::inputs::input_amount".into()),
                                generic_parameters: None,
                                parameters: vec![
                                    sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                                ],
                            }))),
                        },
                        sway::AsmRegister {
                            name: "r3".into(),
                            value: Some(asset_id.unwrap_or_else(|| sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::from(sway::MemberAccess {
                                    expression: sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::Identifier("std::inputs::input_asset_id".into()),
                                        generic_parameters: None,
                                        parameters: vec![
                                            sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                                        ],
                                    }),
                                    member: "unwrap".into(),
                                }),
                                generic_parameters: None,
                                parameters: vec![],
                            }))),
                        },
                        sway::AsmRegister {
                            name: "r4".into(),
                            value: Some(gas.unwrap_or_else(|| sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier("std::registers::global_gas".into()),
                                generic_parameters: None,
                                parameters: vec![],
                            }))),
                        },
                    ],

                    instructions: vec![
                        sway::AsmInstruction {
                            op_code: "call".into(),
                            args: vec![
                                "r1".into(),
                                "r2".into(),
                                "r3".into(),
                                "r4".into(),
                            ],
                        },
                    ],

                    final_expression: Some(sway::AsmFinalExpression {
                        register: "ret".into(),
                        type_name: Some(sway::TypeName::Identifier {
                            name: "raw_ptr".into(),
                            generic_parameters: None,
                        }),
                    }),
                }),
            }),

            // let return_length = asm() {
            //     retl: u64
            // };
            sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::from(sway::LetIdentifier {
                    is_mutable: false,
                    name: return_length_name.clone(),
                }),
                type_name: None,
                value: sway::Expression::from(sway::AsmBlock {
                    registers: vec![],
                    instructions: vec![],
                    final_expression: Some(sway::AsmFinalExpression {
                        register: "retl".into(),
                        type_name: Some(sway::TypeName::Identifier {
                            name: "u64".into(),
                            generic_parameters: None,
                        }),
                    }),
                }),
            }),

            // let result_ptr = std::alloc::alloc_bytes(return_length);
            sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::from(sway::LetIdentifier {
                    is_mutable: false,
                    name: result_ptr_name.clone(),
                }),
                type_name: None,
                value: sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::Identifier("std::alloc::alloc_bytes".into()),
                    generic_parameters: None,
                    parameters: vec![
                        sway::Expression::Identifier(return_length_name.clone()),
                    ],
                }),
            }),

            // return_ptr.copy_to::<u8>(result_ptr, return_length);
            sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::from(sway::MemberAccess {
                    expression: sway::Expression::Identifier(return_ptr_name.clone()),
                    member: "copy_to".into(),
                }),
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![
                        sway::GenericParameter {
                            type_name: sway::TypeName::Identifier {
                                name: "u8".into(),
                                generic_parameters: None,
                            },
                            implements: None,
                        }
                    ],
                }),
                parameters: vec![
                    sway::Expression::Identifier(result_ptr_name.clone()),
                    sway::Expression::Identifier(return_length_name.clone()),
                ],
            })),
        ],

        // (true, Bytes::from(raw_slice::from_parts::<u8>(result_ptr, return_length)))
        final_expr: Some(sway::Expression::Tuple(vec![
            sway::Expression::from(sway::Literal::Bool(true)),
            sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::Identifier("Bytes::from".into()),
                generic_parameters: None,
                parameters: vec![
                    sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("raw_slice::from_parts".into()),
                        generic_parameters: Some(sway::GenericParameterList {
                            entries: vec![
                                sway::GenericParameter {
                                    type_name: sway::TypeName::Identifier {
                                        name: "u8".into(),
                                        generic_parameters: None,
                                    },
                                    implements: None,
                                }
                            ],
                        }),
                        parameters: vec![
                            sway::Expression::Identifier(result_ptr_name.clone()),
                            sway::Expression::Identifier(return_length_name.clone()),
                        ],
                    }),
                ],
            })
        ])),
    }))
}

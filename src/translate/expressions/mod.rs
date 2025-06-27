use crate::{error::Error, project::Project, sway, translate::*};
use num_bigint::BigUint;
use num_traits::{Num, Zero};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

mod address_call;
mod array;
mod assignment;
mod conditional;
mod delete;
mod expression_type;
mod function_call;
mod list;
mod literals;
mod member_access;
mod new;
mod parenthesis;
mod pre_post;
mod unary;
mod variable;
pub use address_call::*;
pub use array::*;
pub use assignment::*;
pub use conditional::*;
pub use delete::*;
pub use expression_type::*;
pub use function_call::*;
pub use list::*;
pub use literals::*;
pub use member_access::*;
pub use new::*;
pub use parenthesis::*;
pub use pre_post::*;
pub use unary::*;
pub use variable::*;

pub fn evaluate_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    type_name: &sway::TypeName,
    expression: &sway::Expression,
) -> sway::Expression {
    match expression {
        sway::Expression::Literal(literal) => match literal {
            sway::Literal::DecInt(value, _) | sway::Literal::HexInt(value, _) => match type_name {
                sway::TypeName::Identifier {
                    name,
                    generic_parameters,
                } => match (name.as_str(), generic_parameters.as_ref()) {
                    ("b256", None) if value.is_zero() => sway::Expression::create_function_calls(
                        None,
                        &[("b256::zero", Some((None, vec![])))],
                    ),

                    _ => expression.clone(),
                },

                _ => expression.clone(),
            },

            _ => expression.clone(),
        },

        sway::Expression::PathExpr(path_expr) => {
            let Some(identifier) = path_expr.as_identifier() else {
                todo!("evaluate non-identifier path expression: {path_expr} - {path_expr:#?}")
            };

            if let Some(constant) = module
                .borrow()
                .constants
                .iter()
                .find(|c| c.name == identifier)
            {
                assert!(type_name.is_compatible_with(&constant.type_name));
                return constant.value.as_ref().unwrap().clone();
            }

            if let Some(configurable) = module.borrow().configurable.as_ref() {
                if let Some(field) = configurable.fields.iter().find(|f| f.name == identifier) {
                    assert!(type_name.is_compatible_with(&field.type_name));
                    return field.value.clone();
                }
            }

            let variable = scope
                .borrow()
                .find_variable(|v| v.borrow().new_name == *identifier)
                .unwrap();

            todo!("evaluate path expression: {expression:#?} - {variable:#?}")
        }

        sway::Expression::FunctionCall(function_call) => match &function_call.function {
            sway::Expression::PathExpr(path_expr) => {
                let Some(identifier) = path_expr.as_identifier() else {
                    match path_expr.to_string().as_str() {
                        "Address::from" => {
                            return sway::Expression::from(sway::FunctionCall {
                                function: function_call.function.clone(),
                                generic_parameters: function_call.generic_parameters.clone(),
                                parameters: function_call
                                    .parameters
                                    .iter()
                                    .map(|p| {
                                        evaluate_expression(
                                            project,
                                            module.clone(),
                                            scope.clone(),
                                            type_name,
                                            p,
                                        )
                                    })
                                    .collect(),
                            });
                        }

                        "b256::from_be_bytes" => {
                            // TODO: sigh...
                            return expression.clone();
                        }

                        "b256::zero" => {
                            return expression.clone();
                        }

                        "keccak256" | "std::hash::keccak256"
                            if function_call.parameters.len() == 1 =>
                        {
                            match &function_call.parameters[0] {
                                sway::Expression::Literal(sway::Literal::String(s)) => {
                                    use sha3::Digest;

                                    let mut hasher = sha3::Keccak256::new();
                                    hasher.update(s.as_bytes());

                                    let value = BigUint::from_str_radix(
                                        hex::encode(hasher.finalize()).as_str(),
                                        16,
                                    )
                                    .unwrap();

                                    return sway::Expression::Commented(
                                        format!("{}", sway::TabbedDisplayer(expression)),
                                        Box::new(if value.is_zero() {
                                            sway::Expression::create_function_calls(
                                                None,
                                                &[("b256::zero", Some((None, vec![])))],
                                            )
                                        } else {
                                            sway::Expression::from(sway::Literal::HexInt(
                                                value, None,
                                            ))
                                        }),
                                    );
                                }

                                _ => todo!("evaluate function call: {expression:#?}"),
                            }
                        }

                        "I8::from_uint" | "I16::from_uint" | "I32::from_uint"
                        | "I64::from_uint" | "I128::from_uint" | "I256::from_uint" => {
                            return evaluate_expression(
                                project,
                                module,
                                scope.clone(),
                                type_name,
                                &function_call.parameters[0],
                            );
                        }

                        "Identity::Address" | "Identity::ContractId" => {
                            return sway::Expression::from(sway::FunctionCall {
                                function: function_call.function.clone(),
                                generic_parameters: function_call.generic_parameters.clone(),
                                parameters: function_call
                                    .parameters
                                    .iter()
                                    .map(|p| {
                                        evaluate_expression(
                                            project,
                                            module.clone(),
                                            scope.clone(),
                                            type_name,
                                            p,
                                        )
                                    })
                                    .collect(),
                            });
                        }

                        "u64::max" => {
                            assert!(function_call.parameters.is_empty());
                            return sway::Expression::from(sway::Literal::DecInt(
                                u64::MAX.into(),
                                None,
                            ));
                        }

                        "u64::min" => {
                            assert!(function_call.parameters.is_empty());
                            return sway::Expression::from(sway::Literal::DecInt(
                                u64::MIN.into(),
                                None,
                            ));
                        }

                        "u256::from_be_bytes" => {
                            // TODO: sigh...
                            return expression.clone();
                        }

                        _ => {}
                    }

                    todo!(
                        "evaluate non-identifier path function call expression: {path_expr}({}) - {path_expr:#?}",
                        function_call
                            .parameters
                            .iter()
                            .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
                            .collect::<Result<Vec<_>, _>>()
                            .unwrap()
                            .iter()
                            .map(|t| t.to_string())
                            .collect::<Vec<_>>()
                            .join(", "),
                    )
                };

                match identifier {
                    "todo!" => expression.clone(),

                    "abi" => sway::Expression::create_todo(Some(format!(
                        "{}",
                        sway::TabbedDisplayer(expression)
                    ))),

                    "__to_str_array" => expression.clone(),

                    _ => todo!("evaluate function call: {expression:#?}"),
                }
            }

            sway::Expression::MemberAccess(member_access) => match &member_access.expression {
                sway::Expression::Literal(
                    sway::Literal::DecInt(lhs_value, lhs_suffix)
                    | sway::Literal::HexInt(lhs_value, lhs_suffix),
                ) => match member_access.member.as_str() {
                    "as_u256" if function_call.parameters.is_empty() => expression.clone(),

                    "as_b256" if function_call.parameters.is_empty() => expression.clone(),

                    "wrapping_neg" if function_call.parameters.is_empty() => {
                        //
                        // TODO: This won't compile currently...
                        //       We should probably calculate the underlying unsigned value
                        //       but for now we're just gonna emit code that fails to compile :shrug:
                        //

                        expression.clone()
                    }

                    "pow" if function_call.parameters.len() == 1 => {
                        let rhs = evaluate_expression(
                            project,
                            module,
                            scope.clone(),
                            type_name,
                            &function_call.parameters[0],
                        );

                        let sway::Expression::Literal(
                            sway::Literal::DecInt(rhs_value, _)
                            | sway::Literal::HexInt(rhs_value, _),
                        ) = rhs
                        else {
                            todo!("integer pow rhs expression: {rhs:#?}")
                        };

                        let rhs_value: u32 = rhs_value.to_string().parse().unwrap();

                        sway::Expression::Commented(
                            format!("{}", sway::TabbedDisplayer(expression)),
                            Box::new(sway::Expression::from(sway::Literal::DecInt(
                                lhs_value.pow(rhs_value),
                                if type_name.is_uint() {
                                    // TODO: we should verify the suffix is valid
                                    Some(type_name.to_string().to_lowercase())
                                } else {
                                    lhs_suffix.clone()
                                },
                            ))),
                        )
                    }

                    member => todo!("translate {member} member call: {expression:#?}"),
                },

                _ => {
                    // todo!("evaluate member access call: {expression:#?}")
                    expression.clone()
                }
            },

            _ => todo!("evaluate function call: {expression:#?}"),
        },

        sway::Expression::FunctionCallBlock(_) => {
            todo!("evaluate function call block: {expression:#?}")
        }

        sway::Expression::Block(_)
        | sway::Expression::Return(_)
        | sway::Expression::Array(_)
        | sway::Expression::ArrayAccess(_)
        | sway::Expression::MemberAccess(_)
        | sway::Expression::Tuple(_)
        | sway::Expression::If(_)
        | sway::Expression::Match(_)
        | sway::Expression::While(_)
        | sway::Expression::UnaryExpression(_)
        | sway::Expression::BinaryExpression(_)
        | sway::Expression::Constructor(_)
        | sway::Expression::Continue
        | sway::Expression::Break
        | sway::Expression::AsmBlock(_) => {
            create_value_expression(project, module, scope.clone(), type_name, Some(expression))
        }

        sway::Expression::Commented(comment, expression) => sway::Expression::Commented(
            comment.clone(),
            Box::new(evaluate_expression(
                project,
                module,
                scope.clone(),
                type_name,
                expression,
            )),
        ),
    }
}

pub fn translate_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    // use solang_parser::pt::CodeLocation;
    // println!(
    //     "Translating expression: {expression}; from {}",
    //     project.loc_to_file_location_string(module.clone(), &expression.loc()),
    // );

    match expression {
        solidity::Expression::BoolLiteral(_, _)
        | solidity::Expression::NumberLiteral(_, _, _, _)
        | solidity::Expression::RationalNumberLiteral(_, _, _, _, _)
        | solidity::Expression::HexNumberLiteral(_, _, _)
        | solidity::Expression::AddressLiteral(_, _)
        | solidity::Expression::HexLiteral(_)
        | solidity::Expression::StringLiteral(_) => {
            translate_literal_expression(project, expression)
        }

        solidity::Expression::Type(_, _) => {
            translate_type_expression(project, module, scope.clone(), expression)
        }

        solidity::Expression::Variable(_) => {
            translate_variable_expression(project, module, scope.clone(), expression)
        }

        solidity::Expression::ArrayLiteral(_, expressions) => translate_array_literal_expression(
            project,
            module,
            scope.clone(),
            expressions.as_slice(),
        ),

        solidity::Expression::ArraySubscript(_, _, _) => {
            translate_array_subscript_expression(project, module, scope.clone(), expression)
        }

        solidity::Expression::ArraySlice(_, _, _, _) => {
            translate_array_slice_expression(project, module, scope.clone(), expression)
        }

        solidity::Expression::List(_, parameters) => {
            translate_list_expression(project, module, scope.clone(), parameters.as_slice())
        }

        solidity::Expression::Parenthesis(_, expression) => {
            translate_parenthesis_expression(project, module, scope.clone(), expression)
        }

        solidity::Expression::MemberAccess(_, container, member) => {
            translate_member_access_expression(
                project,
                module,
                scope.clone(),
                expression,
                container,
                member,
            )
        }

        solidity::Expression::FunctionCall(_, function, arguments) => {
            translate_function_call_expression(
                project,
                module,
                scope.clone(),
                expression,
                function,
                None,
                arguments,
            )
        }

        solidity::Expression::FunctionCallBlock(_, function, block) => {
            translate_function_call_block_expression(
                project,
                module,
                scope.clone(),
                function,
                block,
            )
        }

        solidity::Expression::NamedFunctionCall(_, function, named_arguments) => {
            translate_function_call_expression(
                project,
                module,
                scope.clone(),
                expression,
                function,
                Some(named_arguments),
                &[],
            )
        }

        solidity::Expression::Not(_, x) => {
            translate_unary_expression(project, module, scope.clone(), "!", x)
        }

        solidity::Expression::BitwiseNot(_, x) => {
            translate_unary_expression(project, module, scope.clone(), "!", x)
        }

        solidity::Expression::UnaryPlus(_, x) => {
            translate_expression(project, module, scope.clone(), x)
        }

        solidity::Expression::Negate(_, x) => {
            translate_unary_expression(project, module, scope.clone(), "-", x)
        }

        solidity::Expression::Power(_, lhs, rhs) => {
            translate_power_expression(project, module, scope.clone(), lhs, rhs)
        }

        solidity::Expression::Multiply(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "*", lhs, rhs)
        }

        solidity::Expression::Divide(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "/", lhs, rhs)
        }

        solidity::Expression::Modulo(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "%", lhs, rhs)
        }

        solidity::Expression::Add(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "+", lhs, rhs)
        }

        solidity::Expression::Subtract(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "-", lhs, rhs)
        }

        solidity::Expression::ShiftLeft(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "<<", lhs, rhs)
        }

        solidity::Expression::ShiftRight(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), ">>", lhs, rhs)
        }

        solidity::Expression::BitwiseAnd(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "&", lhs, rhs)
        }

        solidity::Expression::BitwiseXor(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "^", lhs, rhs)
        }

        solidity::Expression::BitwiseOr(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "|", lhs, rhs)
        }

        solidity::Expression::Less(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "<", lhs, rhs)
        }

        solidity::Expression::More(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), ">", lhs, rhs)
        }

        solidity::Expression::LessEqual(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "<=", lhs, rhs)
        }

        solidity::Expression::MoreEqual(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), ">=", lhs, rhs)
        }

        solidity::Expression::Equal(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "==", lhs, rhs)
        }

        solidity::Expression::NotEqual(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "!=", lhs, rhs)
        }

        solidity::Expression::And(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "&&", lhs, rhs)
        }

        solidity::Expression::Or(_, lhs, rhs) => {
            translate_binary_expression(project, module, scope.clone(), "||", lhs, rhs)
        }

        solidity::Expression::ConditionalOperator(_, condition, then_value, else_value) => {
            translate_conditional_operator_expression(
                project,
                module,
                scope.clone(),
                condition,
                then_value,
                else_value,
            )
        }

        solidity::Expression::Assign(_, lhs, rhs) => translate_assignment_expression(
            project,
            module,
            scope.clone(),
            "=",
            lhs.as_ref(),
            rhs.as_ref(),
        ),

        solidity::Expression::AssignOr(_, lhs, rhs) => translate_assignment_expression(
            project,
            module,
            scope.clone(),
            "|=",
            lhs.as_ref(),
            rhs.as_ref(),
        ),

        solidity::Expression::AssignAnd(_, lhs, rhs) => translate_assignment_expression(
            project,
            module,
            scope.clone(),
            "&=",
            lhs.as_ref(),
            rhs.as_ref(),
        ),

        solidity::Expression::AssignXor(_, lhs, rhs) => translate_assignment_expression(
            project,
            module,
            scope.clone(),
            "^=",
            lhs.as_ref(),
            rhs.as_ref(),
        ),

        solidity::Expression::AssignShiftLeft(_, lhs, rhs) => translate_assignment_expression(
            project,
            module,
            scope.clone(),
            "<<=",
            lhs.as_ref(),
            rhs.as_ref(),
        ),

        solidity::Expression::AssignShiftRight(_, lhs, rhs) => translate_assignment_expression(
            project,
            module,
            scope.clone(),
            ">>=",
            lhs.as_ref(),
            rhs.as_ref(),
        ),

        solidity::Expression::AssignAdd(_, lhs, rhs) => translate_assignment_expression(
            project,
            module,
            scope.clone(),
            "+=",
            lhs.as_ref(),
            rhs.as_ref(),
        ),

        solidity::Expression::AssignSubtract(_, lhs, rhs) => translate_assignment_expression(
            project,
            module,
            scope.clone(),
            "-=",
            lhs.as_ref(),
            rhs.as_ref(),
        ),

        solidity::Expression::AssignMultiply(_, lhs, rhs) => translate_assignment_expression(
            project,
            module,
            scope.clone(),
            "*=",
            lhs.as_ref(),
            rhs.as_ref(),
        ),

        solidity::Expression::AssignDivide(_, lhs, rhs) => translate_assignment_expression(
            project,
            module,
            scope.clone(),
            "/=",
            lhs.as_ref(),
            rhs.as_ref(),
        ),

        solidity::Expression::AssignModulo(_, lhs, rhs) => translate_assignment_expression(
            project,
            module,
            scope.clone(),
            "%=",
            lhs.as_ref(),
            rhs.as_ref(),
        ),

        solidity::Expression::PreIncrement(_, _)
        | solidity::Expression::PostIncrement(_, _)
        | solidity::Expression::PreDecrement(_, _)
        | solidity::Expression::PostDecrement(_, _) => {
            translate_pre_or_post_operator_value_expression(
                project,
                module,
                scope.clone(),
                expression,
            )
        }

        solidity::Expression::New(_, expression) => {
            translate_new_expression(project, module, scope.clone(), expression)
        }

        solidity::Expression::Delete(_, expression) => {
            translate_delete_expression(project, module, scope.clone(), expression)
        }
    }
}

pub fn create_value_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    type_name: &sway::TypeName,
    value: Option<&sway::Expression>,
) -> sway::Expression {
    if let Some(value) = value {
        let value_type =
            get_expression_type(project, module.clone(), scope.clone(), value).unwrap();

        return coerce_expression(
            project,
            module.clone(),
            scope.clone(),
            value,
            &value_type,
            type_name,
        )
        .unwrap();
    }

    match type_name {
        sway::TypeName::Undefined => panic!("Undefined type name"),

        sway::TypeName::Identifier {
            name,
            generic_parameters,
        } => match (name.as_str(), generic_parameters.as_ref()) {
            ("todo!", None) => {
                sway::Expression::create_function_calls(None, &[("todo!", Some((None, vec![])))])
            }

            ("bool", None) => sway::Expression::Literal(sway::Literal::Bool(false)),

            ("b256", None) => sway::Expression::create_function_calls(
                None,
                &[("b256::zero", Some((None, vec![])))],
            ),

            ("I8" | "I16" | "I32" | "I64" | "I128" | "I256", None) => {
                let value = match name.as_str() {
                    "I128" => {
                        module.borrow_mut().ensure_use_declared("std::u128::*");
                        sway::Expression::create_function_calls(
                            None,
                            &[("U128::zero", Some((None, vec![])))],
                        )
                    }

                    _ => sway::Expression::from(sway::Literal::DecInt(
                        BigUint::zero(),
                        Some(format!("u{}", name.trim_start_matches('I'))),
                    )),
                };

                let value_type =
                    get_expression_type(project, module.clone(), scope.clone(), &value).unwrap();

                coerce_expression(
                    project,
                    module.clone(),
                    scope.clone(),
                    &value,
                    &value_type,
                    type_name,
                )
                .unwrap()
            }

            ("u8" | "u16" | "u32" | "u64" | "u256", None) => {
                sway::Expression::Literal(sway::Literal::DecInt(BigUint::zero(), None))
            }

            ("raw_ptr", None) => {
                // Ensure `std::alloc::alloc_bytes` is imported
                module
                    .borrow_mut()
                    .ensure_dependency_declared("std::alloc::alloc_bytes");

                // alloc_bytes(0)
                sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::create_identifier("alloc_bytes".into()),
                    generic_parameters: None,
                    parameters: vec![sway::Expression::from(sway::Literal::DecInt(
                        0u8.into(),
                        None,
                    ))],
                })
            }

            ("Bytes", None) => {
                // Ensure `std::bytes::Bytes` is imported
                module.borrow_mut().ensure_use_declared("std::bytes::Bytes");

                sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::create_identifier("Bytes::new".into()),
                    generic_parameters: None,
                    parameters: vec![],
                })
            }

            ("Identity", None) => sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::create_identifier("Identity::Address".into()),
                generic_parameters: None,
                parameters: vec![sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::create_identifier("Address::from".into()),
                    generic_parameters: None,
                    parameters: vec![sway::Expression::create_function_calls(
                        None,
                        &[("b256::zero", Some((None, vec![])))],
                    )],
                })],
            }),

            ("Option", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                sway::Expression::create_identifier("None".into())
            }

            ("StorageKey", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                sway::Expression::create_todo(Some("create storage key".into()))
            }

            ("StorageMap", Some(_)) => sway::Expression::from(sway::Constructor {
                type_name: sway::TypeName::Identifier {
                    name: "StorageMap".into(),
                    generic_parameters: None,
                },
                fields: vec![],
            }),

            ("StorageString", None) => sway::Expression::from(sway::Constructor {
                type_name: sway::TypeName::Identifier {
                    name: "StorageString".into(),
                    generic_parameters: None,
                },
                fields: vec![],
            }),

            ("StorageVec", Some(_)) => sway::Expression::from(sway::Constructor {
                type_name: sway::TypeName::Identifier {
                    name: "StorageVec".into(),
                    generic_parameters: None,
                },
                fields: vec![],
            }),

            ("String", None) => sway::Expression::create_function_calls(
                None,
                &[("String::new", Some((None, vec![])))],
            ),

            ("Vec", Some(_)) => sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::create_identifier("Vec::new".into()),
                generic_parameters: None,
                parameters: vec![],
            }),

            (name, _) => {
                // Check to see if the type is a type definition
                if project.is_type_definition_declared(module.clone(), name) {
                    let underlying_type = get_underlying_type(project, module.clone(), type_name);
                    return create_value_expression(
                        project,
                        module,
                        scope.clone(),
                        &underlying_type,
                        None,
                    );
                }
                // Check to see if the type is a translated enum
                else if let Some(translated_enum) = project.find_enum(module.clone(), name) {
                    let Some(sway::ImplItem::Constant(value)) =
                        translated_enum.variants_impl.items.first()
                    else {
                        let underlying_type =
                            get_underlying_type(project, module.clone(), type_name);
                        return create_value_expression(
                            project,
                            module.clone(),
                            scope.clone(),
                            &underlying_type,
                            None,
                        );
                    };

                    return sway::Expression::create_identifier(format!(
                        "{}::{}",
                        name, value.name
                    ));
                }
                // Check to see if the type is a struct definition
                else if let Some(struct_definition) =
                    project.find_struct(module.clone(), scope.clone(), name)
                {
                    return sway::Expression::from(sway::Constructor {
                        type_name: sway::TypeName::Identifier {
                            name: name.to_string(),
                            generic_parameters: None,
                        },
                        fields: struct_definition
                            .borrow()
                            .fields
                            .iter()
                            .map(|f| sway::ConstructorField {
                                name: f.name.clone(),
                                value: create_value_expression(
                                    project,
                                    module.clone(),
                                    scope.clone(),
                                    &f.type_name,
                                    None,
                                ),
                            })
                            .collect(),
                    });
                }

                panic!("Unknown value type: {type_name}")
            }
        },

        sway::TypeName::Array { type_name, length } => sway::Expression::Array(sway::Array {
            elements: (0..*length)
                .map(|_| {
                    create_value_expression(project, module.clone(), scope.clone(), type_name, None)
                })
                .collect(),
        }),

        sway::TypeName::Tuple { type_names } => sway::Expression::Tuple(
            type_names
                .iter()
                .map(|type_name| {
                    create_value_expression(project, module.clone(), scope.clone(), type_name, None)
                })
                .collect(),
        ),

        sway::TypeName::StringSlice => sway::Expression::from(sway::Literal::String(String::new())),

        sway::TypeName::StringArray { length } => sway::Expression::from(sway::FunctionCall {
            function: sway::Expression::create_identifier("__to_str_array".into()),
            generic_parameters: None,
            parameters: vec![sway::Expression::Literal(sway::Literal::String(
                (0..*length).map(|_| " ").collect(),
            ))],
        }),

        sway::TypeName::Function { .. } => {
            sway::Expression::create_todo(Some(type_name.to_string()))
        }

        sway::TypeName::Abi { .. } => sway::Expression::from(sway::FunctionCall {
            function: sway::Expression::create_identifier("Identity::Address".into()),
            generic_parameters: None,
            parameters: vec![sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::create_identifier("Address::from".into()),
                generic_parameters: None,
                parameters: vec![sway::Expression::create_function_calls(
                    None,
                    &[("b256::zero", Some((None, vec![])))],
                )],
            })],
        }),
    }
}

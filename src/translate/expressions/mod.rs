use std::{cell::RefCell, clone, rc::Rc};
use function_call::utils::coerce_expression;
use num_bigint::BigUint;
use num_traits::{Num, Zero};
use solang_parser::pt as solidity;
use crate::{errors::Error, project::Project, sway};
use super::{TranslatedDefinition, TranslationScope};

pub mod address_call;
pub mod array;
pub mod assignment;
pub mod binary_unary;
pub mod conditional;
pub mod delete;
pub mod function_call;
pub mod literal;
pub mod list;
pub mod member_access;
pub mod new;
pub mod parenthesis;
pub mod pre_post;
pub mod expression_type;
pub mod variable;

pub fn evaluate_expression(
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    type_name: &sway::TypeName,
    expression: &sway::Expression,
) -> sway::Expression {
    match expression {
        sway::Expression::Literal(literal) => match literal {
            sway::Literal::DecInt(value, _) | sway::Literal::HexInt(value, _) => match type_name {
                sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                    ("b256", None) if value.is_zero() => {
                        // Ensure `std::constants::ZERO_B256` is imported
                        translated_definition.ensure_use_declared("std::constants::ZERO_B256");

                        sway::Expression::Identifier("ZERO_B256".into())
                    }

                    _ => expression.clone(),
                }

                _ => expression.clone(),
            }

            _ => expression.clone(),
        }
        
        sway::Expression::Identifier(identifier) => {
            let variable = scope.borrow().find_variable(|v| v.borrow().new_name == *identifier).unwrap();

            if variable.borrow().is_constant {
                if let Some(constant) = translated_definition.constants.iter().find(|c| c.name == variable.borrow().new_name) {
                    assert!(type_name.is_compatible_with(&constant.type_name));
                    return constant.value.as_ref().unwrap().clone();
                }
            }
            
            if variable.borrow().is_configurable {
                if let Some(configurable) = translated_definition.configurable.as_ref() {
                    if let Some(field) = configurable.fields.iter().find(|f| f.name == variable.borrow().new_name) {
                        assert!(type_name.is_compatible_with(&field.type_name));
                        return field.value.clone();
                    }
                }
            }

            todo!("evaluate identifier: {expression:#?} - {variable:#?}")
        }
        
        sway::Expression::FunctionCall(function_call) => match &function_call.function {
            sway::Expression::Identifier(identifier) => match identifier.as_str() {
                "todo!" => expression.clone(),

                "abi" => sway::Expression::create_todo(Some(format!("{}", sway::TabbedDisplayer(expression)))),

                "Address::from" => {
                    sway::Expression::from(sway::FunctionCall {
                        function: function_call.function.clone(),
                        generic_parameters: function_call.generic_parameters.clone(),
                        parameters: function_call.parameters.iter().map(|p| evaluate_expression(translated_definition, scope.clone(), type_name, p)).collect(),
                    })
                }

                "b256::from_be_bytes" => {
                    // TODO: sigh...
                    expression.clone()
                }

                "keccak256" | "std::hash::keccak256" if function_call.parameters.len() == 1 => match &function_call.parameters[0] {
                    sway::Expression::Literal(sway::Literal::String(s)) => {
                        use sha3::Digest;

                        let mut hasher = sha3::Keccak256::new();
                        hasher.update(s.as_bytes());

                        let value = BigUint::from_str_radix(hex::encode(hasher.finalize()).as_str(), 16).unwrap();

                        return sway::Expression::Commented(
                            format!("{}", sway::TabbedDisplayer(expression)),
                            Box::new(
                                if value.is_zero() {
                                    // Ensure `std::constants::ZERO_B256` is imported
                                    translated_definition.ensure_use_declared("std::constants::ZERO_B256");

                                    sway::Expression::Identifier("ZERO_B256".into())
                                } else {
                                    sway::Expression::from(sway::Literal::HexInt(value, None))
                                }
                            ),
                        );
                    }

                    _ => todo!("evaluate function call: {expression:#?}"),
                }

                "I8::from_uint"
                | "I16::from_uint"
                | "I32::from_uint"
                | "I64::from_uint"
                | "I128::from_uint"
                | "I256::from_uint" => {
                    evaluate_expression(translated_definition, scope, type_name, &function_call.parameters[0])
                }

                "Identity::Address" | "Identity::ContractId" => {
                    sway::Expression::from(sway::FunctionCall {
                        function: function_call.function.clone(),
                        generic_parameters: function_call.generic_parameters.clone(),
                        parameters: function_call.parameters.iter().map(|p| evaluate_expression(translated_definition, scope.clone(), type_name, p)).collect(),
                    })
                }

                "u64::max" => {
                    assert!(function_call.parameters.is_empty());
                    sway::Expression::from(sway::Literal::DecInt(u64::MAX.into(), None))
                }

                "u64::min" => {
                    assert!(function_call.parameters.is_empty());
                    sway::Expression::from(sway::Literal::DecInt(u64::MIN.into(), None))
                }

                "__to_str_array" => expression.clone(),

                _ => todo!("evaluate function call: {expression:#?}"),
            }

            sway::Expression::MemberAccess(member_access) => match &member_access.expression {
                sway::Expression::Literal(
                    sway::Literal::DecInt(lhs_value, lhs_suffix) |
                    sway::Literal::HexInt(lhs_value, lhs_suffix)
                ) => match member_access.member.as_str() {
                    "as_u256" if function_call.parameters.is_empty() => {
                        expression.clone()
                    }

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
                            translated_definition,
                            scope.clone(),
                            type_name,
                            &function_call.parameters[0],
                        );
                        
                        let sway::Expression::Literal(
                            sway::Literal::DecInt(rhs_value, _) |
                            sway::Literal::HexInt(rhs_value, _)
                        ) = rhs else {
                            todo!("integer pow rhs expression: {rhs:#?}")
                        };

                        let rhs_value: u32 = rhs_value.to_string().parse().unwrap();
                        
                        sway::Expression::Commented(
                            format!("{}", sway::TabbedDisplayer(expression)),
                            Box::new(sway::Expression::from(sway::Literal::DecInt(
                                lhs_value.pow(rhs_value.into()),
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
                }

                _ => {
                    // todo!("evaluate member access call: {expression:#?}")
                    expression.clone()
                }
            }

            _ => todo!("evaluate function call: {expression:#?}"),
        }

        sway::Expression::FunctionCallBlock(_) => todo!("evaluate function call block: {expression:#?}"),

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
        | sway::Expression::AsmBlock(_) => create_value_expression(translated_definition, scope, type_name, Some(expression)),
        
        sway::Expression::Commented(comment, expression) => sway::Expression::Commented(
            comment.clone(),
            Box::new(evaluate_expression(translated_definition, scope, type_name, expression)),
        ),
    }
}

pub fn create_value_expression(
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    type_name: &sway::TypeName,
    value: Option<&sway::Expression>,
) -> sway::Expression {
    match type_name {
        sway::TypeName::Undefined => panic!("Undefined type name"),
        
        sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
            ("bool", None) => match value {
                None => sway::Expression::Literal(sway::Literal::Bool(false)),
                Some(sway::Expression::Literal(sway::Literal::Bool(value))) => sway::Expression::Literal(sway::Literal::Bool(*value)),
                
                Some(sway::Expression::UnaryExpression(unary_expression)) => match unary_expression.operator.as_str() {
                    "!" => value.unwrap().clone(),
                    _ => panic!("Invalid bool value expression: {value:#?}"),
                }
                
                Some(sway::Expression::BinaryExpression(binary_expression)) => match binary_expression.operator.as_str() {
                    "==" | "!=" | ">" | "<" | ">=" | "<=" | "&&" | "||" => value.unwrap().clone(),
                    _ => panic!("Invalid bool value expression: {value:#?}"),
                }

                Some(value) => panic!("Invalid bool value expression: {value:#?}"),
            }

            ("b256", None) => match value {
                None => {
                    // Ensure `std::constants::ZERO_B256` is imported
                    translated_definition.ensure_use_declared("std::constants::ZERO_B256");

                    sway::Expression::Identifier("ZERO_B256".into())
                }

                Some(value) => {
                    if matches!(value, sway::Expression::Literal(sway::Literal::DecInt(_, _) | sway::Literal::HexInt(_, _))) {
                        return value.clone();
                    }

                    let value_type_name = translated_definition.get_expression_type(scope.clone(), value).unwrap();

                    match value_type_name {
                        sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                            ("b256", None) => value.clone(),
                            _ => panic!("Invalid {name} value expression: {value:#?}"),
                        }

                        sway::TypeName::StringSlice => match value {
                            sway::Expression::Literal(sway::Literal::String(s)) => {
                                if s.len() > 64 {
                                    panic!("Invalid {name} value expression: {value:#?}");
                                }
                                
                                let mut s = s.bytes().collect::<Vec<u8>>();

                                while s.len() < 64 {
                                    s.push(0);
                                }

                                let s = s.iter().map(|b| format!("{b:02X}")).collect::<String>();
                                sway::Expression::from(sway::Literal::HexInt(BigUint::from_str_radix(&s, 16).unwrap(), None))
                            }

                            _ => panic!("Invalid {name} value expression: {value:#?} {value_type_name:#?}"),
                        }

                        sway::TypeName::Array { type_name, length } if type_name.to_string() == "u8" && length == 32 => {
                            sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier("b256::from_be_bytes".into()),
                                generic_parameters: None,
                                parameters: vec![
                                    value.clone(),
                                ],
                            })
                        }

                        _ => panic!("Invalid {name} value expression: {value:#?} {value_type_name:#?}"),
                    }
                }
            }

            ("I8" | "I16" | "I32" | "I64" | "I128" | "I256", None) => {
                let mut value = match value.cloned() {
                    Some(value) => value,
                    None => sway::Expression::from(sway::Literal::DecInt(
                        BigUint::zero(),
                        Some(format!("u{}", name.trim_start_matches("I").to_string())),
                    )),
                };

                match &value {
                    sway::Expression::Literal(sway::Literal::DecInt(_, _) | sway::Literal::HexInt(_, _)) => {
                        sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier(format!("{name}::from_uint").into()),
                            generic_parameters: None,
                            parameters: vec![value.clone()],
                        })
                    }
                    
                    sway::Expression::FunctionCall(function_call) => match &function_call.function {
                        sway::Expression::Identifier(name) if name == "todo!" => value.clone(),
                        
                        _ => {
                            let mut value_type_name = translated_definition.get_expression_type(scope.clone(), &value).unwrap();
                            
                            if value_type_name.is_uint() {
                                coerce_expression(&mut value, &mut value_type_name, &type_name);
                            }

                            if !value_type_name.is_int() {
                                panic!("Invalid {name} value expression: {} ({value_type_name})", sway::TabbedDisplayer(&value))
                            }
                            
                            value.clone()
                        }
                    }
                    
                    x if matches!(x, sway::Expression::BinaryExpression(_)) => (x).clone(),
                    
                    sway::Expression::Identifier(ref name) => {
                        let Some(variable) = scope.borrow().get_variable_from_new_name(&name) else {
                            panic!("error: Variable not found in scope: \"{name}\"");
                        };
                        
                        if variable.borrow().type_name != *type_name {
                            panic!("Invalid {name} value expression: {value:#?}");
                        }
                        
                        sway::Expression::Identifier(name.clone())
                    }
                    
                    _ => {
                        let value_type_name = translated_definition.get_expression_type(scope.clone(), &value).unwrap();
                        
                        if value_type_name != *type_name {
                            panic!("Invalid {name} value expression: {value:#?}")
                        }
                        
                        sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier(format!("{name}::from_uint").into()),
                            generic_parameters: None,
                            parameters: vec![value.clone()],
                        })
                    }
                }
            }

            ("u8" | "u16" | "u32" | "u64" | "u256", None) => match value.as_ref() {
                None => sway::Expression::Literal(sway::Literal::DecInt(BigUint::zero(), None)),
                
                Some(value) if matches!(value, sway::Expression::Literal(sway::Literal::DecInt(_, _) | sway::Literal::HexInt(_, _))) => (*value).clone(),
                
                Some(sway::Expression::FunctionCall(function_call)) => match &function_call.function {
                    sway::Expression::Identifier(identifier_name) => match identifier_name.as_str() {
                        "todo!" => value.unwrap().clone(),
                        s if s == format!("{name}::max") => value.unwrap().clone(),
                        s if s == format!("{name}::min") => value.unwrap().clone(),
                        _ => panic!("Invalid {name} value expression: {value:#?}"),
                    }

                    sway::Expression::MemberAccess(member_access) => match member_access.member.as_str() {
                        "pow" => {
                            let type_name = translated_definition.get_expression_type(scope.clone(), &member_access.expression).unwrap();
                    
                            if !type_name.is_uint() {
                                panic!("Invalid {name} value expression: {value:#?}")
                            }
                    
                            value.unwrap().clone()
                        }

                        "as_u256" => {
                            value.unwrap().clone()
                        }
                    
                        _ => panic!("Invalid {name} value expression: {value:#?}"),
                    }

                    _ => panic!("Invalid {name} value expression: {value:#?}"),
                }

                Some(x) if matches!(x, sway::Expression::BinaryExpression(_)) => (*x).clone(),

                Some(sway::Expression::Identifier(name)) => {
                    let Some(variable) = scope.borrow().get_variable_from_new_name(name) else {
                        panic!("error: Variable not found in scope: \"{name}\"");
                    };
            
                    if variable.borrow().type_name != *type_name {
                        panic!("Invalid {name} value expression: {value:#?}");
                    }

                    sway::Expression::Identifier(name.clone())
                }
                
                Some(value) => panic!("Invalid {name} value expression: {value:#?}"),
            }

            ("raw_ptr", None) => {
                // Ensure `std::alloc::alloc_bytes` is imported
                translated_definition.ensure_dependency_declared("std::alloc::alloc_bytes");

                // alloc_bytes(0)
                sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::Identifier("alloc_bytes".into()),
                    generic_parameters: None,
                    parameters: vec![
                        sway::Expression::from(sway::Literal::DecInt(0u8.into(), None)),
                    ],
                })
            }

            ("Bytes", None) => {
                // Ensure `std::bytes::Bytes` is imported
                translated_definition.ensure_use_declared("std::bytes::Bytes");

                sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::Identifier("Bytes::new".into()),
                    generic_parameters: None,
                    parameters: vec![],
                })
            }

            ("Identity", None) => match value {
                None => sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::Identifier("Identity::Address".into()),
                    generic_parameters: None,
                    parameters: vec![
                        sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier("Address::from".into()),
                            generic_parameters: None,
                            parameters: vec![
                                {
                                    // Ensure `std::constants::ZERO_B256` is imported
                                    translated_definition.ensure_use_declared("std::constants::ZERO_B256");
            
                                    sway::Expression::Identifier("ZERO_B256".into())
                                },
                            ],
                        })
                    ],
                }),

                Some(value) => value.clone(),
            }

            ("Option", Some(generic_parameters)) if generic_parameters.entries.len() ==1 => match value {
                Some(value) => value.clone(),
                None => sway::Expression::Identifier("None".into())
            }

            ("StorageKey", Some(generic_parameters)) if generic_parameters.entries.len() == 1 => {
                sway::Expression::create_todo(Some("create storage key".into()))
            }

            ("StorageMap", Some(_)) => match value {
                None => sway::Expression::from(sway::Constructor {
                    type_name: sway::TypeName::Identifier {
                        name: "StorageMap".into(),
                        generic_parameters: None,
                    },
                    fields: vec![],
                }),

                Some(value) => panic!("Invalid StorageMap value expression: {value:#?}"),
            }

            ("StorageString", None) => match value {
                None => sway::Expression::from(sway::Constructor {
                    type_name: sway::TypeName::Identifier {
                        name: "StorageString".into(),
                        generic_parameters: None,
                    },
                    fields: vec![],
                }),

                Some(value) => panic!("Invalid StorageString value expression: {value:#?}"),
            }

            ("StorageVec", Some(_)) => match value {
                None => sway::Expression::from(sway::Constructor {
                    type_name: sway::TypeName::Identifier {
                        name: "StorageVec".into(),
                        generic_parameters: None,
                    },
                    fields: vec![],
                }),

                Some(value) => panic!("Invalid StorageVec value expression: {value:#?}"),
            }

            ("Vec", Some(_)) => sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::Identifier("Vec::new".into()),
                generic_parameters: None,
                parameters: vec![],
            }),

            (name, _) => {
                // Check to see if the type is a type definition
                if translated_definition.type_definitions.iter().any(|s| {
                    let sway::TypeName::Identifier { name: type_name, generic_parameters: None } = &s.name else { return false };
                    type_name == name
                }) {
                    let underlying_type = translated_definition.get_underlying_type(type_name);
                    return create_value_expression(translated_definition, scope, &underlying_type, value);
                }
                // Check to see if the type is a translated enum
                else if let Some(translated_enum) = translated_definition.enums.iter().find(|s| {
                    let sway::TypeName::Identifier { name: enum_name, generic_parameters: None } = &s.type_definition.name else { return false };
                    enum_name == name
                }) {
                    let Some(sway::ImplItem::Constant(value)) = translated_enum.variants_impl.items.first() else {
                        let underlying_type = translated_definition.get_underlying_type(type_name);
                        return create_value_expression(translated_definition, scope, &underlying_type, value);
                    };

                    return sway::Expression::Identifier(format!("{}::{}", name, value.name));
                }
                // Check to see if the type is a struct definition
                else if let Some(struct_definition) = translated_definition.structs.iter().find(|s| s.borrow().name == *name).cloned() {
                    return sway::Expression::from(sway::Constructor {
                        type_name: sway::TypeName::Identifier {
                            name: name.to_string(),
                            generic_parameters: None,
                        },
                        fields: struct_definition.borrow().fields.iter().map(|f| sway::ConstructorField {
                            name: f.name.clone(),
                            value: create_value_expression(translated_definition, scope.clone(), &f.type_name, value)
                        }).collect(),
                    });
                }

                panic!("Unknown value type: {type_name}")
            }
        },

        sway::TypeName::Array { type_name, length } => match value {
            None => sway::Expression::Array(sway::Array {
                elements: (0..*length).map(|_| create_value_expression(translated_definition, scope.clone(), type_name, None)).collect(),
            }),

            Some(sway::Expression::Array(value)) => {
                if value.elements.len() != *length {
                    panic!("Invalid array value expression, expected {} elements, found {}: {value:#?}", *length, value.elements.len());
                }

                sway::Expression::Array(value.clone())
            }

            Some(sway::Expression::Literal(sway::Literal::String(s))) => {
                if s.len() != *length {
                    panic!("Invalid array value string, expected {} characters, found {}: \"{s}\"", *length, s.len());
                }

                sway::Expression::Array(sway::Array {
                    elements: s.chars().map(|c| sway::Expression::Literal(sway::Literal::HexInt(BigUint::from(c as u8), None))).collect(),
                })
            }

            Some(value) => match type_name.as_ref() {
                sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                    ("u8", None) => match value {
                        sway::Expression::Literal(sway::Literal::DecInt(value, _) | sway::Literal::HexInt(value, _)) => {
                            sway::Expression::from(sway::Array {
                                elements: value.to_bytes_be().iter().map(|b| sway::Expression::from(sway::Literal::HexInt((*b).into(), None))).collect(),
                            })
                        }

                        _ => sway::Expression::create_todo(Some(format!("{}", sway::TabbedDisplayer(value)))),
                    }

                    _ => panic!("Invalid {type_name} array value expression: {value:#?}"),
                }

                _ => panic!("Invalid {type_name} array value expression: {value:#?}"),
            }
        }

        sway::TypeName::Tuple { type_names } => match value {
            None => sway::Expression::Tuple(
                type_names.iter().map(|type_name| create_value_expression(translated_definition, scope.clone(), type_name, None)).collect()
            ),

            Some(sway::Expression::Tuple(value)) if value.len() == type_names.len() => sway::Expression::Tuple(value.clone()),

            Some(value) => panic!("Invalid tuple value expression: {value:#?}"),
        }

        sway::TypeName::StringSlice => match value {
            None => sway::Expression::from(sway::Literal::String(String::new())),
            Some(sway::Expression::Literal(sway::Literal::String(value))) => sway::Expression::from(sway::Literal::String(value.clone())),
            Some(value) => panic!("Invalid string slice value expression: {value:#?}"),
        }

        sway::TypeName::StringArray { length } => match value {
            None => sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::Identifier("__to_str_array".into()),
                generic_parameters: None,
                parameters: vec![
                    sway::Expression::Literal(sway::Literal::String((0..*length).map(|_| " ").collect())),
                ],
            }),

            Some(sway::Expression::Literal(sway::Literal::String(value))) => {
                if value.len() > *length {
                    panic!("Invalid string value expression, string is {} characters long, expected {}: {value}", value.len(), *length);
                }

                let mut value = value.clone();

                while value.len() < *length {
                    value.push(' ');
                }

                sway::Expression::FunctionCall(Box::new(sway::FunctionCall {
                    function: sway::Expression::Identifier("__to_str_array".into()),
                    generic_parameters: None,
                    parameters: vec![
                        sway::Expression::Literal(sway::Literal::String(value)),
                    ],
                }))
            }

            Some(sway::Expression::FunctionCall(f)) => {
                let sway::Expression::Identifier(id) = &f.function else {
                    panic!("Invalid string value expression, expected `__to_str_array` function call, found: {value:#?}");
                };

                if id != "__to_str_array" {
                    panic!("Invalid string value expression, expected `__to_str_array` function call, found: {value:#?}");
                }

                if f.parameters.len() != 1 {
                    panic!("Invalid string value expression, invalid parameters supplied to `__to_str_array` function call, found: {value:#?}");
                }

                let sway::Expression::Literal(sway::Literal::String(value)) = &f.parameters[0] else {
                    panic!("Invalid string value expression, expected string literal to be supplied to `__to_str_array` function call, found: {value:#?}");
                };

                if value.len() > *length {
                    panic!("Invalid string value expression, string is {} characters long, expected {}: {value}", value.len(), *length);
                }

                let mut value = value.clone();

                while value.len() < *length {
                    value.push(' ');
                }

                sway::Expression::FunctionCall(Box::new(sway::FunctionCall {
                    function: sway::Expression::Identifier("__to_str_array".into()),
                    generic_parameters: None,
                    parameters: vec![
                        sway::Expression::Literal(sway::Literal::String(value)),
                    ],
                }))
            }

            Some(value) => panic!("Invalid string array value expression: {value:#?}"),
        }
    
        sway::TypeName::Function { .. } => match value {
            Some(value) => sway::Expression::create_todo(Some(sway::TabbedDisplayer(value).to_string())),
            None => sway::Expression::create_todo(Some(type_name.to_string())),
        }
    }
}

pub fn translate_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    // println!(
    //     "Translating expression: {expression}; from {}",
    //     match project.loc_to_line_and_column(&translated_definition.path, &expression.loc()) {
    //         Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
    //         None => format!("{} - ", translated_definition.path.to_string_lossy()),
    //     },
    // );

    match expression {
        solidity::Expression::BoolLiteral(_, _)
        | solidity::Expression::NumberLiteral(_, _, _, _)
        | solidity::Expression::RationalNumberLiteral(_, _, _, _, _)
        | solidity::Expression::HexNumberLiteral(_, _, _)
        | solidity::Expression::AddressLiteral(_, _)
        | solidity::Expression::HexLiteral(_)
        | solidity::Expression::StringLiteral(_) => literal::translate_literal_expression(project, expression),
        
        solidity::Expression::Type(_, _) => expression_type::translate_type_expression(project, translated_definition, scope.clone(), expression),
        solidity::Expression::Variable(_) => variable::translate_variable_expression(project, translated_definition, scope.clone(), expression),
        
        solidity::Expression::ArrayLiteral(_, expressions) => array::translate_array_literal_expression(project, translated_definition, scope.clone(), expressions.as_slice()),
        solidity::Expression::ArraySubscript(_, _, _) => array::translate_array_subscript_expression(project, translated_definition, scope.clone(), expression),
        solidity::Expression::ArraySlice(_, _, _, _) => array::translate_array_slice_expression(project, translated_definition, scope.clone(), expression),
        solidity::Expression::List(_, parameters) => list::translate_list_expression(project, translated_definition, scope.clone(), parameters.as_slice()),
        solidity::Expression::Parenthesis(_, expression) => parenthesis::translate_parenthesis_expression(project, translated_definition, scope.clone(), expression),
        
        solidity::Expression::MemberAccess(_, container, member) => member_access::translate_member_access_expression(project, translated_definition, scope.clone(), expression, container, member),
        
        solidity::Expression::FunctionCall(_, function, arguments) => {
            let result = function_call::translate_function_call_expression(project, translated_definition, scope.clone(), expression, function, None, arguments)?;
            // println!("Translated function call from {} to {}", expression, sway::TabbedDisplayer(&result));
            Ok(result)
        }

        solidity::Expression::FunctionCallBlock(_, function, block) => function_call::translate_function_call_block_expression(project, translated_definition, scope.clone(), function, block),
        
        solidity::Expression::NamedFunctionCall(_, function, named_arguments) => {
            let result = function_call::translate_function_call_expression(project, translated_definition, scope.clone(), expression, function, Some(named_arguments), &[])?;
            // println!("Translated named function call from {} to {}", expression, sway::TabbedDisplayer(&result));
            Ok(result)
        }
        
        solidity::Expression::Not(_, x) => binary_unary::translate_unary_expression(project, translated_definition, scope.clone(), "!", x),
        solidity::Expression::BitwiseNot(_, x) => binary_unary::translate_unary_expression(project, translated_definition, scope.clone(), "!", x),
        solidity::Expression::UnaryPlus(_, x) => translate_expression(project, translated_definition, scope.clone(), x),
        solidity::Expression::Negate(_, x) => binary_unary::translate_unary_expression(project, translated_definition, scope.clone(), "-", x),
        
        solidity::Expression::Power(_, lhs, rhs) => binary_unary::translate_power_expression(project, translated_definition, scope.clone(), lhs, rhs),
        solidity::Expression::Multiply(_, lhs, rhs) => binary_unary::translate_binary_expression(project, translated_definition, scope.clone(), "*", lhs, rhs),
        solidity::Expression::Divide(_, lhs, rhs) => binary_unary::translate_binary_expression(project, translated_definition, scope.clone(), "/", lhs, rhs),
        solidity::Expression::Modulo(_, lhs, rhs) => binary_unary::translate_binary_expression(project, translated_definition, scope.clone(), "%", lhs, rhs),
        solidity::Expression::Add(_, lhs, rhs) => binary_unary::translate_binary_expression(project, translated_definition, scope.clone(), "+", lhs, rhs),
        solidity::Expression::Subtract(_, lhs, rhs) => binary_unary::translate_binary_expression(project, translated_definition, scope.clone(), "-", lhs, rhs),
        solidity::Expression::ShiftLeft(_, lhs, rhs) => binary_unary::translate_binary_expression(project, translated_definition, scope.clone(), "<<", lhs, rhs),
        solidity::Expression::ShiftRight(_, lhs, rhs) => binary_unary::translate_binary_expression(project, translated_definition, scope.clone(), ">>", lhs, rhs),
        solidity::Expression::BitwiseAnd(_, lhs, rhs) => binary_unary::translate_binary_expression(project, translated_definition, scope.clone(), "&", lhs, rhs),
        solidity::Expression::BitwiseXor(_, lhs, rhs) => binary_unary::translate_binary_expression(project, translated_definition, scope.clone(), "^", lhs, rhs),
        solidity::Expression::BitwiseOr(_, lhs, rhs) => binary_unary::translate_binary_expression(project, translated_definition, scope.clone(), "|", lhs, rhs),
        solidity::Expression::Less(_, lhs, rhs) => binary_unary::translate_binary_expression(project, translated_definition, scope.clone(), "<", lhs, rhs),
        solidity::Expression::More(_, lhs, rhs) => binary_unary::translate_binary_expression(project, translated_definition, scope.clone(), ">", lhs, rhs),
        solidity::Expression::LessEqual(_, lhs, rhs) => binary_unary::translate_binary_expression(project, translated_definition, scope.clone(), "<=", lhs, rhs),
        solidity::Expression::MoreEqual(_, lhs, rhs) => binary_unary::translate_binary_expression(project, translated_definition, scope.clone(), ">=", lhs, rhs),
        solidity::Expression::Equal(_, lhs, rhs) => binary_unary::translate_binary_expression(project, translated_definition, scope.clone(), "==", lhs, rhs),
        solidity::Expression::NotEqual(_, lhs, rhs) => binary_unary::translate_binary_expression(project, translated_definition, scope.clone(), "!=", lhs, rhs),
        solidity::Expression::And(_, lhs, rhs) => binary_unary::translate_binary_expression(project, translated_definition, scope.clone(), "&&", lhs, rhs),
        solidity::Expression::Or(_, lhs, rhs) => binary_unary::translate_binary_expression(project, translated_definition, scope.clone(), "||", lhs, rhs),
        
        solidity::Expression::ConditionalOperator(_, condition, then_value, else_value) => conditional::translate_conditional_operator_expression(project, translated_definition, scope.clone(), condition, then_value, else_value),
        
        solidity::Expression::Assign(_, lhs, rhs) => assignment::translate_assignment_expression(project, translated_definition, scope.clone(), "=", lhs.as_ref(), rhs.as_ref()),
        solidity::Expression::AssignOr(_, lhs, rhs) => assignment::translate_assignment_expression(project, translated_definition, scope.clone(), "|=", lhs.as_ref(), rhs.as_ref()),
        solidity::Expression::AssignAnd(_, lhs, rhs) => assignment::translate_assignment_expression(project, translated_definition, scope.clone(), "&=", lhs.as_ref(), rhs.as_ref()),
        solidity::Expression::AssignXor(_, lhs, rhs) => assignment::translate_assignment_expression(project, translated_definition, scope.clone(), "^=", lhs.as_ref(), rhs.as_ref()),
        solidity::Expression::AssignShiftLeft(_, lhs, rhs) => assignment::translate_assignment_expression(project, translated_definition, scope.clone(), "<<=", lhs.as_ref(), rhs.as_ref()),
        solidity::Expression::AssignShiftRight(_, lhs, rhs) => assignment::translate_assignment_expression(project, translated_definition, scope.clone(), ">>=", lhs.as_ref(), rhs.as_ref()),
        solidity::Expression::AssignAdd(_, lhs, rhs) => assignment::translate_assignment_expression(project, translated_definition, scope.clone(), "+=", lhs.as_ref(), rhs.as_ref()),
        solidity::Expression::AssignSubtract(_, lhs, rhs) => assignment::translate_assignment_expression(project, translated_definition, scope.clone(), "-=", lhs.as_ref(), rhs.as_ref()),
        solidity::Expression::AssignMultiply(_, lhs, rhs) => assignment::translate_assignment_expression(project, translated_definition, scope.clone(), "*=", lhs.as_ref(), rhs.as_ref()),
        solidity::Expression::AssignDivide(_, lhs, rhs) => assignment::translate_assignment_expression(project, translated_definition, scope.clone(), "/=", lhs.as_ref(), rhs.as_ref()),
        solidity::Expression::AssignModulo(_, lhs, rhs) => assignment::translate_assignment_expression(project, translated_definition, scope.clone(), "%=", lhs.as_ref(), rhs.as_ref()),
        
        solidity::Expression::PreIncrement(_, _)
        | solidity::Expression::PostIncrement(_, _)
        | solidity::Expression::PreDecrement(_, _)
        | solidity::Expression::PostDecrement(_, _) => pre_post::translate_pre_or_post_operator_value_expression(project, translated_definition, scope.clone(), expression),

        solidity::Expression::New(_, expression) => new::translate_new_expression(project, translated_definition, scope.clone(), expression),
        solidity::Expression::Delete(_, expression) => delete::translate_delete_expression(project, translated_definition, scope.clone(), expression),
    }
}

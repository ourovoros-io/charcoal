use super::{translate_type_name, TranslatedDefinition, TranslatedVariable, TranslationScope};
use crate::{project::Project, sway, translate::resolve_import, Error};
use convert_case::Case;
use num_bigint::BigUint;
use num_traits::{Num, One, Zero};
use solang_parser::{helpers::CodeLocation, pt as solidity};
use std::{cell::RefCell, rc::Rc};

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
                        parameters: function_call.parameters.iter().map(|p| evaluate_expression(translated_definition, scope.clone(), type_name, p)).collect() 
                    })
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
                        parameters: function_call.parameters.iter().map(|p| evaluate_expression(translated_definition, scope.clone(), type_name, p)).collect() 
                    })
                }



                _ => todo!("evaluate function call: {expression:#?}"),
            }

            sway::Expression::MemberAccess(member_access) => match &member_access.expression {
                sway::Expression::Literal(
                    sway::Literal::DecInt(lhs_value, lhs_suffix) |
                    sway::Literal::HexInt(lhs_value, lhs_suffix)
                ) => match member_access.member.as_str() {
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

                        _ => panic!("Invalid {name} value expression: {value:#?} {value_type_name:#?}"),
                    }
                }
            }

            ("I8" | "I16" | "I32" | "I64" | "I128" | "I256", None) => {
                let value = match value.as_ref() {
                    Some(value) => *value,
                    None => &sway::Expression::from(sway::Literal::DecInt(
                        BigUint::zero(),
                        Some(format!("u{}", name.trim_start_matches("I").to_string())),
                    )),
                };

                match value {
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
                            let value_type_name = translated_definition.get_expression_type(scope.clone(), value).unwrap();
        
                            if !value_type_name.is_int() {
                                panic!("Invalid {name} value expression: {value:#?} ({value_type_name})")
                            }
        
                            sway::Expression::from(sway::FunctionCall { 
                                function: sway::Expression::Identifier(format!("{name}::from_uint").into()), 
                                generic_parameters: None, 
                                parameters: vec![value.clone()], 
                            })
                        }
                    }
    
                    x if matches!(x, sway::Expression::BinaryExpression(_)) => (*x).clone(),
    
                    sway::Expression::Identifier(name) => {
                        let Some(variable) = scope.borrow().get_variable_from_new_name(name) else {
                            panic!("error: Variable not found in scope: \"{name}\"");
                        };
                
                        if variable.borrow().type_name != *type_name {
                            panic!("Invalid {name} value expression: {value:#?}");
                        }
    
                        sway::Expression::Identifier(name.clone())
                    }
                    
                    _ => {
                        let value_type_name = translated_definition.get_expression_type(scope.clone(), value).unwrap();
    
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

            ("StorageString", Some(_)) => match value {
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
                else if let Some(struct_definition) = translated_definition.structs.iter().find(|s| s.name == *name).cloned() {
                    return sway::Expression::from(sway::Constructor {
                        type_name: sway::TypeName::Identifier {
                            name: name.to_string(),
                            generic_parameters: None,
                        },
                        fields: struct_definition.fields.iter().map(|f| sway::ConstructorField {
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
        | solidity::Expression::StringLiteral(_) => translate_literal_expression(project, expression),
        
        solidity::Expression::Type(_, _) => translate_type_expression(project, translated_definition, scope.clone(), expression),
        solidity::Expression::Variable(_) => translate_variable_expression(project, translated_definition, scope.clone(), expression),
        
        solidity::Expression::ArrayLiteral(_, expressions) => translate_array_literal_expression(project, translated_definition, scope.clone(), expressions.as_slice()),
        solidity::Expression::ArraySubscript(_, _, _) => translate_array_subscript_expression(project, translated_definition, scope.clone(), expression),
        solidity::Expression::ArraySlice(_, _, _, _) => translate_array_slice_expression(project, translated_definition, scope.clone(), expression),
        solidity::Expression::List(_, parameters) => translate_list_expression(project, translated_definition, scope.clone(), parameters.as_slice()),
        solidity::Expression::Parenthesis(_, expression) => translate_parenthesis_expression(project, translated_definition, scope.clone(), expression),
        
        solidity::Expression::MemberAccess(_, container, member) => translate_member_access_expression(project, translated_definition, scope.clone(), expression, container, member),
        
        solidity::Expression::FunctionCall(_, function, arguments) => {
            let result = translate_function_call_expression(project, translated_definition, scope.clone(), expression, function, None, arguments)?;
            // println!("Translated function call from {} to {}", expression, sway::TabbedDisplayer(&result));
            Ok(result)
        }

        solidity::Expression::FunctionCallBlock(_, function, block) => translate_function_call_block_expression(project, translated_definition, scope.clone(), function, block),
        
        solidity::Expression::NamedFunctionCall(_, function, named_arguments) => {
            let result = translate_function_call_expression(project, translated_definition, scope.clone(), expression, function, Some(named_arguments), &[])?;
            // println!("Translated named function call from {} to {}", expression, sway::TabbedDisplayer(&result));
            Ok(result)
        }
        
        solidity::Expression::Not(_, x) => translate_unary_expression(project, translated_definition, scope.clone(), "!", x),
        solidity::Expression::BitwiseNot(_, x) => translate_unary_expression(project, translated_definition, scope.clone(), "!", x),
        solidity::Expression::UnaryPlus(_, x) => translate_expression(project, translated_definition, scope.clone(), x),
        solidity::Expression::Negate(_, x) => translate_unary_expression(project, translated_definition, scope.clone(), "-", x),
        
        solidity::Expression::Power(_, lhs, rhs) => translate_power_expression(project, translated_definition, scope.clone(), lhs, rhs),
        solidity::Expression::Multiply(_, lhs, rhs) => translate_binary_expression(project, translated_definition, scope.clone(), "*", lhs, rhs),
        solidity::Expression::Divide(_, lhs, rhs) => translate_binary_expression(project, translated_definition, scope.clone(), "/", lhs, rhs),
        solidity::Expression::Modulo(_, lhs, rhs) => translate_binary_expression(project, translated_definition, scope.clone(), "%", lhs, rhs),
        solidity::Expression::Add(_, lhs, rhs) => translate_binary_expression(project, translated_definition, scope.clone(), "+", lhs, rhs),
        solidity::Expression::Subtract(_, lhs, rhs) => translate_binary_expression(project, translated_definition, scope.clone(), "-", lhs, rhs),
        solidity::Expression::ShiftLeft(_, lhs, rhs) => translate_binary_expression(project, translated_definition, scope.clone(), "<<", lhs, rhs),
        solidity::Expression::ShiftRight(_, lhs, rhs) => translate_binary_expression(project, translated_definition, scope.clone(), ">>", lhs, rhs),
        solidity::Expression::BitwiseAnd(_, lhs, rhs) => translate_binary_expression(project, translated_definition, scope.clone(), "&", lhs, rhs),
        solidity::Expression::BitwiseXor(_, lhs, rhs) => translate_binary_expression(project, translated_definition, scope.clone(), "^", lhs, rhs),
        solidity::Expression::BitwiseOr(_, lhs, rhs) => translate_binary_expression(project, translated_definition, scope.clone(), "|", lhs, rhs),
        solidity::Expression::Less(_, lhs, rhs) => translate_binary_expression(project, translated_definition, scope.clone(), "<", lhs, rhs),
        solidity::Expression::More(_, lhs, rhs) => translate_binary_expression(project, translated_definition, scope.clone(), ">", lhs, rhs),
        solidity::Expression::LessEqual(_, lhs, rhs) => translate_binary_expression(project, translated_definition, scope.clone(), "<=", lhs, rhs),
        solidity::Expression::MoreEqual(_, lhs, rhs) => translate_binary_expression(project, translated_definition, scope.clone(), ">=", lhs, rhs),
        solidity::Expression::Equal(_, lhs, rhs) => translate_binary_expression(project, translated_definition, scope.clone(), "==", lhs, rhs),
        solidity::Expression::NotEqual(_, lhs, rhs) => translate_binary_expression(project, translated_definition, scope.clone(), "!=", lhs, rhs),
        solidity::Expression::And(_, lhs, rhs) => translate_binary_expression(project, translated_definition, scope.clone(), "&&", lhs, rhs),
        solidity::Expression::Or(_, lhs, rhs) => translate_binary_expression(project, translated_definition, scope.clone(), "||", lhs, rhs),
        
        solidity::Expression::ConditionalOperator(_, condition, then_value, else_value) => translate_conditional_operator_expression(project, translated_definition, scope.clone(), condition, then_value, else_value),
        
        solidity::Expression::Assign(_, lhs, rhs) => translate_assignment_expression(project, translated_definition, scope.clone(), "=", lhs.as_ref(), rhs.as_ref()),
        solidity::Expression::AssignOr(_, lhs, rhs) => translate_assignment_expression(project, translated_definition, scope.clone(), "|=", lhs.as_ref(), rhs.as_ref()),
        solidity::Expression::AssignAnd(_, lhs, rhs) => translate_assignment_expression(project, translated_definition, scope.clone(), "&=", lhs.as_ref(), rhs.as_ref()),
        solidity::Expression::AssignXor(_, lhs, rhs) => translate_assignment_expression(project, translated_definition, scope.clone(), "^=", lhs.as_ref(), rhs.as_ref()),
        solidity::Expression::AssignShiftLeft(_, lhs, rhs) => translate_assignment_expression(project, translated_definition, scope.clone(), "<<=", lhs.as_ref(), rhs.as_ref()),
        solidity::Expression::AssignShiftRight(_, lhs, rhs) => translate_assignment_expression(project, translated_definition, scope.clone(), ">>=", lhs.as_ref(), rhs.as_ref()),
        solidity::Expression::AssignAdd(_, lhs, rhs) => translate_assignment_expression(project, translated_definition, scope.clone(), "+=", lhs.as_ref(), rhs.as_ref()),
        solidity::Expression::AssignSubtract(_, lhs, rhs) => translate_assignment_expression(project, translated_definition, scope.clone(), "-=", lhs.as_ref(), rhs.as_ref()),
        solidity::Expression::AssignMultiply(_, lhs, rhs) => translate_assignment_expression(project, translated_definition, scope.clone(), "*=", lhs.as_ref(), rhs.as_ref()),
        solidity::Expression::AssignDivide(_, lhs, rhs) => translate_assignment_expression(project, translated_definition, scope.clone(), "/=", lhs.as_ref(), rhs.as_ref()),
        solidity::Expression::AssignModulo(_, lhs, rhs) => translate_assignment_expression(project, translated_definition, scope.clone(), "%=", lhs.as_ref(), rhs.as_ref()),
        
        solidity::Expression::PreIncrement(_, _)
        | solidity::Expression::PostIncrement(_, _)
        | solidity::Expression::PreDecrement(_, _)
        | solidity::Expression::PostDecrement(_, _) => translate_pre_or_post_operator_value_expression(project, translated_definition, scope.clone(), expression),

        solidity::Expression::New(_, expression) => translate_new_expression(project, translated_definition, scope.clone(), expression),
        solidity::Expression::Delete(_, expression) => translate_delete_expression(project, translated_definition, scope.clone(), expression),
    }
}

#[inline]
pub fn translate_literal_expression(
    _project: &mut Project,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    match expression {
        solidity::Expression::BoolLiteral(_, value) => {
            Ok(sway::Expression::from(sway::Literal::Bool(*value)))
        }
        
        solidity::Expression::NumberLiteral(_, value, _, _) => {
            Ok(sway::Expression::from(sway::Literal::DecInt(value.parse().unwrap(), None)))
        }

        solidity::Expression::RationalNumberLiteral(_, _, _, _, _) => {
            Ok(sway::Expression::create_todo(Some(format!("rational number: {}", expression))))
        }

        solidity::Expression::HexNumberLiteral(_, value, _) | solidity::Expression::AddressLiteral(_, value) => {
            Ok(sway::Expression::from(sway::Literal::HexInt(
                BigUint::from_str_radix(value.trim_start_matches("0x"), 16)
                    .map_err(|e| Error::Wrapped(Box::new(e)))?,
                None,
            )))
        }

        solidity::Expression::HexLiteral(hex_literals) => {
            Ok(sway::Expression::from(sway::Literal::HexInt(
                BigUint::from_str_radix(
                    hex_literals.iter()
                        .map(|x| x.hex.clone())
                        .collect::<Vec<_>>()
                        .join("")
                        .as_str(),
                    16,
                )
                .map_err(|e| Error::Wrapped(Box::new(e)))?,
                None,
            )))
        }
        
        solidity::Expression::StringLiteral(value) => {
            Ok(sway::Expression::from(sway::Literal::String(
                value.iter().map(|s| s.string.clone()).collect::<Vec<_>>().join("")
            )))
        }

        _ => panic!("Expected literal expression, found {} - {expression:#?}", expression),
    }
}

#[inline]
pub fn translate_type_expression(
    _project: &mut Project,
    _translated_definition: &mut TranslatedDefinition,
    _scope: Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    //
    // NOTE:
    // Type expressions should never be encountered on their own.
    // They should be handled in a higher level expression.
    //

    unimplemented!("type expression: {expression} - {expression:#?}")
}

#[inline]
pub fn translate_variable_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    //
    // NOTE:
    // Variable expressions should only ever be encountered for reading the value.
    // Writes are handled when translating assignment expressions.
    //

    let solidity::Expression::Variable(variable) = expression else {
        panic!("this should only be used with variable expressions")
    };

    // Check for built-in variables
    match variable.name.as_str() {
        "now" => {
            // now => std::block::timestamp().as_u256()
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
            }));
        }

        _ => {}
    }

    let (variable, expression) = translate_variable_access_expression(project, translated_definition, scope.clone(), expression)?;
    if variable.is_none() {
        panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
    }
    let variable = variable.unwrap();
    let mut variable = variable.borrow_mut();

    variable.read_count += 1;
    
    if variable.is_storage {
        match &variable.type_name {
            sway::TypeName::Identifier { name, .. } if name == "StorageString" => {
                Ok(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression: sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::from(sway::MemberAccess {
                                expression,
                                member: "read_slice".into(),
                            }),
                            generic_parameters: None,
                            parameters: vec![],
                        }),
                        member: "unwrap".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![],
                }))
            }

            _ => Ok(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::from(sway::MemberAccess {
                    expression,
                    member: "read".into(),
                }),
                generic_parameters: None,
                parameters: vec![],
            }))
        }
    } else {
        Ok(expression)
    }
}

#[inline]
pub fn translate_array_literal_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    expressions: &[solidity::Expression],
) -> Result<sway::Expression, Error> {
    Ok(sway::Expression::Array(sway::Array {
        elements: expressions.iter()
            .map(|x| translate_expression(project, translated_definition, scope.clone(), x))
            .collect::<Result<Vec<_>, _>>()?,
    }))
}

#[inline]
pub fn translate_array_subscript_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    //
    // NOTE:
    // Array subscript expressions should only ever be encountered for reading the value.
    // Writes are handled when translating assignment expressions.
    //

    let (variable, expression) = translate_variable_access_expression(project, translated_definition, scope.clone(), expression)?;

    if variable.is_none() {
        return Ok(expression);
    }
    
    let variable = variable.unwrap();
    let mut variable = variable.borrow_mut();

    variable.read_count += 1;

    if variable.is_storage {
        match &variable.type_name {
            sway::TypeName::Identifier { name, .. } if name == "StorageVec" => Ok(
                sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression: sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::from(sway::MemberAccess {
                                expression,
                                member: "unwrap".into(),
                            }),
                            generic_parameters: None,
                            parameters: vec![],
                        }),
                        member: "read".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![],
                })
            ),
            
            _ => Ok(sway::Expression::from(sway::FunctionCall {
                function: sway::Expression::from(sway::MemberAccess {
                    expression,
                    member: "read".into(),
                }),
                generic_parameters: None,
                parameters: vec![],
            }))
        }
    } else {
        match &variable.type_name {
            sway::TypeName::Identifier { name, .. } if name == "Vec" => {
                let sway::Expression::ArrayAccess(array_access) = expression else {
                    panic!("Expected array access expression, found {expression:#?}");
                };

                Ok(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression: sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::from(sway::MemberAccess {
                                expression: array_access.expression.clone(),
                                member: "get".into(),
                            }),
                            generic_parameters: None,
                            parameters: vec![
                                array_access.index.clone(),
                            ],
                        }),
                        member: "unwrap".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![],
                }))
            }

            _ => Ok(expression),
        }
    }
}

#[inline]
pub fn translate_array_slice_expression(
    _project: &mut Project,
    _translated_definition: &mut TranslatedDefinition,
    _scope: Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    todo!("translate array slice expression: {expression} - {expression:#?}")
}

#[inline]
pub fn translate_list_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    parameters: &[(solidity::Loc, Option<solidity::Parameter>)],
) -> Result<sway::Expression, Error> {
    //
    // NOTE:
    // Assignments are handled at the statement level, since it's an assignment to a list of variable declarations.
    //

    // Ensure all elements of the list have no name (value-only tuple)
    if !parameters.iter().all(|(_, p)| p.as_ref().unwrap().name.is_none()) {
        unimplemented!("non-value list expression")
    }

    // Create a tuple expression
    Ok(sway::Expression::Tuple(
        parameters.iter()
            .map(|(_, p)| translate_expression(project, translated_definition, scope.clone(), &p.as_ref().unwrap().ty))
            .collect::<Result<Vec<_>, _>>()?
    ))
}

#[inline]
pub fn translate_parenthesis_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    let sway_expr = translate_expression(project, translated_definition, scope.clone(), expression)?;
    
    match &expression {
        solidity::Expression::Assign(_, lhs, _) => {
            // function add(uint x, uint y) internal pure returns (uint z) {
            //    require((z = x + y) >= x, "ds-math-add-overflow");
            // }
            //
            // fn add(x: u256, y: u256) -> u256 {
            //    let mut z: u256 = 0;
            //    require({z = x + y; z } >= x, "ds-math-add-overflow");
            //    z
            // }
            //
            let sway_lhs = translate_expression(project, translated_definition, scope.clone(), lhs)?;
            
            Ok(sway::Expression::from(sway::Block {
                statements: vec![
                    sway::Statement::from(sway_expr),
                ],
                final_expr:  Some(sway_lhs),
            }))
        },
        _ => {
            // (x)
            Ok(sway::Expression::Tuple(vec![
                sway_expr,
            ]))
        }
    }
}

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
                // }

                return Ok(sway::Expression::from(sway::AsmBlock {
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
                if let Some(external_definition) = resolve_import(project, &name.to_string(), &translated_definition.path)? {
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
                if let Some(external_definition) = project.find_definition_with_abi(name) {
                    if member.name == "selector" {
                        let external_scope = external_definition.toplevel_scope.borrow();

                        if external_scope.find_function(|f| f.borrow().old_name == member1.name).is_some() {
                            return Ok(sway::Expression::create_todo(Some(expression.to_string())));
                        }
                    }
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
                            // Import the enum if we haven't already
                            if !translated_definition.enums.contains(external_enum) {
                                translated_definition.import_enum(external_enum);
                            }
    
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
    
    let container_type_name = translated_definition.get_expression_type(scope.clone(), &container)?;
    let container_type_name_string = container_type_name.to_string();

    // Check if container is a struct
    if let Some(struct_definition) = translated_definition.structs.iter().find(|s| s.name == container_type_name_string) {
        let field_name = crate::translate_naming_convention(member.name.as_str(), Case::Snake);

        if struct_definition.fields.iter().any(|f| f.name == field_name) {
            return Ok(sway::Expression::from(sway::MemberAccess {
                expression: container,
                member: field_name,
            }));
        }
    }

    // Check for fields of built-in solidity value types
    match container_type_name {
        sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
            ("Bytes", None) => match member.name.as_str() {
                "length" => return Ok(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression: container,
                        member: "len".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![],
                })),

                _ => {}
            }

            ("Identity", None) => match member.name.as_str() {
                "balance" => return Ok(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::Identifier("std::context::balance_of".into()),
                    generic_parameters: None,
                    parameters: vec![
                        sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::from(sway::MemberAccess {
                                expression: sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: container,
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
                })),

                _ => {}
            }

            ("StorageVec", Some(_)) => match member.name.as_str() {
                "length" => return Ok(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression: container,
                        member: "len".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![],
                })),

                _ => {}
            }

            ("Vec", Some(_)) => match member.name.as_str() {
                "length" => return Ok(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression: container,
                        member: "len".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![],
                })),
                _ => {}
            }
            _ => {}
        }
        _ => {}
    }

    println!("Structs : {:#?}", translated_definition.structs);

    todo!("{}translate {container_type_name_string} member access expression: {expression} - {expression:#?}", match project.loc_to_line_and_column(&translated_definition.path, &expression.loc()) {
        Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
        None => format!("{} - ", translated_definition.path.to_string_lossy()),
    },)
}

#[inline]
pub fn translate_function_call_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
    function: &solidity::Expression,
    named_arguments: Option<&[solidity::NamedArgument]>,
    arguments: &[solidity::Expression],
) -> Result<sway::Expression, Error> {
    if named_arguments.is_some() && !arguments.is_empty() {
        panic!("Invalid call to translate_function_call_expression: named_arguments is Some(_) and arguments is not empty");
    }

    // println!(
    //     "Translating function call: {expression}; from {}",
    //     match project.loc_to_line_and_column(&translated_definition.path, &expression.loc()) {
    //         Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
    //         None => format!("{} - ", translated_definition.path.to_string_lossy()),
    //     },
    // );

    match function {
        solidity::Expression::Type(_, ty) => {
            // Type casting

            if arguments.len() != 1 {
                panic!("Invalid type cast: {expression:#?}");
            }

            match ty {
                solidity::Type::Address => match &arguments[0] {
                    solidity::Expression::HexNumberLiteral(_, _, _) |
                    solidity::Expression::NumberLiteral(_, _, _, _) => {
                        Ok(sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier("Identity::Address".into()),
                            generic_parameters: None,
                            parameters: vec![
                                sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier("Address::from".into()),
                                    generic_parameters: None,
                                    parameters: vec![
                                        sway::Expression::from(match &arguments[0] {
                                            solidity::Expression::HexNumberLiteral(_, value, _) => {
                                                let value = BigUint::from_str_radix(value.as_str().trim_start_matches("0x"), 16).unwrap();

                                                if value.is_zero() {
                                                    // Ensure `std::constants::ZERO_B256` is imported
                                                    translated_definition.ensure_use_declared("std::constants::ZERO_B256");
                                
                                                    sway::Expression::Identifier("ZERO_B256".into())
                                                } else {
                                                    sway::Expression::from(
                                                        sway::Literal::HexInt(value, Some("b256".into()))
                                                    )
                                                }
                                            }

                                            solidity::Expression::NumberLiteral(_, value, _, _) => {
                                                let value: BigUint = value.parse().unwrap();

                                                if value.is_zero() {
                                                    // Ensure `std::constants::ZERO_B256` is imported
                                                    translated_definition.ensure_use_declared("std::constants::ZERO_B256");
                                
                                                    sway::Expression::Identifier("ZERO_B256".into())
                                                } else {
                                                    sway::Expression::from(
                                                        sway::Literal::DecInt(value, Some("b256".into()))
                                                    )
                                                }
                                            }

                                            _ => unreachable!(),
                                        }),
                                    ],
                                }),
                            ],
                        }))
                    }

                    solidity::Expression::Variable(solidity::Identifier { name, .. }) if name == "this" => {
                        // address(this) => Identity::from(ContractId::this())
                        Ok(sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier("Identity::ContractId".into()),
                            generic_parameters: None,
                            parameters: vec![
                                sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier("ContractId::this".into()),
                                    generic_parameters: None,
                                    parameters: vec![],
                                }),
                            ],
                        }))
                    }

                    value => {
                        let value = translate_expression(project, translated_definition, scope.clone(), value)?;
                        let value_type_name = translated_definition.get_expression_type(scope.clone(), &value)?;

                        match value_type_name {
                            // No reason to cast if it's already an Identity
                            sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                                ("Identity", None) => Ok(value),
                                ("u256", None) => Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier("Identity::Address".into()),
                                    generic_parameters: None,
                                    parameters: vec![
                                        sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::Identifier("Address::from".into()),
                                            generic_parameters: None,
                                            parameters: vec![value],
                                        }),
                                    ],
                                })),
                                _ => panic!("translate address cast: {expression} - {value:#?}"),
                            }
                            _ => {
                                panic!(
                                    "{}translate address cast: {expression} - {value_type_name:#?}",
                                    match project.loc_to_line_and_column(&translated_definition.path, &arguments[0].loc()) {
                                        Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                                        None => format!("{} - ", translated_definition.path.to_string_lossy()),
                                    },
                                );  
                                // todo!("translate address cast: {expression}")
                            },
                        }
                    }
                }

                solidity::Type::Payable => {
                    // payable(x) => x

                    let parameters = arguments.iter()
                        .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                        .collect::<Result<Vec<_>, _>>()?;
                    
                    if parameters.len() != 1 {
                        panic!("Malformed payable cast: {} - {expression:#?}", expression);
                    }

                    Ok(parameters[0].clone())
                }

                solidity::Type::Int(bits) => {
                    let value_expression = translate_expression(project, translated_definition, scope.clone(), &arguments[0])?;
                    let value_type_name = translated_definition.get_expression_type(scope.clone(), &value_expression)?;
                    let value_type_name = translated_definition.get_underlying_type(&value_type_name);

                    let create_int_try_from_unwrap_expression = |from_bits: usize, to_bits: usize, value: sway::Expression| -> Result<sway::Expression, Error> {
                        if from_bits == to_bits {
                            return Ok(value);
                        }

                        Ok(sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier(format!("I{to_bits}::from")),
                            generic_parameters: None,
                            parameters: vec![
                                sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::Identifier(format!("{}{to_bits}::try_from", if to_bits > 64 { "U" } else { "u" })),
                                            generic_parameters: None,
                                            parameters: vec![value],
                                        }),
                                        member: "unwrap".into(),
                                    }),
                                    generic_parameters: None,
                                    parameters: vec![],
                                }),
                            ],
                        }))
                    };

                    let bits = match bits {
                        0..=8 => {
                            if *bits != 8 {
                                eprintln!("WARNING: unsupported signed integer type `int{bits}`, using `i8`...");
                            }
                            8
                        }
                        9..=16 => {
                            if *bits != 16 {
                                eprintln!("WARNING: unsupported signed integer type `int{bits}`, using `i16`...");
                            }
                            16
                        }
                        17..=32 => {
                            if *bits != 32 {
                                eprintln!("WARNING: unsupported signed integer type `int{bits}`, using `i32`...");
                            }
                            32
                        }
                        33..=64 => {
                            if *bits != 64 {
                                eprintln!("WARNING: unsupported signed integer type `int{bits}`, using `i64`...");
                            }
                            64
                        }
                        65..=128 => {
                            if *bits != 128 {
                                eprintln!("WARNING: unsupported signed integer type `int{bits}`, using `i128`...");
                            }
                            128
                        }
                        129..=256 => {
                            if *bits != 256 {
                                eprintln!("WARNING: unsupported signed integer type `int{bits}`, using `i256`...");
                            }
                            256
                        }
                        _ => panic!("Invalid int type: {expression:#?}"),
                    };

                    match &value_type_name {
                        sway::TypeName::Identifier { name, generic_parameters: None } => match (name.as_str(), bits) {
                            ("I8", 8 | 16 | 32 | 64 | 128 | 256) => create_int_try_from_unwrap_expression(8, bits as usize, value_expression),
                            ("I16", 8 | 16 | 32 | 64 | 128 | 256) => create_int_try_from_unwrap_expression(16, bits as usize, value_expression),
                            ("I32", 8 | 16 | 32 | 64 | 128 | 256) => create_int_try_from_unwrap_expression(32, bits as usize, value_expression),
                            ("I64", 8 | 16 | 32 | 64 | 128 | 256) => create_int_try_from_unwrap_expression(64, bits as usize, value_expression),
                            ("I128", 8 | 16 | 32 | 64 | 128 | 256) => create_int_try_from_unwrap_expression(128, bits as usize, value_expression),
                            ("I256", 8 | 16 | 32 | 64 | 128 | 256) => create_int_try_from_unwrap_expression(256, bits as usize, value_expression),
                            
                            ("u64", 256) => {
                                Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier("I256::from_uint".into()),
                                    generic_parameters: None,
                                    parameters: vec![
                                        sway::Expression::from(sway::FunctionCall { 
                                            function: sway::Expression::from(sway::MemberAccess { 
                                                expression: value_expression, 
                                                member: "as_u256".into() 
                                            }), 
                                            generic_parameters: None, 
                                            parameters: vec![], 
                                        })
                                    ],
                                }))
                            }

                            ("u256", 256) => {
                                Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier("I256::from_uint".into()),
                                    generic_parameters: None,
                                    parameters: vec![
                                        value_expression,
                                    ],
                                }))
                            }

                            _ => todo!("translate type cast from {value_type_name} to I{bits}: {expression} - {expression:#?}"),
                        }
                        
                        _ => todo!("translate type cast from {value_type_name} to I{bits}: {expression} - {expression:#?}"),
                    }
                }

                solidity::Type::Uint(bits) => {
                    let value_expression = translate_expression(project, translated_definition, scope.clone(), &arguments[0])?;
                    let value_type_name = translated_definition.get_expression_type(scope.clone(), &value_expression)?;
                    let value_type_name = translated_definition.get_underlying_type(&value_type_name);

                    if value_type_name.is_int() {
                        match &arguments[0] {
                            solidity::Expression::Negate(_, expr) => {
                                match expr.as_ref() {
                                    solidity::Expression::NumberLiteral(_, value, _, _) => {
                                        let value = value.parse::<BigUint>().map_err(|e| Error::Wrapped(Box::new(e)))?;
                                        let max = if *bits == 256 {
                                            BigUint::from_str_radix("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 16).map_err(|e| Error::Wrapped(Box::new(e)))?
                                        } else {
                                            (BigUint::one() << *bits) - BigUint::one()
                                        };

                                        return Ok(sway::Expression::from(sway::Literal::HexInt((max - value) + BigUint::one(), Some("u256".to_string()))));
                                    }
                                    _ => {}
                                }
                            }
                            _ => {}
                        }
                    }

                    let create_uint_try_from_unwrap_expression = |from_bits: usize, to_bits: usize, value: sway::Expression| -> Result<sway::Expression, Error> {
                        if from_bits == to_bits {
                            return Ok(value);
                        }

                        Ok(sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::from(sway::MemberAccess {
                                expression: sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier(format!("{}{to_bits}::try_from", if to_bits > 64 { "U" } else { "u" })),
                                    generic_parameters: None,
                                    parameters: vec![value],
                                }),
                                member: "unwrap".into(),
                            }),
                            generic_parameters: None,
                            parameters: vec![],
                        }))
                    };

                    let bits = match bits {
                        0..=8 => {
                            if *bits != 8 {
                                eprintln!("WARNING: unsupported unsigned integer type `uint{bits}`, using `u8`...");
                            }
                            8
                        }
                        9..=16 => {
                            if *bits != 16 {
                                eprintln!("WARNING: unsupported unsigned integer type `uint{bits}`, using `u16`...");
                            }
                            16
                        }
                        17..=32 => {
                            if *bits != 32 {
                                eprintln!("WARNING: unsupported unsigned integer type `uint{bits}`, using `u32`...");
                            }
                            32
                        }
                        33..=64 => {
                            if *bits != 64 {
                                eprintln!("WARNING: unsupported unsigned integer type `uint{bits}`, using `u64`...");
                            }
                            64
                        }
                        65..=256 => {
                            if *bits != 256 {
                                eprintln!("WARNING: unsupported unsigned integer type `uint{bits}`, using `u256`...");
                            }
                            translated_definition.ensure_use_declared("std::u256::*");
                            256
                        }
                        _ => panic!("Invalid uint type: {expression:#?}"),
                    };

                    match &value_type_name {
                        sway::TypeName::Identifier { name, .. } => match (name.as_str(), bits) {
                            ("u8", 8 | 16 | 32 | 64 | 256) => create_uint_try_from_unwrap_expression(8, bits as usize, value_expression),
                            ("u16", 8 | 16 | 32 | 64 | 256) => create_uint_try_from_unwrap_expression(16, bits as usize, value_expression),
                            ("u32", 8 | 16 | 32 | 64 | 256) => create_uint_try_from_unwrap_expression(32, bits as usize, value_expression),
                            ("u64", 8 | 16 | 32 | 64 | 256) => create_uint_try_from_unwrap_expression(64, bits as usize, value_expression),
                            ("u256", 8 | 16 | 32 | 64 | 256) => create_uint_try_from_unwrap_expression(256, bits as usize, value_expression),
                            
                            // Direct signed-to-unsigned conversion
                            ("I8", 8) | ("I16", 16) | ("I32", 32) | ("I64", 64) | ("I128", 128) | ("I256", 256) => {
                                Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: value_expression,
                                        member: "underlying".into(),
                                    }),
                                    generic_parameters: None,
                                    parameters: vec![],
                                }))
                            }

                            // Indirect signed-to-unsigned conversion
                            // NOTE: this isn't converting between bits correctly
                            //       we'll have to fix this eventually...
                            ("I8", _) | ("I16", _) | ("I32", _) | ("I64", _) | ("I128", _) | ("I256", _) => {
                                Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: value_expression,
                                        member: "underlying".into(),
                                    }),
                                    generic_parameters: None,
                                    parameters: vec![],
                                }))
                            }

                            ("b256", 256) => {
                                Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: value_expression,
                                        member: "as_u256".into(),
                                    }),
                                    generic_parameters: None,
                                    parameters: vec![],
                                }))
                            }

                            ("Identity", 256) => {
                                // if x.is_address() {
                                //     b256::from(x.as_address().unwrap()).as_u256()
                                // } else {
                                //     b256::from(x.as_contract_id().unwrap()).as_u256()
                                // }

                                Ok(sway::Expression::from(sway::If {
                                    condition: Some(sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::from(sway::MemberAccess {
                                            expression: value_expression.clone(),
                                            member: "is_address".into(),
                                        }),
                                        generic_parameters: None,
                                        parameters: vec![],
                                    })),
                                    then_body: sway::Block {
                                        statements: vec![],
                                        final_expr: Some(sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::from(sway::MemberAccess {
                                                expression: sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::Identifier("b256::from".into()),
                                                    generic_parameters: None,
                                                    parameters: vec![
                                                        sway::Expression::from(sway::FunctionCall {
                                                            function: sway::Expression::from(sway::MemberAccess {
                                                                expression: sway::Expression::from(sway::FunctionCall {
                                                                    function: sway::Expression::from(sway::MemberAccess {
                                                                        expression: value_expression.clone(),
                                                                        member: "as_address".into(),
                                                                    }),
                                                                    generic_parameters: None,
                                                                    parameters: vec![],
                                                                }),
                                                                member: "unwrap".into(),
                                                            }),
                                                            generic_parameters: None,
                                                            parameters: vec![],
                                                        }),
                                                    ],
                                                }),
                                                member: "as_u256".into(),
                                            }),
                                            generic_parameters: None,
                                            parameters: vec![],
                                        })),
                                    },
                                    else_if: Some(Box::new(sway::If {
                                        condition: None,
                                        then_body: sway::Block {
                                            statements: vec![],
                                            final_expr: Some(sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::from(sway::MemberAccess {
                                                    expression: sway::Expression::from(sway::FunctionCall {
                                                        function: sway::Expression::Identifier("b256::from".into()),
                                                        generic_parameters: None,
                                                        parameters: vec![
                                                            sway::Expression::from(sway::FunctionCall {
                                                                function: sway::Expression::from(sway::MemberAccess {
                                                                    expression: sway::Expression::from(sway::FunctionCall {
                                                                        function: sway::Expression::from(sway::MemberAccess {
                                                                            expression: value_expression.clone(),
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
                                                        ],
                                                    }),
                                                    member: "as_u256".into(),
                                                }),
                                                generic_parameters: None,
                                                parameters: vec![],
                                            })),
                                        },
                                        else_if: None,
                                    })),
                                }))
                            }

                            ("todo!", _) => Ok(value_expression),

                            _ => panic!(
                                "{}translate from {value_type_name} to u{bits}: {value_expression:#?}",
                                match project.loc_to_line_and_column(&translated_definition.path, &arguments[0].loc()) {
                                    Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                                    None => format!("{} - ", translated_definition.path.to_string_lossy()),
                                },
                            ),
                        }

                        _ => todo!("translate {value_type_name} type cast: {} - {expression:#?}", expression),
                    }
                }

                solidity::Type::Bytes(byte_count) => {
                    // bytesN(x) => ???

                    let value_expression = translate_expression(project, translated_definition, scope.clone(), &arguments[0])?;
                    let value_type_name = translated_definition.get_expression_type(scope.clone(), &value_expression)?;

                    match &value_type_name {
                        sway::TypeName::Undefined => panic!("Undefined type name"),

                        sway::TypeName::Identifier { name, generic_parameters } => match name.as_str() {
                            "b256" if generic_parameters.is_none() => {
                                // Ensure `std::bytes::Bytes` is imported
                                translated_definition.ensure_use_declared("std::bytes::Bytes");
        
                                // Generate a unique name for our variable
                                let variable_name = scope.borrow_mut().generate_unique_variable_name("bytes");
        
                                Ok(sway::Expression::from(sway::Block {
                                    statements: vec![
                                        sway::Statement::from(sway::Let {
                                            pattern: sway::LetPattern::from(sway::LetIdentifier {
                                                is_mutable: false,
                                                name: variable_name.clone(),
                                            }),
                                            type_name: None,
                                            value: sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::Identifier("Bytes::from".into()),
                                                generic_parameters: None,
                                                parameters: vec![
                                                    value_expression.clone(),
                                                ],
                                            }),
                                        }),
                                        sway::Statement::from(sway::Let {
                                            pattern: sway::LetPattern::from(vec![
                                                sway::LetIdentifier {
                                                    is_mutable: false,
                                                    name: variable_name.clone(),
                                                },
                                                sway::LetIdentifier {
                                                    is_mutable: false,
                                                    name: "_".into(),
                                                },
                                            ]),
                                            type_name: None,
                                            value: sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::from(sway::MemberAccess {
                                                    expression: sway::Expression::Identifier(variable_name.clone()),
                                                    member: "split_at".into(),
                                                }),
                                                generic_parameters: None,
                                                parameters: vec![
                                                    sway::Expression::from(sway::Literal::DecInt(BigUint::from(*byte_count), None)),
                                                ],
                                            }),
                                        }),
                                    ],
                                    final_expr: Some(sway::Expression::Identifier(variable_name)),
                                }))
                            }

                            _ => Ok(sway::Expression::create_todo(Some(format!("translate from {value_type_name} to bytes{byte_count}: {expression}")))),
                        }

                        sway::TypeName::Array { .. } => todo!("translate from {value_type_name} to bytes{byte_count}"),
                        sway::TypeName::Tuple { .. } => todo!("translate from {value_type_name} to bytes{byte_count}"),

                        sway::TypeName::StringSlice => {
                            match &value_expression {
                                sway::Expression::Literal(sway::Literal::String(s)) => Ok(
                                    sway::Expression::from(sway::Literal::HexInt(
                                        BigUint::from_str_radix(s.trim_start_matches("0x"), 16)
                                            .map_err(|e| Error::Wrapped(Box::new(e)))?,
                                        None,
                                    ))
                                ),

                                _ => todo!("translate from {value_type_name} to bytes{byte_count}"),
                            }
                        }

                        sway::TypeName::StringArray { .. } => todo!("translate from {value_type_name} to bytes{byte_count}"),
                    }
                }

                solidity::Type::DynamicBytes => {
                    // bytes(x) => ???

                    let value_expression = translate_expression(project, translated_definition, scope.clone(), &arguments[0])?;
                    let value_type_name = translated_definition.get_expression_type(scope.clone(), &value_expression)?;

                    match &value_type_name {
                        sway::TypeName::Undefined => panic!("Undefined type name"),
                        sway::TypeName::Identifier { .. } => todo!("translate from {value_type_name} to bytes"),
                        sway::TypeName::Array { .. } => todo!("translate from {value_type_name} to bytes"),
                        sway::TypeName::Tuple { .. } => todo!("translate from {value_type_name} to bytes"),
    
                        sway::TypeName::StringSlice => {
                            // Ensure `std::bytes::Bytes` is imported
                            translated_definition.ensure_use_declared("std::bytes::Bytes");
    
                            // Generate a unique name for our variable
                            let variable_name = scope.borrow_mut().generate_unique_variable_name("s");

                            if let sway::Expression::Identifier(variable_name) = &value_expression {
                                return Ok(sway::Expression::from(sway::FunctionCall {
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
                                                sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::from(sway::MemberAccess {
                                                        expression: sway::Expression::Identifier(variable_name.clone()),
                                                        member: "as_ptr".into(),
                                                    }),
                                                    generic_parameters: None,
                                                    parameters: vec![],
                                                }),
                                                sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::from(sway::MemberAccess {
                                                        expression: sway::Expression::Identifier(variable_name.clone()),
                                                        member: "len".into(),
                                                    }),
                                                    generic_parameters: None,
                                                    parameters: vec![],
                                                }),
                                            ],
                                        }),
                                    ],
                                }));
                            }
    
                            Ok(sway::Expression::from(sway::Block {
                                statements: vec![
                                    sway::Statement::from(sway::Let {
                                        pattern: sway::LetPattern::from(sway::LetIdentifier {
                                            is_mutable: false,
                                            name: variable_name.clone(),
                                        }),
                                        type_name: None,
                                        value: value_expression.clone(),
                                    }),
                                ],
                                final_expr: Some(sway::Expression::from(sway::FunctionCall {
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
                                                sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::from(sway::MemberAccess {
                                                        expression: sway::Expression::Identifier(variable_name.clone()),
                                                        member: "as_ptr".into(),
                                                    }),
                                                    generic_parameters: None,
                                                    parameters: vec![],
                                                }),
                                                sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::from(sway::MemberAccess {
                                                        expression: sway::Expression::Identifier(variable_name.clone()),
                                                        member: "len".into(),
                                                    }),
                                                    generic_parameters: None,
                                                    parameters: vec![],
                                                }),
                                            ],
                                        }),
                                    ],
                                })),
                            }))
                        }

                        sway::TypeName::StringArray { .. } => todo!("translate from {value_type_name} to bytes"),
                    }
                }

                solidity::Type::String => {
                    // string(x) => ???

                    let value_expression = translate_expression(project, translated_definition, scope.clone(), &arguments[0])?;
                    let value_type_name = translated_definition.get_expression_type(scope.clone(), &value_expression)?;

                    match &value_type_name {
                        sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                            ("Bytes", None) => {
                                // Ensure `std::string::*` is imported
                                translated_definition.ensure_use_declared("std::string::*");
                                
                                Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier("String::from_ascii".into()),
                                    generic_parameters: None,
                                    parameters: vec![
                                        value_expression,
                                    ],
                                }))
                            }

                            _ => todo!("translate {value_type_name} type cast: {} - {expression:#?}", expression),
                        }

                        _ => todo!("translate {value_type_name} type cast: {} - {expression:#?}", expression),
                    }
                }

                _ => todo!("translate type cast: {} - {expression:#?}", expression),
            }
        }

        solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
            let mut parameters = arguments.iter()
                .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                .collect::<Result<Vec<_>, _>>()?;

            match name.as_str() {
                "blockhash" => {
                    // blockhash(block_number) => std::block::block_header_hash(block_height).unwrap_or(0)

                    if parameters.len() != 1 {
                        panic!("Invalid blockhash call: {expression:#?}");
                    }

                    Ok(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::from(sway::MemberAccess {
                            expression: sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier("std::block::block_header_hash".into()),
                                generic_parameters: None,
                                parameters,
                            }),
                            member: "unwrap_or".into(),
                        }),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                        ],
                    }))
                }

                "gasleft" => {
                    // gasleft() => std::registers::global_gas()

                    if !parameters.is_empty() {
                        panic!("Invalid gasleft call: {expression:#?}");
                    }

                    Ok(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("std::registers::global_gas".into()),
                        generic_parameters: None,
                        parameters,
                    }))
                }

                "addmod" => {
                    // addmod(x, y, k) => (x + y) % k

                    if parameters.len() != 3 {
                        panic!("Invalid addmod call: {expression:#?}");
                    }

                    Ok(sway::Expression::from(sway::BinaryExpression {
                        operator: "%".into(),
                        lhs: sway::Expression::Tuple(vec![
                            sway::Expression::from(sway::BinaryExpression {
                                operator: "+".into(),
                                lhs: parameters[0].clone(),
                                rhs: parameters[1].clone(),
                            }),
                        ]),
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
                        lhs: sway::Expression::Tuple(vec![
                            sway::Expression::from(sway::BinaryExpression {
                                operator: "*".into(),
                                lhs: parameters[0].clone(),
                                rhs: parameters[1].clone(),
                            }),
                        ]),
                        rhs: parameters[2].clone(),
                    }))
                }

                "keccak256" => {
                    // keccak256(value) => std::hash::keccak256(value)

                    if parameters.len() != 1 {
                        panic!("Invalid keccak256 call: {expression:#?}");
                    }

                    Ok(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("std::hash::keccak256".into()),
                        generic_parameters: None,
                        parameters,
                    }))
                }

                "sha256" => {
                    // sha256(value) => std::hash::sha256(value)

                    if parameters.len() != 1 {
                        panic!("Invalid sha256 call: {expression:#?}");
                    }

                    Ok(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("std::hash::sha256".into()),
                        generic_parameters: None,
                        parameters,
                    }))
                }

                "ripemd160" => {
                    // ripemd160() => /*unsupported: block.basefee; using:*/ 0
                    
                    Ok(sway::Expression::Commented(
                        "unsupported: ripemd160(); using:".into(),
                        Box::new(sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None))),
                    ))
                }

                "ecrecover" => {
                    // ecrecover(hash, v, r, s) => std::ecr::ec_recover(sig, msg_hash)

                    //
                    // TODO: how should we generate the sig value from v,r,s?
                    //

                    if parameters.len() != 4 {
                        panic!("Invalid ecrecover call: {expression:#?}");
                    }

                    Ok(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("std::ecr::ec_recover".into()),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::create_todo(Some("ecrecover: how should we generate the sig value from v,r,s?".into())),
                            parameters[0].clone(),
                        ],
                    }))
                }

                "selfdestruct" => {
                    //
                    // TODO: how should we handle this?
                    //

                    Ok(sway::Expression::create_unimplemented(Some("selfdestruct is not supported in sway".into())))
                }

                "assert" => {
                    // assert(x) => assert(x)

                    if parameters.len() != 1 {
                        panic!("Invalid assert call: {expression:#?}");
                    }

                    Ok(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("assert".into()),
                        generic_parameters: None,
                        parameters,
                    }))
                }

                "require" => {
                    // require(x) => require(x, "Requirement failed: x")
                    // require(x, "msg") => require(x, "msg")

                    if parameters.len() == 1 {
                        parameters.push(
                            sway::Expression::from(sway::Literal::String(
                                format!("Requirement failed: {}", sway::TabbedDisplayer(&parameters[0])),
                            ))
                        );
                    }

                    if parameters.len() != 2 {
                        panic!("Invalid require call: {expression:#?}");
                    }

                    Ok(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("require".into()),
                        generic_parameters: None,
                        parameters,
                    }))
                }

                "revert" => {
                    // revert() => revert(0)
                    // revert("msg") => {
                    //     log("msg");
                    //     revert(0);
                    // }

                    if parameters.is_empty() {
                        return Ok(sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier("revert".into()),
                            generic_parameters: None,
                            parameters: vec![
                                sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                            ],
                        }));
                    }

                    if parameters.len() != 1 {
                        panic!("Invalid revert call: {expression:#?}");
                    }

                    Ok(sway::Expression::from(sway::Block {
                        statements: vec![
                            sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier("log".into()),
                                generic_parameters: None,
                                parameters,
                            })),

                            sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier("revert".into()),
                                generic_parameters: None,
                                parameters: vec![
                                    sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                                ],
                            })),
                        ],
                        
                        final_expr: None,
                    }))
                }

                old_name => {
                    let mut parameter_types = parameters.iter()
                        .map(|p| translated_definition.get_expression_type(scope.clone(), p))
                        .collect::<Result<Vec<_>, _>>()?;
        
                    // Check to see if the expression is a by-value struct constructor
                    if let Some(struct_definition) = translated_definition.structs.iter().find(|s| s.name == old_name).cloned() {
                        let mut valid = true;

                        if parameters.len() != struct_definition.fields.len() {
                            if let Some(named_arguments) = named_arguments {
                                if named_arguments.len() != struct_definition.fields.len() {
                                    valid = false;
                                } else {
                                    parameters = vec![];
                                    parameter_types = vec![];

                                    for field in struct_definition.fields.iter() {
                                        let arg = named_arguments.iter().find(|a| {
                                            let new_name = crate::translate_naming_convention(&a.name.name, Case::Snake);
                                            new_name == field.name
                                        }).unwrap();

                                        let parameter = translate_expression(project, translated_definition, scope.clone(), &arg.expr)?;
                                        let parameter_type = translated_definition.get_expression_type(scope.clone(), &parameter)?;

                                        parameters.push(parameter);
                                        parameter_types.push(parameter_type);
                                    }
                                }
                            } else {
                                valid = false;
                            }
                        }

                        if valid && !struct_definition.fields.iter().zip(parameter_types.iter()).all(|(f, t)| f.type_name.is_compatible_with(t)) {
                            valid = false;
                        }

                        if valid {
                            return Ok(sway::Expression::from(sway::Constructor {
                                type_name: sway::TypeName::Identifier {
                                    name: struct_definition.name.clone(),
                                    generic_parameters: None,
                                },

                                fields: struct_definition.fields.iter()
                                    .zip(parameters.iter())
                                    .map(|(field, value)| sway::ConstructorField {
                                        name: field.name.clone(),
                                        value: value.clone(),
                                    })
                                    .collect(),
                            }));
                        }
                    }

                    // Check to see if the expression is an ABI type
                    if parameters.len() == 1 {
                        if let Some(external_definition) = project.find_definition_with_abi(old_name) {
                            match translated_definition.get_expression_type(scope.clone(), &parameters[0])? {
                                sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                                    ("Identity", None) => {
                                        // Ensure the ABI is added to the current definition
                                        if !translated_definition.abis.iter().any(|a| a.name == old_name) {
                                            translated_definition.abis.push(external_definition.abi.as_ref().unwrap().clone());
                                        }
                                        
                                        return Ok(sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::Identifier("abi".into()),
                                            generic_parameters: None,
                                            parameters: vec![
                                                sway::Expression::Identifier(old_name.into()),
        
                                                // x.as_contract_id().unwrap().into()
                                                sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::from(sway::MemberAccess {
                                                        expression: sway::Expression::from(sway::FunctionCall {
                                                            function: sway::Expression::from(sway::MemberAccess {
                                                                expression: sway::Expression::from(sway::FunctionCall {
                                                                    function: sway::Expression::from(sway::MemberAccess {
                                                                        expression: parameters[0].clone(),
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
                                                        member: "into".into(),
                                                    }),
                                                    generic_parameters: None,
                                                    parameters: vec![],
                                                }),
                                            ],
                                        }));
                                    }

                                    ("u256" | "b256", None) => {
                                        // Thing(x) => abi(Thing, Identity::from(ContractId::from(x)))

                                        // Ensure the ABI is added to the current definition
                                        if !translated_definition.abis.iter().any(|a| a.name == old_name) {
                                            translated_definition.abis.push(external_definition.abi.as_ref().unwrap().clone());
                                        }

                                        // abi(Thing, Identity::from(ContractId::from(x)))
                                        return Ok(sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::Identifier("abi".into()),
                                            generic_parameters: None,
                                            parameters: vec![
                                                sway::Expression::Identifier(old_name.into()),

                                                // Identity::from(ContractId::from(x))
                                                sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::Identifier("Identity::from".into()),
                                                    generic_parameters: None,
                                                    parameters: vec![
                                                        // ContractId::from(x)
                                                        sway::Expression::from(sway::FunctionCall {
                                                            function: sway::Expression::Identifier("ContractId::from".into()),
                                                            generic_parameters: None,
                                                            parameters: vec![
                                                                parameters[0].clone(),
                                                            ],
                                                        }),
                                                    ],
                                                }),
                                            ],
                                        }));
                                    }

                                    (name, _) if name == old_name => {
                                        println!("Parameter type matches ABI type: {}", sway::TabbedDisplayer(&parameters[0]));
                                        // TODO
                                    }

                                    (name, _) => {
                                        println!("Parameter {} has type {}", sway::TabbedDisplayer(&parameters[0]), name);
                                    }
                                }

                                type_name => {
                                    println!("Parameter has type {}", sway::TabbedDisplayer(&type_name));
                                }
                            }
                        }
                    }

                    if let Some(named_arguments) = named_arguments {
                        let mut named_parameters = vec![];

                        for arg in named_arguments {
                            named_parameters.push((
                                crate::translate_naming_convention(&arg.name.name, Case::Snake),
                                translate_expression(project, translated_definition, scope.clone(), &arg.expr)?
                            ));
                        }

                        if let Some(function) = scope.borrow().find_function(|f| {
                            let f = f.borrow();

                            if f.old_name != old_name {
                                return false;
                            }

                            if f.parameters.entries.len() != named_parameters.len() {
                                return false;
                            }

                            f.parameters.entries.iter().all(|p| named_parameters.iter().any(|(name, _)| p.name == *name))
                        }) {
                            let function = function.borrow();

                            parameters = vec![];
                            parameter_types = vec![];

                            for parameter in function.parameters.entries.iter() {
                                let arg = named_arguments.iter().find(|a| {
                                    let new_name = crate::translate_naming_convention(&a.name.name, Case::Snake);
                                    new_name == parameter.name
                                }).unwrap();

                                let parameter = translate_expression(project, translated_definition, scope.clone(), &arg.expr)?;
                                let parameter_type = translated_definition.get_expression_type(scope.clone(), &parameter)?;

                                parameters.push(parameter);
                                parameter_types.push(parameter_type);
                            }
                        }
                    }

                    // Ensure the function exists in scope
                    let parameters_cell = Rc::new(RefCell::new(parameters.as_mut_slice()));
    
                    if let Some(function) = scope.borrow().find_function(|function| {
                        let function = function.borrow();

                        let mut parameters = parameters_cell.borrow_mut();

                        // Ensure the function's old name matches the function call we're translating
                        if function.old_name != old_name {
                            return false;
                        }

                        // Ensure the supplied function call args match the function's parameters
                        if parameters.len() != function.parameters.entries.len() {
                            return false;
                        }

                        for (i, value_type_name) in parameter_types.iter().enumerate() {
                            let Some(parameter_type_name) = function.parameters.entries[i].type_name.as_ref() else { continue };

                            //
                            // If `parameter_type_name` is `Identity`, but `container` is an abi cast expression,
                            // then we need to de-cast it, so `container` turns into the 2nd parameter of the abi cast,
                            // and `value_type_name` turns into `Identity`.
                            //

                            if let sway::TypeName::Identifier { name: parameter_type_name, generic_parameters: None } = parameter_type_name {
                                if parameter_type_name == "Identity" {
                                    if let sway::Expression::FunctionCall(function_call) = parameters[i].clone() {
                                        if let sway::Expression::Identifier(function_name) = &function_call.function {
                                            if function_name == "abi" {
                                                parameters[i] = function_call.parameters[1].clone();
                                                continue;
                                            }
                                        }
                                    }
                                }
                            }

                            match parameter_type_name {
                                sway::TypeName::Identifier { name: parameter_name, generic_parameters } => match (parameter_name.as_str(), generic_parameters.as_ref()) {
                                    ("Bytes", None) => match value_type_name {
                                        sway::TypeName::StringSlice => {
                                            // Bytes::from(raw_slice::from_parts::<u8>(s.as_ptr(), s.len()))
                                            parameters[i] = sway::Expression::from(sway::FunctionCall {
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
                                                                },
                                                            ],
                                                        }),
                                                        parameters: vec![
                                                            sway::Expression::from(sway::FunctionCall {
                                                                function: sway::Expression::from(sway::MemberAccess {
                                                                    expression: parameters[i].clone(),
                                                                    member: "as_ptr".into(),
                                                                }),
                                                                generic_parameters: None,
                                                                parameters: vec![],
                                                            }),
                                                            sway::Expression::from(sway::FunctionCall {
                                                                function: sway::Expression::from(sway::MemberAccess {
                                                                    expression: parameters[i].clone(),
                                                                    member: "len".into(),
                                                                }),
                                                                generic_parameters: None,
                                                                parameters: vec![],
                                                            }),
                                                        ],
                                                    }),
                                                ],
                                            });
                                            continue;
                                        }
                                        _ => {}
                                    }
                                    _ => {}
                                },
                                _ => {}
                            }
                            
                            if !value_type_name.is_compatible_with(parameter_type_name) {
                                // println!(
                                //     "incompatible parameter types: {} ({value_type_name:#?}) vs {} ({parameter_type_name:#?}), {:#?}",
                                //     sway::TabbedDisplayer(value_type_name),
                                //     sway::TabbedDisplayer(parameter_type_name),
                                //     parameters,
                                // );
                                return false;
                            }
                        }

                        true
                    }) {
                        let function = function.borrow();
                        
                        *translated_definition.function_call_counts.entry(function.new_name.clone()).or_insert(0) += 1;
                        translated_definition.functions_called
                            .entry(translated_definition.current_functions.last().cloned().unwrap())
                            .or_insert_with(|| vec![])
                            .push(function.new_name.clone());

                        return Ok(sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier(function.new_name.clone()),
                            generic_parameters: None,
                            parameters,
                        }));
                    }

                    // If we couldn't find the function in scope, we wanna check if this is a struct constructor
                    if let Some(struct_definition) = translated_definition.structs.iter().find(|s| s.name == old_name).cloned() {
                        return Ok(sway::Expression::from(sway::Constructor {
                            type_name: sway::TypeName::Identifier {
                                name: struct_definition.name.clone(),
                                generic_parameters: None,
                            },

                            fields: struct_definition.fields.iter()
                                .zip(parameters.iter())
                                .map(|(field, value)| sway::ConstructorField {
                                    name: field.name.clone(),
                                    value: value.clone(),
                                })
                                .collect(),
                        }));
                    }

                    panic!(
                        "{}error: Failed to find function `{old_name}({})` in scope",
                        match project.loc_to_line_and_column(&translated_definition.path, &function.loc()) {
                            Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                            None => format!("{} - ", translated_definition.path.to_string_lossy()),
                        },
                        parameter_types.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "),
                    )
                }
            }
        }

        solidity::Expression::MemberAccess(_, container, member) => {
            match container.as_ref() {
                solidity::Expression::Type(_, ty) => match ty {
                    solidity::Type::String => match member.name.as_str() {
                        "concat" => {
                            // string.concat(x) => ???

                            //
                            // TODO: how should this be handled?
                            //

                            return Ok(sway::Expression::create_todo(Some(expression.to_string())));
                        }
                        
                        member => todo!("translate `string.{member}``")
                    }

                    solidity::Type::DynamicBytes => match member.name.as_str() {
                        "concat" => {
                            // bytes.concat(x) => ???

                            //
                            // TODO: how should this be handled?
                            //

                            return Ok(sway::Expression::create_todo(Some(expression.to_string())));
                        }
                        
                        member => todo!("translate `bytes.{member}`")
                    }

                    _ => todo!("translate member access function call: {expression} - {expression:#?}"),
                }

                solidity::Expression::Variable(solidity::Identifier { name, .. }) => match name.as_str() {
                    "abi" => match member.name.as_str() {
                        "decode" => {
                            // abi.decode(encodedData, (uint256, bool)) =>
                            // let (a, b): (u256, bool) = {
                            //     let slice = encoded_data.as_raw_slice();
                            //     let mut ptr = slice.ptr();
                            //     let a = ptr.read::<u256>();
                            //     ptr = ptr.add::<u256>(1);
                            //     let b = ptr.read::<bool>();
                            //     ptr = ptr.add::<bool>(1);
                            //     (a, b)
                            // };

                            if arguments.len() != 2 {
                                panic!("Invalid `abi.decode` call: expected 2 arguments, found {}: {} - {expression:#?}", arguments.len(), expression);
                            }

                            let encoded_data = translate_expression(project, translated_definition, scope.clone(), &arguments[0])?;
                            
                            let parameter_types = match &arguments[1] {
                                solidity::Expression::List(_, parameter_types) => {
                                    parameter_types.iter()
                                        .map(|(_, p)| translate_type_name(project, translated_definition, &p.as_ref().unwrap().ty, false, false))
                                        .collect::<Vec<_>>()
                                }

                                solidity::Expression::Parenthesis(_, expression) if matches!(expression.as_ref(), solidity::Expression::Type(_, _)) => {
                                    vec![
                                        translate_type_name(project, translated_definition, expression, false, false),
                                    ]
                                }

                                _ => {
                                    panic!("Invalid `abi.decode` call: expected type list, found {} - {:#?}", arguments[1], arguments[1]);
                                }
                            };

                            let parameter_names = ('a'..='z').enumerate()
                                .take_while(|(i, _)| *i < parameter_types.len())
                                .map(|(_, c)| c.to_string())
                                .collect::<Vec<_>>();

                            if parameter_types.len() != parameter_names.len() {
                                panic!("Failed to generate parameter names for `{}`", expression);
                            }

                            // If we only have 1 parameter to decode, just decode it directly
                            if parameter_types.len() == 1 {
                                // encoded_data.as_raw_slice().ptr().read::<u256>()
                                return Ok(
                                    sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::from(sway::MemberAccess {
                                            expression: sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::from(sway::MemberAccess {
                                                    expression: sway::Expression::from(sway::FunctionCall {
                                                        function: sway::Expression::from(sway::MemberAccess {
                                                            expression: encoded_data.clone(),
                                                            member: "as_raw_slice".into(),
                                                        }),
                                                        generic_parameters: None,
                                                        parameters: vec![],
                                                    }),
                                                    member: "ptr".into(),
                                                }),
                                                generic_parameters: None,
                                                parameters: vec![],
                                            }),
                                            member: "read".into(),
                                        }),
                                        generic_parameters: Some(sway::GenericParameterList {
                                            entries: vec![
                                                sway::GenericParameter {
                                                    type_name: parameter_types[0].clone(),
                                                    implements: None,
                                                },
                                            ],
                                        }),
                                        parameters: vec![],
                                    })
                                )
                            }

                            let mut block = sway::Block {
                                statements: vec![
                                    // let mut ptr = encoded_data.as_raw_slice().ptr();
                                    sway::Statement::from(sway::Let {
                                        pattern: sway::LetPattern::from(sway::LetIdentifier {
                                            // This only needs to be mutable if there's multiple parameters to decode
                                            is_mutable: parameter_names.len() > 1,
                                            name: "ptr".into(),
                                        }),
                                        type_name: None,
                                        value: sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::from(sway::MemberAccess {
                                                expression: sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::from(sway::MemberAccess {
                                                        expression: encoded_data.clone(),
                                                        member: "as_raw_slice".into(),
                                                    }),
                                                    generic_parameters: None,
                                                    parameters: vec![],
                                                }),
                                                member: "ptr".into(),
                                            }),
                                            generic_parameters: None,
                                            parameters: vec![],
                                        }),
                                    }),
                                ],
                                final_expr: Some(sway::Expression::Tuple(
                                    parameter_names.iter()
                                        .map(|p| sway::Expression::Identifier(p.clone()))
                                        .collect()
                                )),
                            };

                            for (i, (parameter_name, parameter_type)) in parameter_names.iter().zip(parameter_types.iter()).enumerate() {
                                // let a = ptr.read::<u256>();
                                block.statements.push(sway::Statement::from(sway::Let {
                                    pattern: sway::LetPattern::from(sway::LetIdentifier {
                                        is_mutable: false,
                                        name: parameter_name.clone(),
                                    }),
                                    type_name: None,
                                    value: sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::from(sway::MemberAccess {
                                            expression: sway::Expression::Identifier("ptr".into()),
                                            member: "read".into(),
                                        }),
                                        generic_parameters: Some(sway::GenericParameterList {
                                            entries: vec![
                                                sway::GenericParameter {
                                                    type_name: parameter_type.clone(),
                                                    implements: None,
                                                },
                                            ],
                                        }),
                                        parameters: vec![],
                                    }),
                                }));

                                // If we have more parameters to decode, increase the ptr
                                if i < parameter_names.len() - 1 {
                                    // ptr = ptr.add::<u256>(1);
                                    block.statements.push(sway::Statement::from(sway::Expression::from(sway::BinaryExpression {
                                        operator: "=".into(),
                                        lhs: sway::Expression::Identifier("ptr".into()),
                                        rhs: sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::from(sway::MemberAccess {
                                                expression: sway::Expression::Identifier("ptr".into()),
                                                member: "add".into(),
                                            }),
                                            generic_parameters: Some(sway::GenericParameterList {
                                                entries: vec![
                                                    sway::GenericParameter {
                                                        type_name: parameter_type.clone(),
                                                        implements: None,
                                                    },
                                                ],
                                            }),
                                            parameters: vec![
                                                sway::Expression::from(sway::Literal::DecInt(BigUint::one(), None)),
                                            ],
                                        }),
                                    })));
                                }
                            }

                            return Ok(sway::Expression::from(block));
                        }

                        "encode" | "encodePacked" => {
                            // abi.encode(a, b, ...) | abi.encodePacked(a, b, ...) => {
                            //     let mut bytes = Bytes::new();
                            //     bytes.append(Bytes::from(core::codec::encode(a)));
                            //     bytes.append(Bytes::from(core::codec::encode(b)));
                            //     // ...
                            //     bytes
                            // }

                            // Ensure `std::bytes::Bytes` is imported
                            translated_definition.ensure_use_declared("std::bytes::Bytes");

                            // Generate a unique variable name
                            let variable_name = scope.borrow_mut().generate_unique_variable_name("bytes");

                            let parameters = arguments.iter()
                                .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                                .collect::<Result<Vec<_>, _>>()?;
                            
                            // Create the abi encoding block
                            let mut block = sway::Block {
                                statements: vec![
                                    sway::Statement::from(sway::Let {
                                        pattern: sway::LetPattern::from(sway::LetIdentifier {
                                            is_mutable: true,
                                            name: variable_name.clone(),
                                        }),
                                        type_name: None,
                                        value: sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::Identifier("Bytes::new".into()),
                                            generic_parameters: None,
                                            parameters: vec![],
                                        }),
                                    }),
                                ],
                                final_expr: Some(sway::Expression::Identifier(variable_name.clone())),
                            };
                            
                            // Add the encoding statements to the block
                            for parameter in parameters {
                                block.statements.push(sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: sway::Expression::Identifier(variable_name.clone()),
                                        member: "append".into(),
                                    }),
                                    generic_parameters: None,
                                    parameters: vec![
                                        sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::Identifier("Bytes::from".into()),
                                            generic_parameters: None,
                                            parameters: vec![
                                                sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::Identifier("core::codec::encode".into()),
                                                    generic_parameters: None,
                                                    parameters: vec![
                                                        parameter.clone(),
                                                    ],
                                                }),
                                            ],
                                        }),
                                    ],
                                })));
                            }

                            return Ok(sway::Expression::from(block))
                        }

                        "encodeWithSelector" => {
                            // abi.encodeWithSelector(selector, ...) => ???

                            //
                            // TODO: how should this be handled?
                            //

                            return Ok(sway::Expression::create_todo(Some(expression.to_string())))
                        }
                        
                        "encodeWithSignature" => {
                            // abi.encodeWithSignature(signature, ...) => ???

                            //
                            // TODO: how should this be handled?
                            //

                            return Ok(sway::Expression::create_todo(Some(expression.to_string())))
                        }
                        
                        "encodeCall" => {
                            // abi.encodeCall(functionPointer, (...)) => ???

                            //
                            // TODO: how should this be handled?
                            //

                            return Ok(sway::Expression::create_todo(Some(expression.to_string())))
                        }
                        
                        member => todo!("handle `abi.{member}` translation"),
                    }

                    "super" => {
                        let mut parameters = arguments.iter()
                            .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                            .collect::<Result<Vec<_>, _>>()?;

                        let mut parameter_types = parameters.iter()
                            .map(|p| translated_definition.get_expression_type(scope.clone(), p))
                            .collect::<Result<Vec<_>, _>>()?;

                        for inherit in translated_definition.inherits.clone() {
                            let Some(inherited_definition) = project.find_definition_with_abi(&inherit).cloned() else { continue };

                            if let Some(named_arguments) = named_arguments {
                                let mut named_parameters = vec![];
        
                                for arg in named_arguments {
                                    named_parameters.push((
                                        crate::translate_naming_convention(&arg.name.name, Case::Snake),
                                        translate_expression(project, translated_definition, scope.clone(), &arg.expr)?
                                    ));
                                }
        
                                if let Some(function) = inherited_definition.toplevel_scope.borrow().find_function(|f| {
                                    let f = f.borrow();
        
                                    if f.old_name != member.name.as_str() {
                                        return false;
                                    }
        
                                    if f.parameters.entries.len() != named_parameters.len() {
                                        return false;
                                    }
        
                                    f.parameters.entries.iter().all(|p| named_parameters.iter().any(|(name, _)| p.name == *name))
                                }) {
                                    let function = function.borrow();
        
                                    parameters = vec![];
                                    parameter_types = vec![];
        
                                    for parameter in function.parameters.entries.iter() {
                                        let arg = named_arguments.iter().find(|a| {
                                            let new_name = crate::translate_naming_convention(&a.name.name, Case::Snake);
                                            new_name == parameter.name
                                        }).unwrap();
        
                                        let parameter = translate_expression(project, translated_definition, scope.clone(), &arg.expr)?;
                                        let parameter_type = translated_definition.get_expression_type(scope.clone(), &parameter)?;
        
                                        parameters.push(parameter);
                                        parameter_types.push(parameter_type);
                                    }
                                }
                            }
                            
                            let parameters_cell = Rc::new(RefCell::new(parameters.as_mut_slice()));

                            if let Some(function) = inherited_definition.toplevel_scope.clone().borrow().find_function(|function| {
                                let function = function.borrow();

                                let mut parameters = parameters_cell.borrow_mut();

                                // Ensure the function's old name matches the function call we're translating
                                if function.old_name != member.name.as_str() {
                                    return false;
                                }

                                // Ensure the supplied function call args match the function's parameters
                                if parameters.len() != function.parameters.entries.len() {
                                    return false;
                                }

                                for (i, value_type_name) in parameter_types.iter().enumerate() {
                                    let Some(parameter_type_name) = function.parameters.entries[i].type_name.as_ref() else { continue };

                                    //
                                    // If `parameter_type_name` is `Identity`, but `container` is an abi cast expression,
                                    // then we need to de-cast it, so `container` turns into the 2nd parameter of the abi cast,
                                    // and `value_type_name` turns into `Identity`.
                                    //

                                    if let sway::TypeName::Identifier { name: parameter_type_name, generic_parameters: None } = parameter_type_name {
                                        if parameter_type_name == "Identity" {
                                            if let sway::Expression::FunctionCall(function_call) = parameters[i].clone() {
                                                if let sway::Expression::Identifier(function_name) = &function_call.function {
                                                    if function_name == "abi" {
                                                        parameters[i] = function_call.parameters[1].clone();
                                                        continue;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    
                                    if !value_type_name.is_compatible_with(parameter_type_name) {
                                        return false;
                                    }
                                }

                                true
                            }) {
                                let function = function.borrow();
                                
                                *translated_definition.function_call_counts.entry(function.new_name.clone()).or_insert(0) += 1;
                                translated_definition.functions_called
                                    .entry(translated_definition.current_functions.last().cloned().unwrap())
                                    .or_insert_with(|| vec![])
                                    .push(function.new_name.clone());

                                return Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier(function.new_name.clone()),
                                    generic_parameters: None,
                                    parameters,
                                }));
                            }
                        }

                        todo!("handle super member access function `{member:#?}`")
                    }

                    "this" => {
                        let mut parameters = arguments.iter()
                            .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                            .collect::<Result<Vec<_>, _>>()?;

                        let mut parameter_types = parameters.iter()
                            .map(|p| translated_definition.get_expression_type(scope.clone(), p))
                            .collect::<Result<Vec<_>, _>>()?;
                            
                        if let Some(named_arguments) = named_arguments {
                            let mut named_parameters = vec![];

                            for arg in named_arguments {
                                named_parameters.push((
                                    crate::translate_naming_convention(&arg.name.name, Case::Snake),
                                    translate_expression(project, translated_definition, scope.clone(), &arg.expr)?
                                ));
                            }

                            if let Some(function) = scope.borrow().find_function(|f| {
                                let f = f.borrow();

                                if f.old_name != member.name {
                                    return false;
                                }

                                if f.parameters.entries.len() != named_parameters.len() {
                                    return false;
                                }

                                f.parameters.entries.iter().all(|p| named_parameters.iter().any(|(name, _)| p.name == *name))
                            }) {
                                let function = function.borrow();

                                parameters = vec![];
                                parameter_types = vec![];

                                for parameter in function.parameters.entries.iter() {
                                    let arg = named_arguments.iter().find(|a| {
                                        let new_name = crate::translate_naming_convention(&a.name.name, Case::Snake);
                                        new_name == parameter.name
                                    }).unwrap();

                                    let parameter = translate_expression(project, translated_definition, scope.clone(), &arg.expr)?;
                                    let parameter_type = translated_definition.get_expression_type(scope.clone(), &parameter)?;

                                    parameters.push(parameter);
                                    parameter_types.push(parameter_type);
                                }
                            }
                        }

                        let parameters_cell = Rc::new(RefCell::new(parameters.as_mut_slice()));

                        if let Some(function) = scope.borrow().find_function(|function| {
                            let function = function.borrow();

                            let mut parameters = parameters_cell.borrow_mut();

                            // Ensure the function's old name matches the function call we're translating
                            if function.old_name != member.name {
                                return false;
                            }

                            // Ensure the supplied function call args match the function's parameters
                            if parameters.len() != function.parameters.entries.len() {
                                return false;
                            }

                            for (i, value_type_name) in parameter_types.iter().enumerate() {
                                let Some(parameter_type_name) = function.parameters.entries[i].type_name.as_ref() else { continue };

                                //
                                // If `parameter_type_name` is `Identity`, but `container` is an abi cast expression,
                                // then we need to de-cast it, so `container` turns into the 2nd parameter of the abi cast,
                                // and `value_type_name` turns into `Identity`.
                                //

                                if let sway::TypeName::Identifier { name: parameter_type_name, generic_parameters: None } = parameter_type_name {
                                    if parameter_type_name == "Identity" {
                                        if let sway::Expression::FunctionCall(function_call) = parameters[i].clone() {
                                            if let sway::Expression::Identifier(function_name) = &function_call.function {
                                                if function_name == "abi" {
                                                    parameters[i] = function_call.parameters[1].clone();
                                                    continue;
                                                }
                                            }
                                        }
                                    }
                                }
                                
                                if !value_type_name.is_compatible_with(parameter_type_name) {
                                    return false;
                                }
                            }

                            true
                        }) {
                            let function = function.borrow();
                            
                            *translated_definition.function_call_counts.entry(function.new_name.clone()).or_insert(0) += 1;
                            translated_definition.functions_called
                                .entry(translated_definition.current_functions.last().cloned().unwrap())
                                .or_insert_with(|| vec![])
                                .push(function.new_name.clone());

                            return Ok(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier(function.new_name.clone()),
                                generic_parameters: None,
                                parameters,
                            }));
                        }
                    }

                    name => {
                        let mut parameters = arguments.iter()
                            .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                            .collect::<Result<Vec<_>, _>>()?;

                        let mut parameter_types = parameters.iter()
                            .map(|p| translated_definition.get_expression_type(scope.clone(), p))
                            .collect::<Result<Vec<_>, _>>()?;

                        // TODO: check full inheritance heirarchy
                        // Check for explicit super function calls
                        if translated_definition.inherits.iter().any(|i| i == name) {
                            if let Some(inherited_definition) = project.find_definition_with_abi(name).cloned() {
                                if let Some(named_arguments) = named_arguments {
                                    let mut named_parameters = vec![];
            
                                    for arg in named_arguments {
                                        named_parameters.push((
                                            crate::translate_naming_convention(&arg.name.name, Case::Snake),
                                            translate_expression(project, translated_definition, scope.clone(), &arg.expr)?
                                        ));
                                    }
            
                                    if let Some(function) = inherited_definition.toplevel_scope.borrow().find_function(|f| {
                                        let f = f.borrow();
            
                                        if f.old_name != member.name.as_str() {
                                            return false;
                                        }
            
                                        if f.parameters.entries.len() != named_parameters.len() {
                                            return false;
                                        }
            
                                        f.parameters.entries.iter().all(|p| named_parameters.iter().any(|(name, _)| p.name == *name))
                                    }) {
                                        let function = function.borrow();
            
                                        parameters = vec![];
                                        parameter_types = vec![];
            
                                        for parameter in function.parameters.entries.iter() {
                                            let arg = named_arguments.iter().find(|a| {
                                                let new_name = crate::translate_naming_convention(&a.name.name, Case::Snake);
                                                new_name == parameter.name
                                            }).unwrap();
            
                                            let parameter = translate_expression(project, translated_definition, scope.clone(), &arg.expr)?;
                                            let parameter_type = translated_definition.get_expression_type(scope.clone(), &parameter)?;
            
                                            parameters.push(parameter);
                                            parameter_types.push(parameter_type);
                                        }
                                    }
                                }

                                let parameters_cell = Rc::new(RefCell::new(parameters.as_mut_slice()));

                                if let Some(function) = inherited_definition.toplevel_scope.borrow().find_function(|function| {
                                    let function = function.borrow();

                                    let mut parameters = parameters_cell.borrow_mut();
        
                                    // Ensure the function's old name matches the function call we're translating
                                    if function.old_name != member.name {
                                        return false;
                                    }
        
                                    // Ensure the supplied function call args match the function's parameters
                                    if parameters.len() != function.parameters.entries.len() {
                                        return false;
                                    }
        
                                    for (i, value_type_name) in parameter_types.iter().enumerate() {
                                        let Some(parameter_type_name) = function.parameters.entries[i].type_name.as_ref() else { continue };
        
                                        //
                                        // If `parameter_type_name` is `Identity`, but `container` is an abi cast expression,
                                        // then we need to de-cast it, so `container` turns into the 2nd parameter of the abi cast,
                                        // and `value_type_name` turns into `Identity`.
                                        //
        
                                        if let sway::TypeName::Identifier { name: parameter_type_name, generic_parameters: None } = parameter_type_name {
                                            if parameter_type_name == "Identity" {
                                                if let sway::Expression::FunctionCall(function_call) = parameters[i].clone() {
                                                    if let sway::Expression::Identifier(function_name) = &function_call.function {
                                                        if function_name == "abi" {
                                                            parameters[i] = function_call.parameters[1].clone();
                                                            continue;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        
                                        if !value_type_name.is_compatible_with(parameter_type_name) {
                                            return false;
                                        }
                                    }
        
                                    true
                                }) {
                                    let function = function.borrow();
                                    
                                    *translated_definition.function_call_counts.entry(function.new_name.clone()).or_insert(0) += 1;
                                    translated_definition.functions_called
                                        .entry(translated_definition.current_functions.last().cloned().unwrap())
                                        .or_insert_with(|| vec![])
                                        .push(function.new_name.clone());
        
                                    return Ok(sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::Identifier(function.new_name.clone()),
                                        generic_parameters: None,
                                        parameters,
                                    }));
                                }
                            }
                        }

                        // Check to see if container is a user-defined type name
                        if translated_definition.type_definitions.iter().any(|t| {
                            let sway::TypeName::Identifier { name: type_name, generic_parameters: None } = &t.name else { return false };
                            type_name == name
                        }) {
                            if let "wrap" | "unwrap" = member.name.as_str() {
                                return Ok(parameters[0].clone());
                            }
                        }

                        // Check if function is contained in an external definition
                        if let Some(external_definition) = project.translated_definitions.iter().find(|x| x.name == name).cloned() {
                            let old_name = member.name.clone();
    
                            // Check to see if the expression is a by-value struct constructor
                            if let Some(struct_definition) = external_definition.structs.iter().find(|s| s.name == old_name).cloned() {
                                let mut valid = true;

                                if parameters.len() != struct_definition.fields.len() {
                                    if let Some(named_arguments) = named_arguments {
                                        if named_arguments.len() != struct_definition.fields.len() {
                                            valid = false;
                                        } else {
                                            parameters = vec![];
                                            parameter_types = vec![];

                                            for field in struct_definition.fields.iter() {
                                                let arg = named_arguments.iter().find(|a| {
                                                    let new_name = crate::translate_naming_convention(&a.name.name, Case::Snake);
                                                    new_name == field.name
                                                }).unwrap();

                                                let parameter = translate_expression(project, translated_definition, scope.clone(), &arg.expr)?;
                                                let parameter_type = translated_definition.get_expression_type(scope.clone(), &parameter)?;

                                                parameters.push(parameter);
                                                parameter_types.push(parameter_type);
                                            }
                                        }
                                    } else {
                                        valid = false;
                                    }
                                }

                                if valid && !struct_definition.fields.iter().zip(parameter_types.iter()).all(|(f, t)| f.type_name.is_compatible_with(t)) {
                                    valid = false;
                                }

                                if valid {
                                    if !translated_definition.structs.contains(&struct_definition) {
                                        for field in struct_definition.fields.iter() {
                                            match &field.type_name {
                                                sway::TypeName::Identifier{ name, .. } => {
                                                    if !translated_definition.structs.iter().any(|s| s.name == *name) {
                                                        for external_definition in project.translated_definitions.iter() {
                                                            for s in external_definition.structs.iter() {
                                                                if s.name == *name {
                                                                    translated_definition.structs.push(s.clone());
                                                                }
                                                            }
                                                        }                        
                                                    }
                                                },
                                                
                                                _ => {}
                                            }
                                        }
                                        translated_definition.structs.push(struct_definition.clone());
                                    }

                                    return Ok(sway::Expression::from(sway::Constructor {
                                        type_name: sway::TypeName::Identifier {
                                            name: struct_definition.name.clone(),
                                            generic_parameters: None,
                                        },

                                        fields: struct_definition.fields.iter()
                                            .zip(parameters.iter())
                                            .map(|(field, value)| sway::ConstructorField {
                                                name: field.name.clone(),
                                                value: value.clone(),
                                            })
                                            .collect(),
                                    }));
                                }
                            }

                            if let Some(named_arguments) = named_arguments {
                                let mut named_parameters = vec![];
        
                                for arg in named_arguments {
                                    named_parameters.push((
                                        crate::translate_naming_convention(&arg.name.name, Case::Snake),
                                        translate_expression(project, translated_definition, scope.clone(), &arg.expr)?
                                    ));
                                }
        
                                if let Some(function) = scope.borrow().find_function(|f| {
                                    let f = f.borrow();
        
                                    if f.old_name != old_name {
                                        return false;
                                    }
        
                                    if f.parameters.entries.len() != named_parameters.len() {
                                        return false;
                                    }
        
                                    f.parameters.entries.iter().all(|p| named_parameters.iter().any(|(name, _)| p.name == *name))
                                }) {
                                    let function = function.borrow();
        
                                    parameters = vec![];
                                    parameter_types = vec![];
        
                                    for parameter in function.parameters.entries.iter() {
                                        let arg = named_arguments.iter().find(|a| {
                                            let new_name = crate::translate_naming_convention(&a.name.name, Case::Snake);
                                            new_name == parameter.name
                                        }).unwrap();
        
                                        let parameter = translate_expression(project, translated_definition, scope.clone(), &arg.expr)?;
                                        let parameter_type = translated_definition.get_expression_type(scope.clone(), &parameter)?;
        
                                        parameters.push(parameter);
                                        parameter_types.push(parameter_type);
                                    }
                                }
                            }
        
                            let parameters_cell = Rc::new(RefCell::new(parameters.as_mut_slice()));

                            // Check if the member is a function defined in the toplevel scope
                            let Some(external_function_declaration) = external_definition.toplevel_scope.borrow().find_function(|function| {
                                let function = function.borrow();

                                let mut parameters = parameters_cell.borrow_mut();

                                // Ensure the function's old name matches the function call we're translating
                                if function.old_name != old_name {
                                    return false;
                                }

                                // Ensure the supplied function call args match the function's parameters
                                if parameters.len() != function.parameters.entries.len() {
                                    return false;
                                }

                                for (i, value_type_name) in parameter_types.iter().enumerate() {
                                    let Some(parameter_type_name) = function.parameters.entries[i].type_name.as_ref() else { continue };

                                    //
                                    // If `parameter_type_name` is `Identity`, but `container` is an abi cast expression,
                                    // then we need to de-cast it, so `container` turns into the 2nd parameter of the abi cast,
                                    // and `value_type_name` turns into `Identity`.
                                    //

                                    if let sway::TypeName::Identifier { name: parameter_type_name, generic_parameters: None } = parameter_type_name {
                                        if parameter_type_name == "Identity" {
                                            if let sway::Expression::FunctionCall(function_call) = parameters[i].clone() {
                                                if let sway::Expression::Identifier(function_name) = &function_call.function {
                                                    if function_name == "abi" {
                                                        parameters[i] = function_call.parameters[1].clone();
                                                        continue;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    
                                    if !value_type_name.is_compatible_with(parameter_type_name) {
                                        return false;
                                    }
                                }

                                true
                            }) else {
                                panic!(
                                    "{}error: Failed to find function in scope: {name}.{old_name}({})",
                                    match project.loc_to_line_and_column(&translated_definition.path, &function.loc()) {
                                        Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                                        None => format!("{} - ", translated_definition.path.to_string_lossy()),
                                    },
                                    parameter_types.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "),
                                )
                            };
    
                            let external_function_declaration = external_function_declaration.borrow();
    
                            // Get the external function definition
                            let Some(external_function_definition) = external_definition.functions.iter().find(|f| {
                                f.name == external_function_declaration.new_name
                                && f.parameters.entries.len() == external_function_declaration.parameters.entries.len()
                            }) else {
                                panic!(
                                    "{}error: Failed to find function in scope: {name}.{old_name}({})",
                                    match project.loc_to_line_and_column(&translated_definition.path, &container.loc()) {
                                        Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                                        None => format!("{} - ", translated_definition.path.to_string_lossy()),
                                    },
                                    parameter_types.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "),
                                );
                            };

                            let parameters = Rc::new(RefCell::new(parameters.as_mut_slice()));
    
                            // Import the function if we haven't already
                            let functions = translated_definition.toplevel_scope.borrow().functions.clone();
                            let mut function_found = false;
                            
                            'function_lookup: for function in functions.iter() {
                                let function = function.borrow();

                                let mut parameters = parameters.borrow_mut();

                                // Ensure the function's old name matches the function call we're translating
                                if function.old_name != member.name {
                                    continue 'function_lookup;
                                }

                                // Ensure the supplied function call args match the function's parameters
                                if parameters.len() != function.parameters.entries.len() {
                                    continue 'function_lookup;
                                }

                                'value_type_check: for (i, value_type_name) in parameter_types.iter().enumerate() {
                                    let Some(parameter_type_name) = function.parameters.entries[i].type_name.as_ref() else { continue };

                                    //
                                    // If `parameter_type_name` is `Identity`, but `container` is an abi cast expression,
                                    // then we need to de-cast it, so `container` turns into the 2nd parameter of the abi cast,
                                    // and `value_type_name` turns into `Identity`.
                                    //

                                    if let sway::TypeName::Identifier { name: parameter_type_name, generic_parameters: None } = parameter_type_name {
                                        if parameter_type_name == "Identity" {
                                            if let sway::Expression::FunctionCall(function_call) = parameters[i].clone() {
                                                if let sway::Expression::Identifier(function_name) = &function_call.function {
                                                    if function_name == "abi" {
                                                        parameters[i] = function_call.parameters[1].clone();
                                                        continue 'value_type_check;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    
                                    if !value_type_name.is_compatible_with(parameter_type_name) {
                                        continue 'function_lookup;
                                    }
                                }

                                function_found = true;
                                break;
                            }
                            
                            if !function_found {
                                // Create the local function definition
                                let mut local_function_definition = external_function_definition.clone();
                                local_function_definition.name = external_function_declaration.new_name.clone();
                                
                                // Add the local function definition to the beginning of the list
                                translated_definition.functions.insert(0, local_function_definition);
    
                                // Create the local function declaration for the toplevel scope
                                let local_function_declaration = external_function_declaration.clone();
    
                                // Add the local function to the beginning of the toplevel scope
                                translated_definition.toplevel_scope.borrow_mut().functions.insert(0, Rc::new(RefCell::new(local_function_declaration.clone())));
                            }

                            // TODO : Check the return type of the function 
                            // If the return type is user define type struct or enum 
                            // then we need to add to the current translated definition
                            // if we have not already
                            if let Some(return_type) = external_function_definition.return_type.as_ref() {   
                                fn check_type_name(project: &Project, translated_definition: &mut TranslatedDefinition, type_name: &sway::TypeName) {
                                    match type_name {
                                        sway::TypeName::Undefined => {},
                                        sway::TypeName::Identifier { name, generic_parameters } => {
                                            if let Some(generic_parameters) = generic_parameters.as_ref() {
                                                for entry in generic_parameters.entries.iter() {
                                                    check_type_name(project, translated_definition, &entry.type_name);
                                                }
                                            }

                                            'lookup: for external_definition in project.translated_definitions.iter() {
                                                for type_definition in external_definition.type_definitions.iter() {
                                                    if type_definition.name.is_compatible_with(type_name) {
                                                        if !translated_definition.type_definitions.contains(type_definition) {
                                                            translated_definition.type_definitions.push(type_definition.clone());
                                                            break 'lookup;
                                                        }
                                                    }
                                                }

                                                for enum_definition in external_definition.enums.iter() {
                                                    if enum_definition.type_definition.name.is_compatible_with(type_name) {
                                                        if !translated_definition.enums.contains(enum_definition) {
                                                            translated_definition.enums.push(enum_definition.clone());
                                                            break 'lookup;
                                                        }
                                                    }
                                                }

                                                for struct_definition in external_definition.structs.iter() {
                                                    if struct_definition.name != *name {
                                                        continue;
                                                    }

                                                    match (struct_definition.generic_parameters.as_ref(), generic_parameters.as_ref()) {
                                                        (Some(struct_generic_parameters), Some(generic_parameters)) => {
                                                            if struct_generic_parameters.entries.len() != generic_parameters.entries.len() {
                                                                continue;
                                                            }

                                                            if !translated_definition.structs.contains(struct_definition) {
                                                                for field in struct_definition.fields.iter() {
                                                                    match &field.type_name {
                                                                        sway::TypeName::Identifier{ name, .. } => {
                                                                            if !translated_definition.structs.iter().any(|s| s.name == *name) {
                                                                                for external_definition in project.translated_definitions.iter() {
                                                                                    for s in external_definition.structs.iter() {
                                                                                        if s.name == *name {
                                                                                            translated_definition.structs.push(s.clone());
                                                                                        }
                                                                                    }
                                                                                }                        
                                                                            }
                                                                        },
                                                                        
                                                                        _ => {}
                                                                    }
                                                                }
                                                                translated_definition.structs.push(struct_definition.clone());
                                                                break 'lookup;
                                                            }
                                                        },
                                                        (None, None) => {},
                                                        _ => continue,
                                                    }

                                                    if !translated_definition.structs.contains(struct_definition) {
                                                        for field in struct_definition.fields.iter() {
                                                            match &field.type_name {
                                                                sway::TypeName::Identifier{ name, .. } => {
                                                                    if !translated_definition.structs.iter().any(|s| s.name == *name) {
                                                                        for external_definition in project.translated_definitions.iter() {
                                                                            for s in external_definition.structs.iter() {
                                                                                if s.name == *name {
                                                                                    translated_definition.structs.push(s.clone());
                                                                                }
                                                                            }
                                                                        }                        
                                                                    }
                                                                },
                                                                
                                                                _ => {}
                                                            }
                                                        }
                                                        translated_definition.structs.push(struct_definition.clone());
                                                        break 'lookup;
                                                    }
                                                }
                                            }
                                        },
                                        sway::TypeName::Array { type_name, .. } => {
                                            check_type_name(project, translated_definition, type_name);
                                        },
                                        sway::TypeName::Tuple { type_names } => {
                                            type_names.iter().for_each(|type_name| check_type_name(project, translated_definition, type_name));
                                        },
                                        sway::TypeName::StringSlice => {},
                                        sway::TypeName::StringArray { .. } => {},
                                    }
                                }
                                check_type_name(project, translated_definition, return_type);
                            }

                            // Create the function call
                            let function_call = sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier(external_function_declaration.new_name.clone()),
                                generic_parameters: None,
                                parameters: arguments.iter()
                                    .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                                    .collect::<Result<Vec<_>, _>>()?,
                            });
    
                            *translated_definition.function_call_counts.entry(external_function_declaration.new_name.clone()).or_insert(0) += 1;
                            translated_definition.functions_called
                                .entry(translated_definition.current_functions.last().cloned().unwrap())
                                .or_insert_with(|| vec![])
                                .push(external_function_declaration.new_name.clone());
                            
                            return Ok(function_call);
                        }
                    }
                }

                _ => {}
            }

            let solidity_container = container;

            let mut container = translate_expression(project, translated_definition, scope.clone(), container)?;
            let type_name = translated_definition.get_expression_type(scope.clone(), &container)?;

            // println!(
            //     "type of {} is {}",
            //     sway::TabbedDisplayer(&container),
            //     sway::TabbedDisplayer(&type_name),
            // );

            match &type_name {
                sway::TypeName::Undefined => panic!("Undefined type name"),
                
                sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                    ("Identity", None) => {
                        match member.name.as_str() {
                            "transfer" => {
                                // to.transfer(amount) => std::asset::transfer(to, asset_id, amount)

                                if arguments.len() == 1 {
                                    let argument = translate_expression(project, translated_definition, scope.clone(), &arguments[0])?;
                                    let argument_type_name = translated_definition.get_expression_type(scope.clone(), &argument)?;

                                    if argument_type_name.is_uint() {
                                        return Ok(sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::Identifier("std::asset::transfer".into()),
                                            generic_parameters: None,
                                            parameters: vec![
                                                container,
                                                sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::Identifier("AssetId::default".into()),
                                                    generic_parameters: None,
                                                    parameters: vec![],
                                                }),
                                                argument,
                                            ],
                                        }));
                                    }
                                }
                            }

                            "send" => {
                                // to.send(amount) => {
                                //     std::asset::transfer(to, asset_id, amount);
                                //     true
                                // }

                                if arguments.len() == 1 {
                                    let argument = translate_expression(project, translated_definition, scope.clone(), &arguments[0])?;
                                    let argument_type_name = translated_definition.get_expression_type(scope.clone(), &argument)?;

                                    if argument_type_name.is_uint() {
                                        return Ok(sway::Expression::from(sway::Block {
                                            statements: vec![
                                                sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::Identifier("std::asset::transfer".into()),
                                                    generic_parameters: None,
                                                    parameters: vec![
                                                        container,
                                                        sway::Expression::from(sway::FunctionCall {
                                                            function: sway::Expression::Identifier("AssetId::default".into()),
                                                            generic_parameters: None,
                                                            parameters: vec![],
                                                        }),
                                                        argument,
                                                    ],
                                                })),
                                            ],
                                            final_expr: Some(sway::Expression::from(sway::Literal::Bool(true))),
                                        }));
                                    }
                                }
                            }

                            "call" => {
                                if arguments.len() == 1 {
                                    let payload = translate_expression(project, translated_definition, scope.clone(), &arguments[0])?;
                                    return translate_address_call_expression(project, translated_definition, scope.clone(), payload, None, None, None);
                                }
                            }

                            "delegatecall" => {
                                //
                                // TODO: is delegatecall possible?
                                //

                                return Ok(sway::Expression::create_todo(Some(expression.to_string())));
                            }

                            "staticcall" => {
                                //
                                // TODO: is staticcall possible?
                                //

                                return Ok(sway::Expression::create_todo(Some(expression.to_string())));
                            }

                            _ => {}
                        }

                        let mut name = name.clone();
                        let new_name_lower = crate::translate_naming_convention(member.name.as_str(), Case::Snake);
                        let new_name_upper = crate::translate_naming_convention(member.name.as_str(), Case::ScreamingSnake);

                        // Check using directives for Identity-specific function
                        for using_directive in translated_definition.using_directives.iter() {
                            let Some(external_definition) = project.translated_definitions.iter().find(|d| {
                                d.name == using_directive.library_name && matches!(d.kind.as_ref().unwrap(), solidity::ContractTy::Library(_))
                            }).cloned() else { continue };
                            
                            if let Some(for_type_name) = &using_directive.for_type {
                                if !type_name.is_identity() && *for_type_name != type_name {
                                    // println!(
                                    //     "Using directive type {} is not {}, skipping...",
                                    //     sway::TabbedDisplayer(for_type_name),
                                    //     sway::TabbedDisplayer(&type_name),
                                    // );
                                    continue;
                                }
                            }
                            
                            for f in external_definition.toplevel_scope.borrow().functions.iter() {
                                let f = f.borrow();

                                if f.old_name != member.name {
                                    continue;
                                }

                                let Some(parameter) = f.parameters.entries.first() else { continue };
                                let Some(parameter_type_name) = parameter.type_name.as_ref() else { continue };

                                if *parameter_type_name == type_name {
                                    *translated_definition.function_call_counts.entry(f.new_name.clone()).or_insert(0) += 1;
                                    translated_definition.functions_called
                                        .entry(translated_definition.current_functions.last().cloned().unwrap())
                                        .or_insert_with(|| vec![])
                                        .push(f.new_name.clone());

                                    return Ok(sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::Identifier(f.new_name.clone()),
                                        generic_parameters: None,
                                        parameters: {
                                            let mut parameters = arguments.iter()
                                                .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                                                .collect::<Result<Vec<_>, _>>()?;

                                            parameters.insert(0, container.clone());

                                            parameters
                                        },
                                    }));
                                }
                            }
                        }

                        let (variable, _) = match translate_variable_access_expression(project, translated_definition, scope.clone(), &solidity_container) {
                            Ok((variable, expression)) => (Some(variable), Some(expression)),
                            Err(_) => (None, None),
                        };

                        // Check if expression is a variable that had an ABI type
                        if let Some(variable) = variable.as_ref() {
                            if variable.is_none() {
                                panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
                            }
                            let variable = variable.as_ref().unwrap();
                            let variable = variable.borrow();

                            if let Some(abi_type_name) = variable.abi_type_name.as_ref() {
                                let abi_type_name = abi_type_name.to_string();

                                // Ensure the ABI is added to the current definition
                                if let Some(external_definition) = project.find_definition_with_abi(abi_type_name.as_str()) {
                                    if let Some(abi) = external_definition.abi.as_ref() {
                                        if abi.name == abi_type_name && !translated_definition.abis.iter().any(|a| a.name == abi.name) {
                                            translated_definition.abis.push(abi.clone());
                                        }
                                    }
                                }
                
                                // Turn the expression into an ABI cast
                                container = sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier("abi".into()),
                                    generic_parameters: None,
                                    parameters: vec![
                                        sway::Expression::Identifier(abi_type_name.clone()),

                                        // x.as_contract_id().unwrap().into()
                                        sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::from(sway::MemberAccess {
                                                expression: sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::from(sway::MemberAccess {
                                                        expression: sway::Expression::from(sway::FunctionCall {
                                                            function: sway::Expression::from(sway::MemberAccess {
                                                                expression: container,
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
                                                member: "into".into(),
                                            }),
                                            generic_parameters: None,
                                            parameters: vec![],
                                        }),
                                    ],
                                });

                                name = abi_type_name.to_string();
                            }
                        }

                        // Check to see if the type is located in an external ABI
                        if let Some(external_definition) = project.find_definition_with_abi(name.as_str()) {
                            let external_abi = external_definition.abi.as_ref().unwrap();

                            // Check lower case names for regular functions
                            if external_abi.functions.iter().any(|f| f.name == new_name_lower) {
                                // Ensure the ABI is added to the current definition
                                if !translated_definition.abis.iter().any(|a| a.name == external_abi.name) {
                                    translated_definition.abis.push(external_abi.clone());
                                }
                                
                                return Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: container.clone(),
                                        member: new_name_lower.into(),
                                    }),
                                    generic_parameters: None,
                                    parameters: arguments.iter()
                                        .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                                        .collect::<Result<Vec<_>, _>>()?,
                                }));
                            }

                            // Check upper case names for constant getter functions
                            if external_abi.functions.iter().any(|f| f.name == new_name_upper) {
                                // Ensure the ABI is added to the current definition
                                if !translated_definition.abis.iter().any(|a| a.name == external_abi.name) {
                                    translated_definition.abis.push(external_abi.clone());
                                }
                                
                                return Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: container.clone(),
                                        member: new_name_upper.into(),
                                    }),
                                    generic_parameters: None,
                                    parameters: arguments.iter()
                                        .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                                        .collect::<Result<Vec<_>, _>>()?,
                                }));
                            }
                        }
                        if variable.is_none() {
                            panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
                        }
                        let variable = variable.unwrap();
                        todo!(
                            "{}translate Identity member function call `{member}`: {} - {container:#?} - {:#?}",
                            match project.loc_to_line_and_column(&translated_definition.path, &function.loc()) {
                                Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                                None => format!("{} - ", translated_definition.path.to_string_lossy()),
                            },
                            sway::TabbedDisplayer(&container),
                            variable.unwrap().borrow(),
                        )
                    }
                    
                    ("StorageVec", Some(_)) => match member.name.as_str() {
                        "push" => {
                            let (variable, container_access) = match translate_variable_access_expression(project, translated_definition, scope.clone(), &solidity_container) {
                                Ok((variable, expression)) => (Some(variable), Some(expression)),
                                Err(_) => (None, None),
                            };
                            
                            let (Some(variable), Some(container_access)) = (variable, container_access) else {
                                panic!("StorageVec is not a variable");
                            };
                            
                            if variable.is_none() {
                                panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
                            }
                            
                            let variable = variable.unwrap();
                           
                            if !variable.borrow().is_storage {
                                panic!("StorageVec is not in storage");
                            }

                            Ok(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::from(sway::MemberAccess {
                                    expression: container_access,
                                    member: "push".into(),
                                }),
                                
                                generic_parameters: None,
                                
                                parameters: arguments.iter()
                                    .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                                    .collect::<Result<Vec<_>, _>>()?,
                            }))
                        }

                        "pop" => {
                            let (variable, container_access) = match translate_variable_access_expression(project, translated_definition, scope.clone(), &solidity_container) {
                                Ok((variable, expression)) => (Some(variable), Some(expression)),
                                Err(_) => (None, None),
                            };

                            let (Some(variable), Some(container_access)) = (variable, container_access) else {
                                panic!("StorageVec is not a variable");
                            };

                            if variable.is_none() {
                                panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
                            }
                            
                            let variable = variable.unwrap();
                            
                            if !variable.borrow().is_storage {
                                panic!("StorageVec is not in storage");
                            }

                            Ok(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::from(sway::MemberAccess {
                                    expression: container_access,
                                    member: "pop".into(),
                                }),
                                generic_parameters: None,
                                parameters: vec![],
                            }))
                        }

                        "remove" => {
                            let (variable, container_access) = match translate_variable_access_expression(project, translated_definition, scope.clone(), &solidity_container) {
                                Ok((variable, expression)) => (Some(variable), Some(expression)),
                                Err(_) => (None, None),
                            };

                            let (Some(variable), Some(container_access)) = (variable, container_access) else {
                                panic!("StorageVec is not a variable");
                            };

                            if variable.is_none() {
                                panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
                            }

                            let variable = variable.unwrap();
                            
                            if !variable.borrow().is_storage {
                                panic!("StorageVec is not in storage");
                            }

                            Ok(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::from(sway::MemberAccess {
                                    expression: container_access,
                                    member: "remove".into(),
                                }),
                                
                                generic_parameters: None,
                                
                                parameters: arguments.iter()
                                    .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                                    .collect::<Result<Vec<_>, _>>()?,
                            }))
                        }

                        _ => todo!("translate StorageVec member function call `{member}`: {} - {container:#?}", sway::TabbedDisplayer(&container))
                    }

                    ("Vec", Some(_)) => match member.name.as_str() {
                        "push" => {
                            let (variable, container_access) = match translate_variable_access_expression(project, translated_definition, scope.clone(), &solidity_container) {
                                Ok((variable, expression)) => (Some(variable), Some(expression)),
                                Err(_) => (None, None),
                            };

                            let (Some(_), Some(container_access)) = (variable, container_access) else {
                                panic!("Vec is not a variable");
                            };

                            Ok(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::from(sway::MemberAccess {
                                    expression: container_access,
                                    member: "push".into(),
                                }),
                                
                                generic_parameters: None,
                                
                                parameters: arguments.iter()
                                    .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                                    .collect::<Result<Vec<_>, _>>()?,
                            }))
                        }

                        "pop" => {
                            let (variable, container_access) = match translate_variable_access_expression(project, translated_definition, scope.clone(), &solidity_container) {
                                Ok((variable, expression)) => (Some(variable), Some(expression)),
                                Err(_) => (None, None),
                            };

                            let (Some(_), Some(container_access)) = (variable, container_access) else {
                                panic!("Vec is not a variable");
                            };

                            Ok(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::from(sway::MemberAccess {
                                    expression: container_access,
                                    member: "pop".into(),
                                }),
                                generic_parameters: None,
                                parameters: vec![],
                            }))
                        }

                        "remove" => {
                            let (variable, container_access) = match translate_variable_access_expression(project, translated_definition, scope.clone(), &solidity_container) {
                                Ok((variable, expression)) => (Some(variable), Some(expression)),
                                Err(_) => (None, None),
                            };

                            let (Some(_), Some(container_access)) = (variable, container_access) else {
                                panic!("Vec is not a variable");
                            };

                            Ok(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::from(sway::MemberAccess {
                                    expression: container_access,
                                    member: "push".into(),
                                }),
                                
                                generic_parameters: None,
                                
                                parameters: arguments.iter()
                                    .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                                    .collect::<Result<Vec<_>, _>>()?,
                            }))
                        }

                        _ => todo!("translate Vec member function call `{member}`: {} - {container:#?}", sway::TabbedDisplayer(&container))
                    }

                    _ => {
                        let mut parameters = arguments.iter()
                            .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                            .collect::<Result<Vec<_>, _>>()?;

                        let mut parameter_types = parameters.iter()
                            .map(|p| translated_definition.get_expression_type(scope.clone(), p))
                            .collect::<Result<Vec<_>, _>>()
                            .unwrap();

                        let mut using_parameters = parameters.clone();
                        using_parameters.insert(0, container.clone());

                        let mut using_parameter_types = parameter_types.clone();
                        using_parameter_types.insert(0, translated_definition.get_expression_type(scope.clone(), &container).unwrap());

                        // Check if this is a function from a using directive
                        for using_directive in translated_definition.using_directives.clone() {
                            // Make sure the type names match
                            if let Some(for_type) = using_directive.for_type.as_ref() {
                                if *for_type != type_name {
                                    continue;
                                }
                            }

                            // Look up the definition of the using directive
                            let Some(external_scope) = project.translated_definitions.iter()
                                .find(|d| {
                                    d.name == using_directive.library_name && matches!(d.kind.as_ref().unwrap(), solidity::ContractTy::Library(_))
                                })
                                .map(|d| d.toplevel_scope.clone())
                            else { continue };

                            if let Some(named_arguments) = named_arguments {
                                let mut named_parameters = vec![];
        
                                for arg in named_arguments {
                                    named_parameters.push((
                                        crate::translate_naming_convention(&arg.name.name, Case::Snake),
                                        translate_expression(project, translated_definition, scope.clone(), &arg.expr)?
                                    ));
                                }
        
                                for function in external_scope.borrow().functions.iter() {
                                    let function = function.borrow();

                                    if function.old_name != member.name {
                                        continue;
                                    }
        
                                    if function.parameters.entries.len() != named_parameters.len() {
                                        continue;
                                    }
        
                                    if function.parameters.entries.iter().all(|p| named_parameters.iter().any(|(name, _)| p.name == *name)) {
                                        using_parameters = vec![];
                                        using_parameters.insert(0, container.clone());
    
                                        using_parameter_types = vec![];
                                        using_parameter_types.insert(0, translated_definition.get_expression_type(scope.clone(), &container).unwrap());
            
                                        for parameter in function.parameters.entries.iter() {
                                            let arg = named_arguments.iter().find(|a| {
                                                let new_name = crate::translate_naming_convention(&a.name.name, Case::Snake);
                                                new_name == parameter.name
                                            }).unwrap();
            
                                            let parameter = translate_expression(project, translated_definition, scope.clone(), &arg.expr)?;
                                            let parameter_type = translated_definition.get_expression_type(scope.clone(), &parameter)?;
            
                                            using_parameters.push(parameter);
                                            using_parameter_types.push(parameter_type);
                                        }

                                        break;
                                    }
                                }
                            }
        
                            'function_lookup: for function in external_scope.borrow().functions.iter() {
                                let function = function.borrow();

                                // Ensure the function's old name matches the function call we're translating
                                if function.old_name != member.name {
                                    continue 'function_lookup;
                                }

                                // Ensure the supplied function call args match the function's parameters
                                if using_parameters.len() != function.parameters.entries.len() {
                                    continue 'function_lookup;
                                }
    
                                'value_type_check: for (i, value_type_name) in using_parameter_types.iter().enumerate() {
                                    let Some(parameter_type_name) = function.parameters.entries[i].type_name.as_ref() else { continue };

                                    //
                                    // If `parameter_type_name` is `Identity`, but `container` is an abi cast expression,
                                    // then we need to de-cast it, so `container` turns into the 2nd parameter of the abi cast,
                                    // and `value_type_name` turns into `Identity`.
                                    //

                                    if let sway::TypeName::Identifier { name: parameter_type_name, generic_parameters: None } = parameter_type_name {
                                        if parameter_type_name == "Identity" {
                                            if let sway::Expression::FunctionCall(function_call) = using_parameters[i].clone() {
                                                if let sway::Expression::Identifier(function_name) = &function_call.function {
                                                    if function_name == "abi" {
                                                        using_parameters[i] = function_call.parameters[1].clone();
                                                        continue 'value_type_check;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    
                                    if !value_type_name.is_compatible_with(parameter_type_name) {
                                        continue 'function_lookup;
                                    }
                                }
                                
                                *translated_definition.function_call_counts.entry(function.new_name.clone()).or_insert(0) += 1;
                                translated_definition.functions_called
                                    .entry(translated_definition.current_functions.last().cloned().unwrap())
                                    .or_insert_with(|| vec![])
                                    .push(function.new_name.clone());

                                return Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier(function.new_name.clone()),
                                    generic_parameters: None,
                                    parameters: using_parameters,
                                }));
                            }
                        }

                        // Check if this is a function from an ABI
                        if let Some(external_scope) = project.find_definition_with_abi(name).map(|d| d.toplevel_scope.clone()) {
                            if let Some(named_arguments) = named_arguments {
                                let mut named_parameters = vec![];
        
                                for arg in named_arguments {
                                    named_parameters.push((
                                        crate::translate_naming_convention(&arg.name.name, Case::Snake),
                                        translate_expression(project, translated_definition, scope.clone(), &arg.expr)?
                                    ));
                                }
        
                                for function in external_scope.borrow().functions.iter() {
                                    let function = function.borrow();
        
                                    if function.old_name != member.name {
                                        continue;
                                    }
        
                                    if function.parameters.entries.len() != named_parameters.len() {
                                        continue;
                                    }
        
                                    if function.parameters.entries.iter().all(|p| named_parameters.iter().any(|(name, _)| p.name == *name)) {
                                        parameters = vec![];
                                        parameter_types = vec![];
            
                                        for parameter in function.parameters.entries.iter() {
                                            let arg = named_arguments.iter().find(|a| {
                                                let new_name = crate::translate_naming_convention(&a.name.name, Case::Snake);
                                                new_name == parameter.name
                                            }).unwrap();
            
                                            let parameter = translate_expression(project, translated_definition, scope.clone(), &arg.expr)?;
                                            let parameter_type = translated_definition.get_expression_type(scope.clone(), &parameter)?;
            
                                            parameters.push(parameter);
                                            parameter_types.push(parameter_type);
                                        }
    
                                        break;
                                    }
                                }
                            }
                            
                            'function_lookup: for function in external_scope.borrow().functions.iter() {
                                let function = function.borrow();

                                // Ensure the function's old name matches the function call we're translating
                                if function.old_name != member.name {
                                    continue 'function_lookup;
                                }

                                // Ensure the supplied function call args match the function's parameters
                                if parameters.len() != function.parameters.entries.len() {
                                    continue 'function_lookup;
                                }
    
                                'value_type_check: for (i, value_type_name) in parameter_types.iter().enumerate() {
                                    let Some(parameter_type_name) = function.parameters.entries[i].type_name.as_ref() else { continue };

                                    //
                                    // If `parameter_type_name` is `Identity`, but `container` is an abi cast expression,
                                    // then we need to de-cast it, so `container` turns into the 2nd parameter of the abi cast,
                                    // and `value_type_name` turns into `Identity`.
                                    //

                                    if let sway::TypeName::Identifier { name: parameter_type_name, generic_parameters: None } = parameter_type_name {
                                        if parameter_type_name == "Identity" {
                                            if let sway::Expression::FunctionCall(function_call) = parameters[i].clone() {
                                                if let sway::Expression::Identifier(function_name) = &function_call.function {
                                                    if function_name == "abi" {
                                                        parameters[i] = function_call.parameters[1].clone();
                                                        continue 'value_type_check;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    
                                    if !value_type_name.is_compatible_with(parameter_type_name) {
                                        continue 'function_lookup;
                                    }
                                }
                                
                                *translated_definition.function_call_counts.entry(function.new_name.clone()).or_insert(0) += 1;
                                translated_definition.functions_called
                                    .entry(translated_definition.current_functions.last().cloned().unwrap())
                                    .or_insert_with(|| vec![])
                                    .push(function.new_name.clone());
                                
                                return Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: container,
                                        member: function.new_name.clone(),
                                    }),
                                    generic_parameters: None,
                                    parameters,
                                }));
                            }
                        }

                        todo!(
                            "{}translate {name} member function call: {}.{member}({}) - {container:#?}",
                            match project.loc_to_line_and_column(&translated_definition.path, &function.loc()) {
                                Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                                None => format!("{} - ", translated_definition.path.to_string_lossy()),
                            },
                            sway::TabbedDisplayer(&container), parameter_types.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "),
                        )
                    }
                }

                sway::TypeName::Array { .. } => todo!("translate array member function call: {} - {container:#?}", sway::TabbedDisplayer(&container)),
                sway::TypeName::Tuple { .. } => todo!("translate tuple member function call: {} - {container:#?}", sway::TabbedDisplayer(&container)),
                sway::TypeName::StringSlice => todo!("translate string slice member function call: {} - {container:#?}", sway::TabbedDisplayer(&container)),
                sway::TypeName::StringArray { .. } => todo!("translate string array member function call: {} - {container:#?}", sway::TabbedDisplayer(&container)),
            }
        }

        solidity::Expression::FunctionCall(_, function, args) => {
            // timelock.executeTransaction.value(proposal.values[i])
            match function.as_ref() {
                solidity::Expression::MemberAccess(_, container, member) => {
                    // timelock.executeTransaction
                    // .value
                    
                    let mut coins = None;
                    let mut gas = None;

                    match member.name.as_str() {
                        "value" if args.len() == 1 => coins = Some(translate_expression(project, translated_definition, scope.clone(), &args[0])?),
                        "gas" if args.len() == 1 => gas = Some(translate_expression(project, translated_definition, scope.clone(), &args[0])?),
                        _ => todo!("translate member function call: {member}"),
                    };

                    match container.as_ref() {
                        solidity::Expression::MemberAccess(_, container, member) => {
                            let variable = translate_variable_access_expression(project, translated_definition, scope.clone(), container).ok().map(|(v, _)| v);
                            let mut container = translate_expression(project, translated_definition, scope.clone(), container)?;
                            let type_name = translated_definition.get_expression_type(scope.clone(), &container)?;
            
                            match type_name {
                                sway::TypeName::Undefined => panic!("Undefined type name"),
                                
                                sway::TypeName::Identifier { name, .. } => match name.as_str() {
                                    "Identity" => match member.name.as_str() {
                                        "call" => {
                                            if arguments.len() != 1 {
                                                panic!("Malformed `address.call` call, expected 1 argument, found {}", arguments.len());
                                            }
            
                                            let payload = translate_expression(project, translated_definition, scope.clone(), &arguments[0])?;
                                            translate_address_call_expression(project, translated_definition, scope.clone(), payload, coins, None, gas)
                                        }
            
                                        _ => {
                                            let mut name = name.clone();
                                            let external_function_new_name = crate::translate_naming_convention(member.name.as_str(), Case::Snake);
                
                                            // Check if expression is a variable that had an ABI type
                                            if let Some(variable) = variable.as_ref() {
                                                if variable.is_none() {
                                                    panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
                                                }
                                                let variable = variable.as_ref().unwrap();
                                                let variable = variable.borrow();
                
                                                if let Some(abi_type_name) = variable.abi_type_name.as_ref() {
                                                    let abi_type_name = abi_type_name.to_string();
                
                                                    // Ensure the ABI is added to the current definition
                                                    if let Some(external_definition) = project.find_definition_with_abi(abi_type_name.as_str()) {
                                                        if let Some(abi) = external_definition.abi.as_ref() {
                                                            if abi.name == abi_type_name && !translated_definition.abis.iter().any(|a| a.name == abi.name) {
                                                                translated_definition.abis.push(abi.clone());
                                                            }
                                                        }
                                                    }
                                    
                                                    // Turn the expression into an ABI cast
                                                    container = sway::Expression::from(sway::FunctionCall {
                                                        function: sway::Expression::Identifier("abi".into()),
                                                        generic_parameters: None,
                                                        parameters: vec![
                                                            sway::Expression::Identifier(abi_type_name.clone()),
                
                                                            // x.as_contract_id().unwrap().into()
                                                            sway::Expression::from(sway::FunctionCall {
                                                                function: sway::Expression::from(sway::MemberAccess {
                                                                    expression: sway::Expression::from(sway::FunctionCall {
                                                                        function: sway::Expression::from(sway::MemberAccess {
                                                                            expression: sway::Expression::from(sway::FunctionCall {
                                                                                function: sway::Expression::from(sway::MemberAccess {
                                                                                    expression: container,
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
                                                                    member: "into".into(),
                                                                }),
                                                                generic_parameters: None,
                                                                parameters: vec![],
                                                            }),
                                                        ],
                                                    });
                
                                                    name = abi_type_name.to_string();
                                                }
                                            }
                    
                                            // Check to see if the type is located in an external ABI
                                            if let Some(external_definition) = project.find_definition_with_abi(name.as_str()) {
                                                let external_abi = external_definition.abi.as_ref().unwrap();
            
                                                if external_abi.functions.iter().any(|f| f.name == external_function_new_name) {
                                                    // Ensure the ABI is added to the current definition
                                                    if !translated_definition.abis.iter().any(|a| a.name == external_abi.name) {
                                                        translated_definition.abis.push(external_abi.clone());
                                                    }
                                                    
                                                    let mut fields = vec![];
            
                                                    if let Some(coins) = coins {
                                                        fields.push(sway::ConstructorField {
                                                            name: "coins".into(),
                                                            value: coins,
                                                        });
                                                    }
                                                    
                                                    if let Some(gas) = gas {
                                                        fields.push(sway::ConstructorField {
                                                            name: "gas".into(),
                                                            value: gas,
                                                        });
                                                    }
            
                                                    return Ok(sway::Expression::from(sway::FunctionCallBlock {
                                                        function: sway::Expression::from(sway::MemberAccess {
                                                            expression: container,
                                                            member: external_function_new_name,
                                                        }),
                                                        generic_parameters: None,
                                                        fields,
                                                        parameters: arguments.iter()
                                                            .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                                                            .collect::<Result<Vec<_>, _>>()?,
                                                    }));
                                                }
                                            }
            
                                            todo!("translate Identity member function call block `{member}` : {} - {container:#?}", sway::TabbedDisplayer(&container))
                                        }
                                    }
            
                                    _ => todo!("translate {name} member function call block: {} - {container:#?}", sway::TabbedDisplayer(&container))
                                }
            
                                sway::TypeName::Array { .. } => todo!(),
                                sway::TypeName::Tuple { .. } => todo!(),
                                sway::TypeName::StringSlice => todo!(),
                                sway::TypeName::StringArray { .. } => todo!(),
                            }
                        }
            
                        _ => todo!("translate member function call: {member}")
                    }
                }
                _ => todo!("translate function call: {function}")
            }
        }

        // timelock.executeTransaction{value : proposal.values[i]}
        solidity::Expression::FunctionCallBlock(_, function, block) => match function.as_ref() {
            solidity::Expression::MemberAccess(_, container, member) => {
                let variable = translate_variable_access_expression(project, translated_definition, scope.clone(), container).ok().map(|(v, _)| v);
                let mut container = translate_expression(project, translated_definition, scope.clone(), container)?;
                let type_name = translated_definition.get_expression_type(scope.clone(), &container)?;

                let solidity::Statement::Args(_, block_args) = block.as_ref() else {
                    panic!("Malformed `address.call` call, expected args block, found: {block:#?}");
                };

                let mut coins = None;
                let mut gas = None;

                for block_arg in block_args.iter() {
                    match block_arg.name.name.as_str() {
                        "value" => coins = Some(translate_expression(project, translated_definition, scope.clone(), &block_arg.expr)?),
                        "gas" => gas = Some(translate_expression(project, translated_definition, scope.clone(), &block_arg.expr)?),
                        arg => todo!("address.transfer block arg: {arg}"),
                    }
                }

                match type_name {
                    sway::TypeName::Undefined => panic!("Undefined type name"),
                    
                    sway::TypeName::Identifier { name, .. } => match name.as_str() {
                        "Identity" => match member.name.as_str() {
                            "call" => {
                                if arguments.len() != 1 {
                                    panic!("Malformed `address.call` call, expected 1 argument, found {}", arguments.len());
                                }

                                let payload = translate_expression(project, translated_definition, scope.clone(), &arguments[0])?;
                                translate_address_call_expression(project, translated_definition, scope.clone(), payload, coins, None, gas)
                            }

                            _ => {
                                let mut name = name.clone();
                                let external_function_new_name = crate::translate_naming_convention(member.name.as_str(), Case::Snake);
    
                                // Check if expression is a variable that had an ABI type
                                if let Some(variable) = variable.as_ref() {
                                    if variable.is_none() {
                                        panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
                                    }
                                    let variable = variable.as_ref().unwrap();
                                    let variable = variable.borrow();
    
                                    if let Some(abi_type_name) = variable.abi_type_name.as_ref() {
                                        let abi_type_name = abi_type_name.to_string();
    
                                        // Ensure the ABI is added to the current definition
                                        if let Some(external_definition) = project.find_definition_with_abi(abi_type_name.as_str()) {
                                            if let Some(abi) = external_definition.abi.as_ref() {
                                                if abi.name == abi_type_name && !translated_definition.abis.iter().any(|a| a.name == abi.name) {
                                                    translated_definition.abis.push(abi.clone());
                                                }
                                            }
                                        }
                        
                                        // Turn the expression into an ABI cast
                                        container = sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::Identifier("abi".into()),
                                            generic_parameters: None,
                                            parameters: vec![
                                                sway::Expression::Identifier(abi_type_name.clone()),
    
                                                // x.as_contract_id().unwrap().into()
                                                sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::from(sway::MemberAccess {
                                                        expression: sway::Expression::from(sway::FunctionCall {
                                                            function: sway::Expression::from(sway::MemberAccess {
                                                                expression: sway::Expression::from(sway::FunctionCall {
                                                                    function: sway::Expression::from(sway::MemberAccess {
                                                                        expression: container,
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
                                                        member: "into".into(),
                                                    }),
                                                    generic_parameters: None,
                                                    parameters: vec![],
                                                }),
                                            ],
                                        });
    
                                        name = abi_type_name.to_string();
                                    }
                                }
        
                                // Check to see if the type is located in an external ABI
                                if let Some(external_definition) = project.find_definition_with_abi(name.as_str()) {
                                    let external_abi = external_definition.abi.as_ref().unwrap();

                                    if external_abi.functions.iter().any(|f| f.name == external_function_new_name) {
                                        // Ensure the ABI is added to the current definition
                                        if !translated_definition.abis.iter().any(|a| a.name == external_abi.name) {
                                            translated_definition.abis.push(external_abi.clone());
                                        }
                                        
                                        let mut fields = vec![];

                                        if let Some(coins) = coins {
                                            fields.push(sway::ConstructorField {
                                                name: "coins".into(),
                                                value: coins,
                                            });
                                        }
                                        
                                        if let Some(gas) = gas {
                                            fields.push(sway::ConstructorField {
                                                name: "gas".into(),
                                                value: gas,
                                            });
                                        }

                                        return Ok(sway::Expression::from(sway::FunctionCallBlock {
                                            function: sway::Expression::from(sway::MemberAccess {
                                                expression: container,
                                                member: external_function_new_name,
                                            }),
                                            generic_parameters: None,
                                            fields,
                                            parameters: arguments.iter()
                                                .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                                                .collect::<Result<Vec<_>, _>>()?,
                                        }));
                                    }
                                }

                                todo!("translate Identity member function call block `{member}{}`: {} - {container:#?}", block.to_string(), sway::TabbedDisplayer(&container))
                            }
                        }

                        _ => todo!("translate {name} member function call block: {} - {container:#?}", sway::TabbedDisplayer(&container))
                    }

                    sway::TypeName::Array { .. } => todo!(),
                    sway::TypeName::Tuple { .. } => todo!(),
                    sway::TypeName::StringSlice => todo!(),
                    sway::TypeName::StringArray { .. } => todo!(),
                }
            }

            _ => todo!("translate function call block expression: {expression} - {expression:#?}")
        }

        _ => todo!("translate function call expression: {expression} - {expression:#?}"),
    }
}

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

#[inline]
pub fn translate_function_call_block_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    function: &solidity::Expression,
    block: &solidity::Statement,
) -> Result<sway::Expression, Error> {
    if block.is_empty() {
        return translate_expression(project, translated_definition, scope, function);
    }
    // println!(
    //     "Translating expression: {function}; from {}",
    //     match project.loc_to_line_and_column(&translated_definition.path, &block.loc()) {
    //         Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
    //         None => format!("{} - ", translated_definition.path.to_string_lossy()),
    //     },
    // );
    todo!("translate function call block expression")
}

#[inline]
pub fn translate_unary_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    operator: &str,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    let expression = translate_expression(project, translated_definition, scope.clone(), expression)?;

    // NOTE: Sway does not have a negate operator, so we need to make sure to use the correct translation
    if operator == "-" {
        let type_name = translated_definition.get_expression_type(scope, &expression)?;

        match &type_name {
            sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                ("I8" | "I16" | "I32" | "I64" | "I128" | "I256", None) => return Ok(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression,
                        member: "wrapping_neg".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![],
                })),

                ("u8" | "u16" | "u32" | "u64" | "u256", None) => { 
                    let bits: usize = name.trim_start_matches('u').parse().unwrap();
                    
                    translated_definition.ensure_dependency_declared(
                        "sway_libs = { git = \"https://github.com/FuelLabs/sway-libs\", tag = \"v0.24.0\" }"
                    );
                    translated_definition.ensure_use_declared(format!("sway_libs::signed_integers::i{bits}::*").as_str());

                    return Ok(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::from(sway::MemberAccess {
                            expression: sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::from(sway::MemberAccess {
                                    expression: sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::Identifier(format!("I{bits}::from_uint")),
                                        generic_parameters: None,
                                        parameters: vec![
                                            expression.clone()
                                        ],
                                    }),
                                    member: "wrapping_neg".into(),
                                }),
                                generic_parameters: None,
                                parameters: vec![],
                            }),
                            member: "underlying".into(),
                        }),
                        generic_parameters: None,
                        parameters: vec![],
                    }))
                },

                _ => {
                    // HACK: allow literals to be negated
                    if let sway::Expression::Literal(sway::Literal::DecInt(_, _) | sway::Literal::HexInt(_, _)) = &expression {
                        return Ok(sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::from(sway::MemberAccess {
                                expression,
                                member: "wrapping_neg".into(),
                            }),
                            generic_parameters: None,
                            parameters: vec![],
                        }));
                    }

                    panic!("Unhandled {type_name} negate operator translation")
                }
            }

            _ => panic!("Unhandled {type_name} negate operator translation"),
        }
    }

    Ok(sway::Expression::from(sway::UnaryExpression {
        operator: operator.into(),
        expression,
    }))
}

#[inline]
pub fn translate_power_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    lhs: &solidity::Expression,
    rhs: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    // lhs ** rhs => lhs.pow(rhs)

    // Ensure std::math::Power is imported for the pow function
    translated_definition.ensure_use_declared("std::math::Power");

    let lhs = translate_expression(project, translated_definition, scope.clone(), lhs)?;
    let rhs = translate_expression(project, translated_definition, scope.clone(), rhs)?;

    Ok(sway::Expression::from(sway::FunctionCall {
        function: sway::Expression::from(sway::MemberAccess {
            expression: lhs,
            member: "pow".into(),
        }),
        generic_parameters: None,
        parameters: vec![
            rhs,
        ],
    }))
}

#[inline]
pub fn translate_binary_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    operator: &str,
    lhs: &solidity::Expression,
    rhs: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    // Hack: x.code.length == 0 => x.as_contract_id().is_none()
    if let solidity::Expression::MemberAccess(_, x, member2) = lhs {
        if let solidity::Expression::MemberAccess(_, x, member1) = x.as_ref() {
            if member1.name == "code" && member2.name == "length" {
                let expression = translate_expression(project, translated_definition, scope.clone(), x)?;
                let type_name = translated_definition.get_expression_type(scope.clone(), &expression)?;

                if let sway::TypeName::Identifier { name, generic_parameters: None } = type_name {
                    if name == "Identity" {
                        if let solidity::Expression::NumberLiteral(_, value, _, _) = rhs {
                            if value == "0" {
                                return Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::from(sway::MemberAccess {
                                                expression,
                                                member: "as_contract_id".into(),
                                            }),
                                            generic_parameters: None,
                                            parameters: vec![],
                                        }),
                                        member: "is_none".into(),
                                    }),
                                    generic_parameters: None,
                                    parameters: vec![],
                                }));
                            }
                        }
                    }
                }
            }
        }
    }

    // authority == DSAuthority(address(0))
    // storage.authority.read() == Identity::Address(Address::from(ZERO_B256))
    let lhs = translate_expression(project, translated_definition, scope.clone(), lhs)?;
    let mut new_name = None;

    if let sway::Expression::FunctionCall(expr) = &lhs {
        if let sway::Expression::MemberAccess(expr) = &expr.function {
            if expr.member == "read" {
                if let sway::Expression::MemberAccess(expr) = &expr.expression {
                    if let sway::Expression::Identifier(ident) = &expr.expression {
                        if ident == "storage" {
                            new_name = Some(expr.member.clone());
                        }
                    }
                }
            }
        }
    }

    if new_name.is_none() {
        return Ok(sway::Expression::from(sway::BinaryExpression {
            operator: operator.into(),
            lhs,
            rhs: translate_expression(project, translated_definition, scope.clone(), rhs)?,
        }))
    }

    let lhs_type_name = scope.borrow()
    .get_variable_from_new_name(new_name.as_ref().unwrap().as_str())
    .and_then(|v| v.borrow().abi_type_name.clone());

    if let Some(lhs_type_name) = lhs_type_name {
        if let solidity::Expression::FunctionCall(_, rhs_expr, rhs_exprs) = rhs {
            if let solidity::Expression::Variable(solidity::Identifier { name: rhs_name, .. }) = rhs_expr.as_ref() {
                if *rhs_name == lhs_type_name.to_string() {
                    return Ok(sway::Expression::from(sway::BinaryExpression {
                        operator: operator.into(),
                        lhs,
                        rhs: translate_expression(project, translated_definition, scope.clone(), &rhs_exprs[0])?,
                    }));
                }
            }
        }
    }

    Ok(sway::Expression::from(sway::BinaryExpression {
        operator: operator.into(),
        lhs,
        rhs: translate_expression(project, translated_definition, scope.clone(), rhs)?,
    }))
}


pub fn translate_variable_access_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
) -> Result<(Option<Rc<RefCell<TranslatedVariable>>>, sway::Expression), Error> {
    match expression {
        solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
            let Some(variable) = scope.borrow().get_variable_from_old_name(name) else {
                return Err(Error::Wrapped(Box::new(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!(
                        "{}error: Variable not found in scope: \"{name}\"",
                        match project.loc_to_line_and_column(&translated_definition.path, &expression.loc()) {
                            Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                            None => format!("{} - ", translated_definition.path.to_string_lossy()),
                        }
                    ),
                ))));
            };

            let variable_name = variable.borrow().new_name.clone();
            let is_storage = variable.borrow().is_storage;

            Ok((
                Some(variable),
                if is_storage {
                    sway::Expression::from(sway::MemberAccess {
                        expression: sway::Expression::Identifier("storage".into()),
                        member: variable_name,
                    })
                } else {
                    sway::Expression::Identifier(variable_name)
                }
            ))
        }

        solidity::Expression::ArraySubscript(_, expression, Some(index)) => {
            let index = translate_expression(project, translated_definition, scope.clone(), index.as_ref())?;
            let (variable, expression) = translate_variable_access_expression(project, translated_definition, scope.clone(), expression)?;
            if variable.is_none() {
                return Ok((None, sway::Expression::from(sway::ArrayAccess {
                    expression,
                    index,
                })));
            }
            let variable = variable.unwrap();
            let is_storage = variable.borrow().is_storage;

            Ok((
                Some(variable),
                if is_storage {
                    // sway::Expression::from(sway::FunctionCall {
                    //     function: sway::Expression::from(sway::MemberAccess {
                    //         expression: sway::Expression::from(sway::FunctionCall {
                    //             function: sway::Expression::from(sway::MemberAccess {
                    //                 expression,
                    //                 member: "get".into(),
                    //             }),
                    //             generic_parameters: None,
                    //             parameters: vec![
                    //                 index.clone(),
                    //             ],
                    //         }),
                    //         member: "unwrap".into(),
                    //     }),
                    //     generic_parameters: None,
                    //     parameters: vec![],
                    // })
                    sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::from(sway::MemberAccess {
                            expression,
                            member: "get".into(),
                        }),
                        generic_parameters: None,
                        parameters: vec![index],
                    })
                } else {
                    sway::Expression::from(sway::ArrayAccess {
                        expression,
                        index,
                    })
                }
            ))
        }
        
        solidity::Expression::MemberAccess(_, container, member) => {
            if let solidity::Expression::Variable(solidity::Identifier { name, .. }) = container.as_ref() {
                for external_definition in project.translated_definitions.iter() {
                    
                    if let Some(solidity::ContractTy::Library(_)) = external_definition.kind.as_ref()  {
                        if external_definition.name == *name {
                            let member_name = crate::translate_naming_convention(&member.name, Case::Snake);
                            let new_name = format!("{}_{}", crate::translate_naming_convention(&external_definition.name, Case::Snake), member_name);
                            
                            if translated_definition.toplevel_scope.borrow().find_function(|f| f.borrow().new_name == new_name).is_none() {
                                // Get the scope entry for the library function
                                let Some(scope_entry) = external_definition.toplevel_scope.borrow().find_function(|f| f.borrow().new_name == new_name) else {
                                    panic!("Failed to find function in scope: \"{}\"", new_name);
                                };

                                // Add the function to the current definition's toplevel scope
                                if !translated_definition.toplevel_scope.borrow().functions.iter().any(|f| {
                                    f.borrow().old_name == scope_entry.borrow().old_name
                                    && f.borrow().parameters == scope_entry.borrow().parameters
                                    && f.borrow().return_type == scope_entry.borrow().return_type
                                }) {
                                    translated_definition.toplevel_scope.borrow_mut().functions.push(Rc::new(RefCell::new(scope_entry.borrow().clone())));
                                }

                                // Add the function name to the current definition's function name list
                                *translated_definition.function_name_counts.entry(new_name.clone()).or_insert(0) += 1;

                                let function = external_definition.functions.iter().find(|f| f.name == new_name).unwrap();

                                // Add the function definition to the current definition
                                if !translated_definition.functions.contains(function) {
                                    translated_definition.functions.push(function.clone());
                                }

                                // Add the function call count from the library definition to the current definition
                                translated_definition.function_call_counts.insert(
                                    function.name.clone(),
                                    if let Some(function_call_count) = external_definition.function_call_counts.get(&function.name) {
                                        *function_call_count
                                    } else {
                                        0
                                    }
                                );

                                // Add the functions called from the library definition to the current definition
                                for (lib_calling_fn, lib_called_fns) in external_definition.functions_called.iter() {
                                    let called_functions = translated_definition.functions_called.entry(lib_calling_fn.clone()).or_default();

                                    for lib_called_fn in lib_called_fns.iter() {
                                        if !called_functions.contains(lib_called_fn) {
                                            called_functions.push(lib_called_fn.clone());
                                        }
                                    }
                                }
                                
                            }
                            return Ok((None, sway::Expression::Identifier(new_name)));
                        }
                    }
                }
            }
            
            let translated_container = translate_expression(project, translated_definition, scope.clone(), container)?;
        
            let container_type_name = translated_definition.get_expression_type(scope.clone(), &translated_container)?;
            let container_type_name_string = container_type_name.to_string();
            let (variable, _) = translate_variable_access_expression(project, translated_definition, scope.clone(), container)?;
        
            // Check if container is a struct
            if let Some(struct_definition) = translated_definition.structs.iter().find(|s| s.name == container_type_name_string) {
                let field_name = crate::translate_naming_convention(member.name.as_str(), Case::Snake);
        
                if struct_definition.fields.iter().any(|f| f.name == field_name) {
                    return Ok((
                        variable,
                        sway::Expression::from(sway::MemberAccess {
                            expression: translated_container,
                            member: field_name,
                        })
                    ))
                }
            }
        
            todo!("translate variable {container_type_name_string} member access expression: {expression} - {expression:#?}")
        }

        solidity::Expression::FunctionCall(_, function, arguments) => {
            let arguments = arguments.iter()
                .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                .collect::<Result<Vec<_>, _>>()?;
            match translate_variable_access_expression(project, translated_definition, scope.clone(), function) {   
                Ok((variable, expression)) => Ok((
                    variable,
                    sway::Expression::from(sway::FunctionCall {
                        function: expression,
                        generic_parameters: None,
                        parameters: arguments,
                    })
                )),
                Err(_) => {
                    Ok((None, translate_expression(project, translated_definition, scope, expression)?))
                }
            }
        }

        solidity::Expression::Type(_, _) => Err(Error::Wrapped(Box::new(
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!("type expression as variable access expression: {expression} - {expression:#?}")
            )
        ))),

        _ => todo!("translate variable access expression: {expression} - {expression:#?}"),
    }
}

#[inline]
pub fn translate_conditional_operator_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    condition: &solidity::Expression,
    then_value: &solidity::Expression,
    else_value: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    // if condition { then_value } else { else_value }
    Ok(sway::Expression::from(sway::If {
        condition: Some(translate_expression(project, translated_definition, scope.clone(), condition)?),
        then_body: sway::Block {
            statements: vec![],
            final_expr: Some(
                translate_expression(project, translated_definition, scope.clone(), then_value)?
            ),
        },
        else_if: Some(Box::new(sway::If {
            condition: None,
            then_body: sway::Block {
                statements: vec![],
                final_expr: Some(
                    translate_expression(project, translated_definition, scope.clone(), else_value)?
                ),
            },
            else_if: None,
        })),
    }))
}

#[inline]
pub fn create_assignment_expression(
    _project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    operator: &str,
    expression: &sway::Expression,
    variable: Rc<RefCell<TranslatedVariable>>,
    rhs: &sway::Expression,
    rhs_type_name: &sway::TypeName,
) -> Result<sway::Expression, Error> {
    // Generate a unique name for our variable
    let variable_name = scope.borrow_mut().generate_unique_variable_name("x");
    
    let mut variable = variable.borrow_mut();
    variable.mutation_count += 1;

    if variable.is_storage {
        let expression = match &variable.type_name {
            sway::TypeName::Identifier { name, .. } if name == "StorageVec" => {
                sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression: expression.clone(),
                        member: "unwrap".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![]
                })
            }

            _ => expression.clone(),
        };
        
        Ok(sway::Expression::from(sway::FunctionCall {
            function: sway::Expression::from(sway::MemberAccess {
                expression: expression.clone(),
                member: match (&variable.type_name, &rhs_type_name) {
                    (
                        sway::TypeName::Identifier { name: lhs_name, .. },
                        sway::TypeName::StringSlice
                    ) if lhs_name == "StorageString" => {
                        "write_slice".into()
                    }

                    _ => "write".into(),
                },
            }),
            generic_parameters: None,
            parameters: vec![
                match operator {
                    "=" => match (&variable.type_name, &rhs_type_name) {
                        (
                            sway::TypeName::Identifier { name: lhs_name, .. },
                            sway::TypeName::StringSlice
                        ) if lhs_name == "StorageString" => {
                            // Ensure `std::string::*` is imported
                            translated_definition.ensure_use_declared("std::string::*");

                            sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier("String::from_ascii_str".into()),
                                generic_parameters: None,
                                parameters: vec![rhs.clone()],
                            })
                        }

                        _ => rhs.clone(),
                    },

                    _ => {
                        variable.read_count += 1;

                        sway::Expression::from(sway::BinaryExpression {
                            operator: operator.trim_end_matches('=').into(),

                            lhs: sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::from(sway::MemberAccess {
                                    expression: expression.clone(),
                                    member: "read".into(),
                                }),
                                generic_parameters: None,
                                parameters: vec![],
                            }),

                            rhs: rhs.clone(),
                        })
                    }
                },
            ],
        }))
    } else {
        match &variable.type_name {
            sway::TypeName::Identifier { name, .. } if name == "Vec" => match &expression {
                sway::Expression::ArrayAccess(array_access) => Ok(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression: array_access.expression.clone(),
                        member: "set".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![
                        array_access.index.clone(),
                        match operator {
                            "=" => rhs.clone(),
                            
                            _ => {
                                variable.read_count += 1;
                                
                                sway::Expression::from(sway::BinaryExpression {
                                    operator: operator.trim_end_matches('=').into(),
    
                                    lhs: sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::from(sway::MemberAccess {
                                            expression: sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::from(sway::MemberAccess {
                                                    expression: array_access.expression.clone(),
                                                    member: "get".into(),
                                                }),
                                                generic_parameters: None,
                                                parameters: vec![
                                                    array_access.index.clone(),
                                                ],
                                            }),
                                            member: "unwrap".into(),
                                        }),
                                        generic_parameters: None,
                                        parameters: vec![],
                                    }),
    
                                    rhs: rhs.clone(),
                                })
                            }
                        }
                    ],
                })),

                sway::Expression::MemberAccess(member_access) => match &member_access.expression {
                    sway::Expression::ArrayAccess(array_access) => {
                        // x[i].member = value => {
                        //     let mut x = expr.get(i).unwrap();
                        //     x.member = value;
                        //     expr.set(i, a);
                        // }

                        Ok(sway::Expression::from(sway::Block {
                            statements: vec![
                                // let mut x = expr.get(i).unwrap();
                                sway::Statement::from(sway::Let {
                                    pattern: sway::LetPattern::from(sway::LetIdentifier {
                                        is_mutable: true,
                                        name: variable_name.clone(),
                                    }),
                                    type_name: None,
                                    value: sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::from(sway::MemberAccess {
                                            expression: sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::from(sway::MemberAccess {
                                                    expression: array_access.expression.clone(),
                                                    member: "get".into(),
                                                }),
                                                generic_parameters: None,
                                                parameters: vec![
                                                    array_access.index.clone(),
                                                ],
                                            }),
                                            member: "unwrap".into(),
                                        }),
                                        generic_parameters: None,
                                        parameters: vec![],
                                    }),
                                }),
                                // x.member = value;
                                sway::Statement::from(sway::Expression::from(sway::BinaryExpression {
                                    operator: "=".into(),
                                    lhs: sway::Expression::from(sway::MemberAccess {
                                        expression: sway::Expression::Identifier(variable_name.clone()),
                                        member: member_access.member.clone(),
                                    }),
                                    rhs: rhs.clone(),
                                })),
                                // expr.set(i, a);
                                sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: array_access.expression.clone(),
                                        member: "set".into(),
                                    }),
                                    generic_parameters: None,
                                    parameters: vec![
                                        sway::Expression::Identifier(variable_name.clone()),
                                    ],
                                })),
                            ],
                            final_expr: None,
                        }))
                    }

                    _ => todo!("translation assignment expression: {}", sway::TabbedDisplayer(expression)),
                }
                
                sway::Expression::Identifier(ident) if operator == "=" => {
                    Ok(sway::Expression::from(sway::BinaryExpression {
                        operator: "=".into(),
                        lhs: expression.clone(),
                        rhs: sway::Expression::Identifier(ident.clone())
                    }))
                }

                _ => todo!("translation assignment expression: {}", sway::TabbedDisplayer(expression)),
            }

            _ => match operator {
                "&=" | "|=" | "^=" => {
                    //
                    // NOTE:
                    // Sway doesn't have these operators, so we have to implement them manually.
                    //
                    
                    variable.read_count += 1;
                    
                    Ok(sway::Expression::from(sway::BinaryExpression {
                        operator: "=".into(),
                        lhs: expression.clone(),
                        rhs: sway::Expression::from(sway::BinaryExpression {
                            operator: operator.trim_end_matches('=').into(),
                            lhs: expression.clone(),
                            rhs: rhs.clone(),
                        }),
                    }))
                }

                _ => Ok(sway::Expression::from(sway::BinaryExpression {
                    operator: operator.into(),
                    lhs: expression.clone(),
                    rhs: rhs.clone(),
                })),
            },
        }
    }
}

#[inline]
pub fn translate_assignment_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    operator: &str,
    lhs: &solidity::Expression,
    rhs: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    // println!(
    //     "Translating assignment expression: {lhs} {operator} {rhs}; from {}",
    //     match project.loc_to_line_and_column(&translated_definition.path, &lhs.loc()) {
    //         Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
    //         None => format!("{} - ", translated_definition.path.to_string_lossy()),
    //     },
    // );
    
    let rhs = match operator {
        "=" => {
            // println!("taking translate_pre_or_post_operator_value_expression path...");
            translate_pre_or_post_operator_value_expression(project, translated_definition, scope.clone(), rhs)?
        }
        _ => {
            // println!("taking translate_expression path...");
            translate_expression(project, translated_definition, scope.clone(), rhs)?
        }
    };
    
    let rhs_type_name = translated_definition.get_expression_type(scope.clone(), &rhs)?;
    
    let (variable, expression) = translate_variable_access_expression(project, translated_definition, scope.clone(), lhs)?;
    if variable.is_none() {
        match &expression {
            sway::Expression::MemberAccess(member_access) => {
                let x = translated_definition.get_expression_type(scope, &member_access.expression)?;
                match x {
                    sway::TypeName::Identifier{ name, .. } => {
                        if let Some(struct_definition) = translated_definition.structs.iter().find(|s| s.name == name) {
                            if struct_definition.fields.iter().any(|f| f.name == member_access.member) {
                               return Ok(expression);
                            }
                        }
                    }

                    _ => {}
                }
            }

            _ => {}
        }
        panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
    }
    let variable = variable.unwrap();

    create_assignment_expression(project, translated_definition, scope.clone(), operator, &expression, variable, &rhs, &rhs_type_name)
}

#[inline]
pub fn translate_pre_or_post_operator_value_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    match expression {
        solidity::Expression::PreIncrement(loc, x) => translate_pre_operator_expression(project, translated_definition, scope.clone(), loc, x, "+="),
        solidity::Expression::PreDecrement(loc, x) => translate_pre_operator_expression(project, translated_definition, scope.clone(), loc, x, "-="),
        solidity::Expression::PostIncrement(loc, x) => translate_post_operator_expression(project, translated_definition, scope.clone(), loc, x, "+="),
        solidity::Expression::PostDecrement(loc, x) => translate_post_operator_expression(project, translated_definition, scope.clone(), loc, x, "-="),
        
        _ => {
            // println!(
            //     "Translating pre- or post-operator value expression: {expression}; from {}",
            //     match project.loc_to_line_and_column(&translated_definition.path, &expression.loc()) {
            //         Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
            //         None => format!("{} - ", translated_definition.path.to_string_lossy()),
            //     },
            // );

            let result = translate_expression(project, translated_definition, scope.clone(), expression)?;
            // println!("Translated pre- or post-operator value expression: {}", sway::TabbedDisplayer(&result));
            Ok(result)
        }
    }
}

#[inline]
pub fn translate_pre_operator_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    loc: &solidity::Loc,
    x: &solidity::Expression,
    operator: &str,
) -> Result<sway::Expression, Error> {
    let assignment = sway::Statement::from(
        translate_assignment_expression(project,
            translated_definition,
           scope.clone(),
            operator,
            x,
            &solidity::Expression::NumberLiteral(*loc, "1".into(), "".into(), None),
        )?
    );

    let (variable, expression) = translate_variable_access_expression(project, translated_definition, scope.clone(), x)?;
    if variable.is_none() {
        panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
    }
    let variable = variable.unwrap();
    let mut variable = variable.borrow_mut();

    variable.read_count += 1;

    Ok(sway::Expression::from(sway::Block {
        statements: vec![assignment],
        final_expr: Some(
            if variable.is_storage {
                sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression,
                        member: "read".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![],
                })
            } else {
                expression
            }
        ),
    }))
}

#[inline]
pub fn translate_post_operator_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    loc: &solidity::Loc,
    x: &solidity::Expression,
    operator: &str,
) -> Result<sway::Expression, Error> {
    let assignment = sway::Statement::from(
        translate_assignment_expression(project,
            translated_definition,
           scope.clone(),
            operator,
            x,
            &solidity::Expression::NumberLiteral(*loc, "1".into(), "".into(), None),
        )?
    );

    let (variable, expression) = translate_variable_access_expression(project, translated_definition, scope.clone(), x)?;
    if variable.is_none() {
        panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
    }
    let variable = variable.unwrap();
    let mut variable = variable.borrow_mut();

    variable.read_count += 1;

    let variable_name = if variable.is_storage {
        variable.new_name.clone()
    } else {
        format!("_{}", variable.new_name)
    };

    Ok(sway::Expression::from(sway::Block {
        statements: vec![
            sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                    is_mutable: false,
                    name: variable_name.clone(),
                }),
                type_name: None,
                value: if variable.is_storage {
                    sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::from(sway::MemberAccess {
                            expression,
                            member: "read".into(),
                        }),
                        generic_parameters: None,
                        parameters: vec![],
                    })
                } else {
                    expression
                },
            }),
            assignment,
        ],
        final_expr: Some(sway::Expression::Identifier(variable_name)),
    }))
}

#[inline]
pub fn translate_new_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
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
                let value = translate_expression(project, translated_definition, scope.clone(), &block_arg.expr)?;

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
                        "{}WARNING: unsupported function call block arg: {arg}",
                        match project.loc_to_line_and_column(&translated_definition.path, &block_arg.loc()) {
                            Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                            None => format!("{} - ", translated_definition.path.to_string_lossy()),
                        }
                    ),
                }
            }

            Some(fields)
        }

        _ => None,
    };

    loop {
        let solidity::Expression::Parenthesis(_, expression) = expr.as_ref() else { break };
        expr = expression.clone();
    }

    if let solidity::Expression::New(_, expression) = expr.as_ref() {
        expr = expression.clone();
    }

    let args = args.iter()
        .map(|e| translate_expression(project, translated_definition, scope.clone(), e))
        .collect::<Result<Vec<_>, _>>()?;

    match expr.as_ref() {
        solidity::Expression::Variable(solidity::Identifier {name, ..}) => {
            if project.find_definition_with_abi(name).is_some() {
                // new Contract(...) => /*unsupported: new Contract(...); using:*/ abi(Contract, Identity::ContractId(ContractId::from(ZERO_B256)))

                translated_definition.ensure_use_declared("std::constants::ZERO_B256");

                return Ok(sway::Expression::Commented(
                    format!("unsupported: new {expression}; using:"),
                    Box::new(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("abi".into()),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::Identifier(name.clone()),
                            sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier("Identity::ContractId".into()),
                                generic_parameters: None,
                                parameters: vec![
                                    sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::Identifier("ContractId::from".into()),
                                        generic_parameters: None,
                                        parameters: vec![
                                            sway::Expression::Identifier("ZERO_B256".into()),
                                        ],
                                    }),
                                ],
                            }),
                        ],
                    }))
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
                    panic!("Invalid new array expression: expected no block args, found {block_fields:#?}");
                }

                if args.len() != 1 {
                    panic!("Invalid new array expression: expected 1 argument, found {}", args.len());
                }

                // Ensure `std::bytes::Bytes` is imported
                translated_definition.ensure_use_declared("std::bytes::Bytes");

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
                                function: sway::Expression::Identifier("Bytes::with_capacity".into()),
                                generic_parameters: None,
                                parameters: vec![
                                    length.clone(),
                                ],
                            }),
                        }),

                        // let mut i = 0;
                        sway::Statement::from(sway::Let {
                            pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                is_mutable: true,
                                name: "i".into(),
                            }),
                            type_name: None,
                            value: sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                        }),

                        // while i < length {
                        //     v.push(0);
                        //     i += 1;
                        // }
                        sway::Statement::from(sway::Expression::from(sway::While {
                            // i < length
                            condition: sway::Expression::from(sway::BinaryExpression {
                                operator: "<".into(),
                                lhs: sway::Expression::Identifier("i".into()),
                                rhs: length.clone(),
                            }),

                            body: sway::Block {
                                statements: vec![
                                    // v.push(0);
                                    sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::from(sway::MemberAccess {
                                            expression: sway::Expression::Identifier("v".into()),
                                            member: "push".into(),
                                        }),
                                        generic_parameters: None,
                                        parameters: vec![
                                            sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                                        ],
                                    })),

                                    // i += 1;
                                    sway::Statement::from(sway::Expression::from(sway::BinaryExpression {
                                        operator: "+=".into(),
                                        lhs: sway::Expression::Identifier("i".into()),
                                        rhs: sway::Expression::from(sway::Literal::DecInt(BigUint::one(), None)),
                                    })),
                                ],
                                final_expr: None,
                            }
                        }))
                    ],

                    // v
                    final_expr: Some(sway::Expression::Identifier("v".into())),
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
                    panic!("Invalid new string expression: expected no block args, found {block_fields:#?}");
                }

                if args.len() != 1 {
                    panic!("Invalid new string expression: expected 1 argument, found {}", args.len());
                }

                // Ensure `std::bytes::Bytes` is imported
                translated_definition.ensure_use_declared("std::bytes::Bytes");

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
                                function: sway::Expression::Identifier("Bytes::with_capacity".into()),
                                generic_parameters: None,
                                parameters: vec![
                                    length.clone(),
                                ],
                            }),
                        }),

                        // let mut i = 0;
                        sway::Statement::from(sway::Let {
                            pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                                is_mutable: true,
                                name: "i".into(),
                            }),
                            type_name: None,
                            value: sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                        }),

                        // while i < length {
                        //     v.push(0);
                        //     i += 1;
                        // }
                        sway::Statement::from(sway::Expression::from(sway::While {
                            // i < length
                            condition: sway::Expression::from(sway::BinaryExpression {
                                operator: "<".into(),
                                lhs: sway::Expression::Identifier("i".into()),
                                rhs: length.clone(),
                            }),

                            body: sway::Block {
                                statements: vec![
                                    // v.push(0);
                                    sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::from(sway::MemberAccess {
                                            expression: sway::Expression::Identifier("v".into()),
                                            member: "push".into(),
                                        }),
                                        generic_parameters: None,
                                        parameters: vec![
                                            sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                                        ],
                                    })),

                                    // i += 1;
                                    sway::Statement::from(sway::Expression::from(sway::BinaryExpression {
                                        operator: "+=".into(),
                                        lhs: sway::Expression::Identifier("i".into()),
                                        rhs: sway::Expression::from(sway::Literal::DecInt(BigUint::one(), None)),
                                    })),
                                ],
                                final_expr: None,
                            }
                        }))
                    ],

                    // String::from(v)
                    final_expr: Some(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("String::from".into()),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::Identifier("v".into()),
                        ],
                    })),
                }));
            }

            _ => todo!("translate new {} expression: {expression} {expression:#?}", type_name.to_string())
        }

        _ => todo!("translate new expression: {expr:#?} - {expression:#?}")
    }

    let name = match expr.as_ref() {
        solidity::Expression::ArraySubscript(_, ex, _) => {
            let solidity::Expression::Variable(solidity::Identifier { name, .. }) = ex.as_ref() else { todo!("translate new expression: {expression:#?}") };
            name
        },
        solidity::Expression::Variable(solidity::Identifier { name, .. }) => name,
        _ => todo!("translate new expression: {expression:#?}")
    };
    
    match block_fields {
        Some(fields) => Ok(sway::Expression::from(sway::FunctionCallBlock {
            function: sway::Expression::Identifier(name.clone()),
            generic_parameters: None,
            fields,
            parameters: vec![],
        })),

        None => Ok(sway::Expression::from(sway::FunctionCall {
            function: sway::Expression::Identifier(name.clone()),
            generic_parameters: None,
            parameters: vec![],
        })),
    }
}

#[inline]
pub fn translate_delete_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    let (variable, expr) = translate_variable_access_expression(project, translated_definition, scope.clone(), expression)?;
    if variable.is_none() {
        panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
    }
    let variable = variable.unwrap();
    let type_name = variable.borrow().type_name.clone();
    
    let value = create_value_expression(translated_definition, scope.clone(), &type_name, None);
    create_assignment_expression(project, translated_definition, scope.clone(), "=", &expr, variable, &value, &type_name)
}

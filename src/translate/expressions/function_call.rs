use std::{cell::RefCell, rc::Rc};
use convert_case::Case;
use num_bigint::BigUint;
use num_traits::{Num, One, Zero};
use solang_parser::{helpers::CodeLocation, pt as solidity};
use crate::{errors::Error, project::Project, sway, translate::{address_call::translate_address_call_expression, translate_type_name, TranslatedDefinition, TranslationScope}};
use super::{translate_expression, variable::translate_variable_access_expression};

pub fn handle_function_named_arguments(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    external_scope: Rc<RefCell<TranslationScope>>,
    function_name: &str,
    named_arguments: Option<&[solidity::NamedArgument]>,
    parameters: &mut Vec<sway::Expression>,
    parameter_types: &mut Vec<sway::TypeName>,
) -> Result<(), Error> {
    let Some(named_arguments) = named_arguments else {
        return Ok(());
    };

    let mut named_parameters = vec![];

    for arg in named_arguments {
        named_parameters.push((
            crate::translate_naming_convention(&arg.name.name, Case::Snake),
            translate_expression(project, translated_definition, scope.clone(), &arg.expr)?
        ));
    }

    if let Some(function) = external_scope.borrow().find_function(|f| {
        let f = f.borrow();

        if f.old_name != function_name {
            return false;
        }

        if f.parameters.entries.len() != named_parameters.len() {
            return false;
        }

        f.parameters.entries.iter().all(|p| named_parameters.iter().any(|(name, _)| p.name == *name))
    }) {
        let function = function.borrow();

        parameters.clear();
        parameter_types.clear();

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

    Ok(())
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

                        match &value_type_name {
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

                                _ => panic!("translate cast from {value_type_name} to address: {expression} - {value:#?}"),
                            }

                            sway::TypeName::Array { type_name: element_type_name, length } => match element_type_name.as_ref() {
                                sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                                    ("u8", None) => {
                                        translated_definition.ensure_use_declared(format!("std::array_conversions::u256::*").as_str());
    
                                        let mut elements = (0..*length).map(|i| {
                                            sway::Expression::from(sway::ArrayAccess {
                                                expression: value.clone(),
                                                index: sway::Expression::from(sway::Literal::DecInt(i.into(), None)),
                                            })
                                        }).collect::<Vec<_>>();
                                        
                                        if *length < 32 {
                                            elements.extend((0 .. (32 - *length)).map(|_| {
                                                sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None))
                                            }));
                                        }

                                        Ok(sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::Identifier("Identity::Address".into()),
                                            generic_parameters: None,
                                            parameters: vec![
                                                sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::Identifier("Address::from".into()),
                                                    generic_parameters: None,
                                                    parameters: vec![
                                                        sway::Expression::from(sway::FunctionCall {
                                                            function: sway::Expression::Identifier(format!("u256::from_be_bytes")),
                                                            generic_parameters: None,
                                                            parameters: vec![
                                                                sway::Expression::from(sway::Array { elements }),
                                                            ],
                                                        }),
                                                    ],
                                                }),
                                            ],
                                        }))
                                    }
    
                                    _ => panic!(
                                        "{}translate cast from {value_type_name} to address: {expression} - {value_type_name:#?}",
                                        match project.loc_to_line_and_column(&translated_definition.path, &arguments[0].loc()) {
                                            Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                                            None => format!("{} - ", translated_definition.path.to_string_lossy()),
                                        },
                                    ),
                                }
    
                                _ => panic!(
                                    "{}translate cast from {value_type_name} to address: {expression} - {value_type_name:#?}",
                                    match project.loc_to_line_and_column(&translated_definition.path, &arguments[0].loc()) {
                                        Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                                        None => format!("{} - ", translated_definition.path.to_string_lossy()),
                                    },
                                ),
                            }
    
                            _ => panic!(
                                "{}translate cast from {value_type_name} to address: {expression} - {value_type_name:#?}",
                                match project.loc_to_line_and_column(&translated_definition.path, &arguments[0].loc()) {
                                    Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                                    None => format!("{} - ", translated_definition.path.to_string_lossy()),
                                },
                            ),
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

                        if from_bits < to_bits {
                            return Ok(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier(format!("I{to_bits}::from")),
                                generic_parameters: None,
                                parameters: vec![sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: value,
                                        member: format!("as_u{to_bits}"),
                                    }),
                                    generic_parameters: None,
                                    parameters: vec![],
                                })],
                            }))
                        }

                        Ok(sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier(format!("I{to_bits}::from")),
                            generic_parameters: None,
                            parameters: vec![
                                sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::Identifier(format!("u{to_bits}::try_from")),
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
                                eprintln!("WARNING: unsupported signed integer type `int{bits}`, using `I8`...");
                            }
                            8
                        }
                        9..=16 => {
                            if *bits != 16 {
                                eprintln!("WARNING: unsupported signed integer type `int{bits}`, using `I16`...");
                            }
                            16
                        }
                        17..=32 => {
                            if *bits != 32 {
                                eprintln!("WARNING: unsupported signed integer type `int{bits}`, using `I32`...");
                            }
                            32
                        }
                        33..=64 => {
                            if *bits != 64 {
                                eprintln!("WARNING: unsupported signed integer type `int{bits}`, using `I64`...");
                            }
                            64
                        }
                        65..=128 => {
                            if *bits != 128 {
                                eprintln!("WARNING: unsupported signed integer type `int{bits}`, using `I128`...");
                            }
                            128
                        }
                        129..=256 => {
                            if *bits != 256 {
                                eprintln!("WARNING: unsupported signed integer type `int{bits}`, using `I256`...");
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
                            
                            ("u8", 32) => {
                                Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier("I32::from_uint".into()),
                                    generic_parameters: None,
                                    parameters: vec![
                                        sway::Expression::from(sway::FunctionCall { 
                                            function: sway::Expression::from(sway::MemberAccess { 
                                                expression: value_expression, 
                                                member: "as_u32".into() 
                                            }), 
                                            generic_parameters: None, 
                                            parameters: vec![], 
                                        })
                                    ],
                                }))
                            }

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
                                            BigUint::from_str_radix("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 16).unwrap()
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

                        if from_bits < to_bits {
                            return Ok(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::from(sway::MemberAccess {
                                    expression: value,
                                    member: format!("as_u{to_bits}"),
                                }),
                                generic_parameters: None,
                                parameters: vec![],
                            }))
                        }

                        Ok(sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::from(sway::MemberAccess {
                                expression: sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier(format!("u{to_bits}::try_from")),
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

                        sway::TypeName::Array { type_name: element_type_name, length } => match element_type_name.as_ref() {
                            sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                                ("u8", None) if bits == 8 && *length == 1 => Ok(sway::Expression::from(sway::ArrayAccess {
                                    expression: value_expression,
                                    index: sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                                })),

                                ("u8", None) if bits == 256 && *length == 32 => {
                                    translated_definition.ensure_use_declared("std::array_conversions::u256::*");

                                    Ok(sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::Identifier("u256::from_be_bytes".into()),
                                        generic_parameters: None,
                                        parameters: vec![
                                            value_expression,
                                        ],
                                    }))
                                }

                                ("u8", None) if *length < (bits / 8) => {
                                    translated_definition.ensure_use_declared(format!("std::array_conversions::u{bits}::*").as_str());

                                    let mut elements = (0..*length).map(|i| sway::Expression::from(sway::ArrayAccess {
                                        expression: value_expression.clone(),
                                        index: sway::Expression::from(sway::Literal::DecInt(i.into(), None)),
                                    })).collect::<Vec<_>>();

                                    elements.extend(
                                        (0 .. ((bits / 8) - *length)).map(|_| sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)))
                                    );

                                    Ok(sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::Identifier(format!("u{bits}::from_be_bytes")),
                                        generic_parameters: None,
                                        parameters: vec![
                                            sway::Expression::from(sway::Array { elements }),
                                        ],
                                    }))
                                }

                                _ => {
                                    todo!("translate cast from {value_type_name} to u{bits}: {} - {expression:#?}", expression)
                                }
                            }

                            _ => todo!("translate {value_type_name} type cast: {} - {expression:#?}", expression),
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

                        sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                            ("todo!", None) => {
                                // HACK: don't try to do anything with todo! expressions...
                                Ok(value_expression)
                            }

                            ("b256", None) => {
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
                                    final_expr: Some(sway::Expression::from(sway::Array {
                                        elements: (0..*byte_count).map(|index| {
                                            sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::from(sway::MemberAccess {
                                                    expression: sway::Expression::from(sway::FunctionCall {
                                                        function: sway::Expression::from(sway::MemberAccess {
                                                            expression: sway::Expression::Identifier(variable_name.clone()),
                                                            member: "get".into(),
                                                        }),
                                                        generic_parameters: None,
                                                        parameters: vec![
                                                            sway::Expression::from(sway::Literal::DecInt(index.into(), None)),
                                                        ],
                                                    }),
                                                    member: "unwrap".into(),
                                                }),
                                                generic_parameters: None,
                                                parameters: vec![],
                                            })
                                        }).collect(),
                                    })),
                                }))
                            }

                            ("u64", None) => match value_expression {
                                sway::Expression::Literal(
                                    sway::Literal::DecInt(value, None)
                                    | sway::Literal::HexInt(value, None)
                                ) if value.is_zero() => {
                                    Ok(sway::Expression::from(sway::Array {
                                        elements: (0..*byte_count).map(|_| {
                                            sway::Expression::from(sway::Literal::DecInt(0u8.into(), Some("u8".into())))
                                        }).collect(),
                                    }))
                                }

                                _ => {
                                    // {
                                    //     let b = x.to_be_bytes();
                                    //     [b.get(0).unwrap(), b.get(1).unwrap(), ..., 0, 0, 0]
                                    // }

                                    let variable_name = scope.borrow_mut().generate_unique_variable_name("b");

                                    Ok(sway::Expression::from(sway::Block {
                                        statements: vec![
                                            sway::Statement::from(sway::Let {
                                                pattern: sway::LetPattern::from(sway::LetIdentifier {
                                                    is_mutable: false,
                                                    name: variable_name.clone(),
                                                }),
                                                type_name: None,
                                                value: sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::from(sway::MemberAccess {
                                                        expression: value_expression.clone(),
                                                        member: "to_be_bytes".into(),
                                                    }),
                                                    generic_parameters: None,
                                                    parameters: vec![],
                                                }),
                                            }),
                                        ],
                                        final_expr: Some(sway::Expression::from(sway::Array {
                                            elements: (0..*byte_count).map(|index| {
                                                if index < 8 {
                                                    sway::Expression::from(sway::ArrayAccess {
                                                        expression: sway::Expression::Identifier(variable_name.clone()),
                                                        index: sway::Expression::from(sway::Literal::DecInt(index.into(), None)),
                                                    })
                                                } else {
                                                    sway::Expression::from(sway::Literal::DecInt(0u8.into(), None))
                                                }
                                            }).collect(),
                                        })),
                                    }))
                                }
                            }

                            ("u256", None) => {
                                translated_definition.ensure_use_declared("std::array_conversions::u256::*");

                                Ok(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: value_expression,
                                        member: "to_be_bytes".into(),
                                    }),
                                    generic_parameters: None,
                                    parameters: vec![],
                                }))
                            }

                            ("Bytes", None) => Ok(sway::Expression::from(sway::Array {
                                elements: (0..*byte_count).map(|index| {
                                    sway::Expression::from(sway::FunctionCall {
                                        function: sway::Expression::from(sway::MemberAccess {
                                            expression: sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::from(sway::MemberAccess {
                                                    expression: value_expression.clone(),
                                                    member: "get".into(),
                                                }),
                                                generic_parameters: None,
                                                parameters: vec![
                                                    sway::Expression::from(sway::Literal::DecInt(index.into(), None)),
                                                ],
                                            }),
                                            member: "unwrap_or".into(),
                                        }),
                                        generic_parameters: None,
                                        parameters: vec![
                                            sway::Expression::from(sway::Literal::DecInt(0u8.into(), Some("u8".into()))),
                                        ],
                                    })
                                }).collect(),
                            })),

                            ("raw_slice", None) => {
                                let variable_name = scope.borrow_mut().generate_unique_variable_name("b");

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
                                                    value_expression,
                                                ]
                                            }),
                                        })
                                    ],
                                    final_expr: Some(sway::Expression::from(sway::Array {
                                        elements: (0..*byte_count).map(|index| {
                                            sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::from(sway::MemberAccess {
                                                    expression: sway::Expression::from(sway::FunctionCall {
                                                        function: sway::Expression::from(sway::MemberAccess {
                                                            expression: sway::Expression::Identifier(variable_name.clone()),
                                                            member: "get".into(),
                                                        }),
                                                        generic_parameters: None,
                                                        parameters: vec![
                                                            sway::Expression::from(sway::Literal::DecInt(index.into(), None)),
                                                        ],
                                                    }),
                                                    member: "unwrap_or".into(),
                                                }),
                                                generic_parameters: None,
                                                parameters: vec![
                                                    sway::Expression::from(sway::Literal::DecInt(0u8.into(), Some("u8".into()))),
                                                ],
                                            })
                                        }).collect(),
                                    })),
                                }))
                            }

                            _ => panic!(
                                "{}TODO: translate from {value_type_name} to bytes{byte_count}",
                                match project.loc_to_line_and_column(&translated_definition.path, &function.loc()) {
                                    Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                                    None => format!("{} - ", translated_definition.path.to_string_lossy()),
                                },
                            ),
                        }

                        sway::TypeName::Array { .. } => todo!("translate from {value_type_name} to bytes{byte_count}"),
                        sway::TypeName::Tuple { .. } => todo!("translate from {value_type_name} to bytes{byte_count}"),

                        sway::TypeName::StringSlice => {
                            match &value_expression {
                                sway::Expression::Literal(sway::Literal::String(s)) => {
                                    let bytes = s.bytes().collect::<Vec<u8>>();

                                    Ok(sway::Expression::from(sway::Array {
                                        elements: (0..*byte_count).map(|index| {
                                            let index = index as usize;
                                            
                                            sway::Expression::from(sway::Literal::HexInt(
                                                if index < bytes.len() {
                                                    bytes[index].into()
                                                } else {
                                                    0u8.into()
                                                },
                                                Some("u8".into()),
                                            ))
                                        }).collect(),
                                    }))
                                }

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
                        
                        sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                            ("String", None) => Ok(sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::from(sway::MemberAccess {
                                    expression: value_expression,
                                    member: "as_bytes".into(),
                                }),
                                generic_parameters: None,
                                parameters: vec![],
                            })),

                            _ => todo!("translate from {value_type_name} to bytes"),
                        },

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

                                    _ => {}
                                }

                                _ => {}
                            }
                        }
                    }

                    handle_function_named_arguments(
                        project,
                        translated_definition,
                        scope.clone(),
                        scope.clone(),
                        old_name,
                        named_arguments,
                        &mut parameters,
                        &mut parameter_types,
                    )?;

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

                            match parameter_type_name {
                                sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                                    ("b256", None) => {
                                        // HACK: convert [u8; 32] to b256
                                        if let sway::TypeName::Array { length: 32, .. } = value_type_name {
                                            if value_type_name.is_u8_array() {
                                                parameters[i] = sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::Identifier("b256::from_be_bytes".into()),
                                                    generic_parameters: None,
                                                    parameters: vec![
                                                        parameters[i].clone(),
                                                    ],
                                                });
                                                continue;
                                            }
                                        }
                                    }

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
                                    
                                    ("Identity", None) => {
                                        // HACK:
                                        // If `parameter_type_name` is `Identity`, but `container` is an abi cast expression,
                                        // then we need to de-cast it, so `container` turns into the 2nd parameter of the abi cast,
                                        // and `value_type_name` turns into `Identity`.
                                        if let sway::Expression::FunctionCall(function_call) = parameters[i].clone() {
                                            if let sway::Expression::Identifier(function_name) = &function_call.function {
                                                if function_name == "abi" {
                                                    parameters[i] = function_call.parameters[1].clone();
                                                    continue;
                                                }
                                            }
                                        }
                                    }

                                    _ => {}
                                }

                                _ => {}
                            }

                            if !value_type_name.is_compatible_with(&parameter_type_name) {
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
                        "{}error: Failed to find function `{old_name}({})` in scope: {function}({})",
                        match project.loc_to_line_and_column(&translated_definition.path, &function.loc()) {
                            Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                            None => format!("{} - ", translated_definition.path.to_string_lossy()),
                        },
                        parameter_types.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "),
                        parameters.iter().map(|t| sway::TabbedDisplayer(t).to_string()).collect::<Vec<_>>().join(", "),
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
                            let Some(inherited_definition) = project.translated_definitions.iter().find(|d| d.name == inherit).cloned() else {
                                panic!("Failed to find inherited definition for `{inherit}`");
                            };

                            handle_function_named_arguments(
                                project,
                                translated_definition,
                                scope.clone(),
                                inherited_definition.toplevel_scope.clone(),
                                member.name.as_str(),
                                named_arguments,
                                &mut parameters,
                                &mut parameter_types,
                            )?;
                            
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
                                        
                                    // HACK: [u8; 32] -> b256
                                    if let Some(32) = value_type_name.u8_array_length() {
                                        if parameter_type_name.is_b256() {
                                            parameters[i] = sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::Identifier("b256::from_be_bytes".into()),
                                                generic_parameters: None,
                                                parameters: vec![
                                                    parameters[i].clone(),
                                                ],
                                            });
                                            continue;
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

                        panic!(
                            "{}TODO: handle super member access function `{member:#?}`",
                            match project.loc_to_line_and_column(&translated_definition.path, &member.loc()) {
                                Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                                None => format!("{} - ", translated_definition.path.to_string_lossy()),
                            },
                        )
                    }

                    "this" => {
                        let mut parameters = arguments.iter()
                            .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                            .collect::<Result<Vec<_>, _>>()?;

                        let mut parameter_types = parameters.iter()
                            .map(|p| translated_definition.get_expression_type(scope.clone(), p))
                            .collect::<Result<Vec<_>, _>>()?;
                        
                        handle_function_named_arguments(
                            project,
                            translated_definition,
                            scope.clone(),
                            scope.clone(),
                            member.name.as_str(),
                            named_arguments,
                            &mut parameters,
                            &mut parameter_types,
                        )?;
                        
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
                                
                                // HACK: [u8; 32] -> b256
                                if let Some(32) = value_type_name.u8_array_length() {
                                    if parameter_type_name.is_b256() {
                                        parameters[i] = sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::Identifier("b256::from_be_bytes".into()),
                                            generic_parameters: None,
                                            parameters: vec![
                                                parameters[i].clone(),
                                            ],
                                        });
                                        continue;
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
                                handle_function_named_arguments(
                                    project,
                                    translated_definition,
                                    scope.clone(),
                                    inherited_definition.toplevel_scope.clone(),
                                    member.name.as_str(),
                                    named_arguments,
                                    &mut parameters,
                                    &mut parameter_types,
                                )?;
                                
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
                                        
                                        // HACK: [u8; 32] -> b256
                                        if let Some(32) = value_type_name.u8_array_length() {
                                            if parameter_type_name.is_b256() {
                                                parameters[i] = sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::Identifier("b256::from_be_bytes".into()),
                                                    generic_parameters: None,
                                                    parameters: vec![
                                                        parameters[i].clone(),
                                                    ],
                                                });
                                                continue;
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
                                    translated_definition.ensure_struct_included(project, &struct_definition);

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

                            handle_function_named_arguments(
                                project,
                                translated_definition,
                                scope.clone(),
                                scope.clone(),
                                old_name.as_str(),
                                named_arguments,
                                &mut parameters,
                                &mut parameter_types,
                            )?;
                            
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

                                    // HACK: StorageKey<u*> -> u*
                                    if let Some(value_type) = value_type_name.storage_key_type() {
                                        if !parameter_type_name.is_storage_key() && parameter_type_name.is_compatible_with(&value_type){
                                            parameters[i] = sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::from(sway::MemberAccess {
                                                    expression: parameters[i].clone(),
                                                    member: "read".into(),
                                                }),
                                                generic_parameters: None,
                                                parameters: vec![],
                                            });
                                            continue;
                                        }
                                    }
                                    
                                    // HACK: [u8; 32] -> b256
                                    if let Some(32) = value_type_name.u8_array_length() {
                                        if parameter_type_name.is_b256() {
                                            parameters[i] = sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::Identifier("b256::from_be_bytes".into()),
                                                generic_parameters: None,
                                                parameters: vec![
                                                    parameters[i].clone(),
                                                ],
                                            });
                                            continue;
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

                                // Ensure the function's new name matches the function call we're translating
                                if function.new_name != external_function_declaration.new_name {
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
                                
                                    // HACK: [u8; 32] -> b256
                                    if let Some(32) = value_type_name.u8_array_length() {
                                        if parameter_type_name.is_b256() {
                                            parameters[i] = sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::Identifier("b256::from_be_bytes".into()),
                                                generic_parameters: None,
                                                parameters: vec![
                                                    parameters[i].clone(),
                                                ],
                                            });
                                            continue;
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
                            // If the return type is user-defined type, struct, or enum 
                            // then we need to add to the current translated definition
                            // if we have not already
                            if let Some(return_type) = external_function_definition.return_type.as_ref() {   
                                fn check_type_name(project: &Project, translated_definition: &mut TranslatedDefinition, type_name: &sway::TypeName) {
                                    match type_name {
                                        sway::TypeName::Undefined => {}

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
                                                        translated_definition.add_enum(enum_definition);
                                                        break 'lookup;
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
                                                        }

                                                        (None, None) => {}

                                                        _ => continue,
                                                    }

                                                    translated_definition.ensure_struct_included(project, struct_definition);
                                                    break 'lookup;
                                                }
                                            }
                                        }

                                        sway::TypeName::Array { type_name, .. } => {
                                            check_type_name(project, translated_definition, type_name);
                                        }

                                        sway::TypeName::Tuple { type_names } => {
                                            type_names.iter().for_each(|type_name| check_type_name(project, translated_definition, type_name));
                                        }

                                        sway::TypeName::StringSlice => {}
                                        sway::TypeName::StringArray { .. } => {}
                                    }
                                }

                                check_type_name(project, translated_definition, return_type);
                            }

                            // Create the function call
                            let function_call = sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier(external_function_declaration.new_name.clone()),
                                generic_parameters: None,
                                parameters: parameters.borrow().iter().map(Clone::clone).collect(),
                            });
    
                            *translated_definition.function_call_counts.entry(external_function_declaration.new_name.clone()).or_insert(0) += 1;
                            translated_definition.functions_called
                                .entry(translated_definition.current_functions.last().cloned().unwrap())
                                .or_insert_with(|| vec![])
                                .push(external_function_declaration.new_name.clone());
                            
                            return Ok(function_call);
                        }
                    
                        // Check if function is contained in the current contract (self-referential: This.func())
                        if name == translated_definition.name {
                            let old_name = member.name.clone();

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
                                    translated_definition.ensure_struct_included(project, &struct_definition);

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

                            handle_function_named_arguments(
                                project,
                                translated_definition,
                                scope.clone(),
                                scope.clone(),
                                old_name.as_str(),
                                named_arguments,
                                &mut parameters,
                                &mut parameter_types,
                            )?;
                            
                            let parameters_cell = Rc::new(RefCell::new(parameters.as_mut_slice()));

                            // Check if the member is a function defined in the toplevel scope
                            let Some(function_declaration) = translated_definition.toplevel_scope.borrow().find_function(|function| {
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
                                
                                    // HACK: [u8; 32] -> b256
                                    if let Some(32) = value_type_name.u8_array_length() {
                                        if parameter_type_name.is_b256() {
                                            parameters[i] = sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::Identifier("b256::from_be_bytes".into()),
                                                generic_parameters: None,
                                                parameters: vec![
                                                    parameters[i].clone(),
                                                ],
                                            });
                                            continue;
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
    
                            let function_declaration = function_declaration.borrow();
                            
                            // Create the function call
                            let function_call = sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier(function_declaration.new_name.clone()),
                                generic_parameters: None,
                                parameters: arguments.iter()
                                    .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                                    .collect::<Result<Vec<_>, _>>()?,
                            });
    
                            *translated_definition.function_call_counts.entry(function_declaration.new_name.clone()).or_insert(0) += 1;
                            translated_definition.functions_called
                                .entry(translated_definition.current_functions.last().cloned().unwrap())
                                .or_insert_with(|| vec![])
                                .push(function_declaration.new_name.clone());
                            
                            return Ok(function_call);
                        }
                    }
                }

                _ => {}
            }

            let solidity_container = container;

            let mut container = translate_expression(project, translated_definition, scope.clone(), container)?;
            let mut type_name = translated_definition.get_expression_type(scope.clone(), &container)?;

            // println!(
            //     "type of {} is {}",
            //     sway::TabbedDisplayer(&container),
            //     sway::TabbedDisplayer(&type_name),
            // );

            // HACK: tack `.read()` onto the end if the container is a StorageKey
            if let Some(storage_key_type) = type_name.storage_key_type() {
                container = sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression: container,
                        member: "read".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![],
                });

                type_name = storage_key_type;
            }

            let converter = |argument: &sway::Expression, argument_type_name: &sway::TypeName| -> sway::Expression {
                match argument_type_name {
                    sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                        ("u64", None) => argument.clone(),
                        ("u256", None) => sway::Expression::from(sway::FunctionCall{
                            function: sway::Expression::from(sway::MemberAccess {
                                expression: sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::Identifier("u64::try_from".into()),
                                    generic_parameters: None,
                                    parameters: vec![argument.clone()],
                                }),
                                member: "unwrap".into(),
                            }),
                            generic_parameters: None,
                            parameters: vec![],
                        }),
                        ("u8"| "u16" | "u32", None) => sway::Expression::from(sway::FunctionCall{
                            function: sway::Expression::from(sway::MemberAccess {
                                expression: argument.clone(),
                                member: "as_u64".into(),
                            }),
                            generic_parameters: None,
                            parameters: vec![],
                        }),

                        _ => argument.clone(),
                    },
                    
                    _ => argument.clone(),
                }
            };

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
                                                converter(&argument, &argument_type_name),
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
                                                        converter(&argument, &argument_type_name),
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

                        let variable = match translate_variable_access_expression(project, translated_definition, scope.clone(), &solidity_container) {
                            Ok((variable, _)) => variable,
                            Err(_) => None,
                        };

                        // Check if expression is a variable that had an ABI type
                        if let Some(variable) = variable.as_ref() {
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

                        todo!(
                            "{}translate Identity member function call `{member}`: {} - {container:#?}",
                            match project.loc_to_line_and_column(&translated_definition.path, &function.loc()) {
                                Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                                None => format!("{} - ", translated_definition.path.to_string_lossy()),
                            },
                            sway::TabbedDisplayer(&container),
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

                        let parameter_types = parameters.iter()
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
                            let external_scope = if matches!(translated_definition.kind, Some(solidity::ContractTy::Library(_))) && using_directive.library_name == translated_definition.name {
                                translated_definition.toplevel_scope.clone()
                            } else {
                                match project.translated_definitions.iter()
                                .find(|d| {
                                    d.name == using_directive.library_name && matches!(d.kind.as_ref().unwrap(), solidity::ContractTy::Library(_))
                                })
                                .map(|d| d.toplevel_scope.clone()) {
                                    Some(s) => s,
                                    None => continue,
                                }
                            };

                            let mut using_parameters = using_parameters.clone();
                            let mut using_parameter_types = using_parameter_types.clone();
                            
                            handle_function_named_arguments(
                                project,
                                translated_definition,
                                scope.clone(),
                                external_scope.clone(),
                                member.name.as_str(),
                                named_arguments,
                                &mut using_parameters,
                                &mut using_parameter_types,
                            )?;

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
                                
                                    // HACK: [u8; 32] -> b256
                                    if let Some(32) = value_type_name.u8_array_length() {
                                        if parameter_type_name.is_b256() {
                                            parameters[i] = sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::Identifier("b256::from_be_bytes".into()),
                                                generic_parameters: None,
                                                parameters: vec![
                                                    parameters[i].clone(),
                                                ],
                                            });
                                            continue;
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
                        let mut check_abi = |abi: &sway::Abi| -> Result<Option<sway::Expression>, Error> {
                            if abi.name != *name {
                                return Ok(None);
                            }

                            // TODO: fix this...
                            if named_arguments.is_some() {
                                return Ok(None);
                            }

                            'function_lookup: for function in abi.functions.iter() {
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
                                
                                    // HACK: [u8; 32] -> b256
                                    if let Some(32) = value_type_name.u8_array_length() {
                                        if parameter_type_name.is_b256() {
                                            parameters[i] = sway::Expression::from(sway::FunctionCall {
                                                function: sway::Expression::Identifier("b256::from_be_bytes".into()),
                                                generic_parameters: None,
                                                parameters: vec![
                                                    parameters[i].clone(),
                                                ],
                                            });
                                            continue 'value_type_check;
                                        }
                                    }

                                    if !value_type_name.is_compatible_with(parameter_type_name) {
                                        continue 'function_lookup;
                                    }
                                }

                                *translated_definition.function_call_counts.entry(function.name.clone()).or_insert(0) += 1;
                                translated_definition.functions_called
                                    .entry(translated_definition.current_functions.last().cloned().unwrap())
                                    .or_insert_with(|| vec![])
                                    .push(function.name.clone());
                                
                                return Ok(Some(sway::Expression::from(sway::FunctionCall {
                                    function: sway::Expression::from(sway::MemberAccess {
                                        expression: container.clone(),
                                        member: function.name.clone(),
                                    }),
                                    generic_parameters: None,
                                    parameters: parameters.clone(),
                                })));
                            }

                            Ok(None)
                        };

                        if let Some(abi) = translated_definition.abi.as_ref() {
                            if let Some(result) = check_abi(abi)? {
                                return Ok(result);
                            }
                        }

                        for abi in translated_definition.abis.clone() {
                            if let Some(result) = check_abi(&abi)? {
                                return Ok(result);
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

                sway::TypeName::Array { .. } => {
                    let mut parameters = arguments.iter()
                        .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                        .collect::<Result<Vec<_>, _>>()?;

                    let mut parameter_types = parameters.iter()
                        .map(|p| translated_definition.get_expression_type(scope.clone(), p))
                        .collect::<Result<Vec<_>, _>>()
                        .unwrap();

                    parameters.insert(0, container.clone());
                    parameter_types.insert(0, translated_definition.get_expression_type(scope.clone(), &container).unwrap());

                    // Check if this is a function from a using directive
                    for using_directive in translated_definition.using_directives.clone() {
                        // Make sure the type names match
                        if let Some(for_type) = using_directive.for_type.as_ref() {
                            if *for_type != type_name {
                                continue;
                            }
                        }

                        // Look up the definition of the using directive
                        let external_scope = if matches!(translated_definition.kind, Some(solidity::ContractTy::Library(_))) && using_directive.library_name == translated_definition.name {
                            translated_definition.toplevel_scope.clone()
                        } else {
                            match project.translated_definitions.iter()
                            .find(|d| {
                                d.name == using_directive.library_name && matches!(d.kind.as_ref().unwrap(), solidity::ContractTy::Library(_))
                            })
                            .map(|d| d.toplevel_scope.clone()) {
                                Some(s) => s,
                                None => continue,
                            }
                        };

                        let mut parameters = parameters.clone();
                        let mut parameter_types = parameter_types.clone();

                        handle_function_named_arguments(
                            project,
                            translated_definition,
                            scope.clone(),
                            external_scope.clone(),
                            member.name.as_str(),
                            named_arguments,
                            &mut parameters,
                            &mut parameter_types,
                        )?;

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

                                // HACK: [u8; 32] -> b256
                                if let Some(32) = value_type_name.u8_array_length() {
                                    if parameter_type_name.is_b256() {
                                        parameters[i] = sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::Identifier("b256::from_be_bytes".into()),
                                            generic_parameters: None,
                                            parameters: vec![
                                                parameters[i].clone(),
                                            ],
                                        });
                                        continue 'value_type_check;
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
                                parameters,
                            }));
                        }
                    }

                    todo!(
                        "{}translate array member function call: {} - {}",
                        match project.loc_to_line_and_column(&translated_definition.path, &function.loc()) {
                            Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                            None => format!("{} - ", translated_definition.path.to_string_lossy()),
                        },
                        expression,
                        sway::TabbedDisplayer(&container),
                    )
                }

                sway::TypeName::Tuple { .. } => todo!("translate tuple member function call: {} - {container:#?}", sway::TabbedDisplayer(&container)),
                
                sway::TypeName::StringSlice => {
                    let mut parameters = arguments.iter()
                        .map(|a| translate_expression(project, translated_definition, scope.clone(), a))
                        .collect::<Result<Vec<_>, _>>()?;

                    let mut parameter_types = parameters.iter()
                        .map(|p| translated_definition.get_expression_type(scope.clone(), p))
                        .collect::<Result<Vec<_>, _>>()
                        .unwrap();
                    
                    parameters.insert(0, container.clone());
                    parameter_types.insert(0, translated_definition.get_expression_type(scope.clone(), &container).unwrap());

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

                        let mut parameters = parameters.clone();
                        let mut parameter_types = parameter_types.clone();

                        handle_function_named_arguments(
                            project,
                            translated_definition,
                            scope.clone(),
                            external_scope.clone(),
                            member.name.as_str(),
                            named_arguments,
                            &mut parameters,
                            &mut parameter_types,
                        )?;

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
                                
                                // HACK: [u8; 32] -> b256
                                if let Some(32) = value_type_name.u8_array_length() {
                                    if parameter_type_name.is_b256() {
                                        parameters[i] = sway::Expression::from(sway::FunctionCall {
                                            function: sway::Expression::Identifier("b256::from_be_bytes".into()),
                                            generic_parameters: None,
                                            parameters: vec![
                                                parameters[i].clone(),
                                            ],
                                        });
                                        continue 'value_type_check;
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
                                parameters,
                            }));
                        }
                    }

                    panic!(
                        "{}TODO: translate string slice member function call: {function}({})",
                        match project.loc_to_line_and_column(&translated_definition.path, &function.loc()) {
                            Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                            None => format!("{} - ", translated_definition.path.to_string_lossy()),
                        },
                        parameter_types.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "),
                    )
                }

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

use crate::{
    errors::Error,
    project::Project,
    sway,
    translate::{translate_expression, TranslatedDefinition, TranslationScope},
};
use convert_case::Case;
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn resolve_abi_function_call(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &Rc<RefCell<TranslationScope>>,
    abi: &sway::Abi,
    container: &sway::Expression,
    function_name: &str,
    named_arguments: Option<&[solidity::NamedArgument]>,
    mut parameters: Vec<sway::Expression>,
    mut parameter_types: Vec<sway::TypeName>,
) -> Result<Option<sway::Expression>, Error> {
    let mut function = None;

    if let Some(named_arguments) = named_arguments {
        let mut named_parameters = vec![];

        for arg in named_arguments {
            named_parameters.push((
                crate::translate::translate_naming_convention(&arg.name.name, Case::Snake),
                translate_expression(project, translated_definition, scope, &arg.expr)?,
            ));
        }

        if let Some(abi_function) = abi.find_function(|f| {
            f.old_name != function_name
            && f.parameters.entries.len() != named_parameters.len()
            && f.parameters.entries.iter().all(|p| named_parameters.iter().any(|(name, _)| p.name == *name))
        }) {
            let function_parameters = &abi_function.parameters;

            parameters.clear();
            parameter_types.clear();

            for parameter in function_parameters.entries.iter() {
                let arg = named_arguments.iter()
                    .find(|a| {
                        let new_name =
                            crate::translate::translate_naming_convention(&a.name.name, Case::Snake);
                        new_name == parameter.name
                    })
                    .unwrap();

                let parameter = translate_expression(project, translated_definition, scope, &arg.expr)?;
                let parameter_type = translated_definition.get_expression_type(scope, &parameter)?;

                parameters.push(parameter);
                parameter_types.push(parameter_type);
            }

            function = Some(abi_function);
        }
    }

    let parameters_cell = Rc::new(RefCell::new(parameters));

    if function.is_none() {
        function = abi.find_function(|function| {
            let function_parameters = &function.parameters;
            let mut parameters = parameters_cell.borrow_mut();
    
            // Ensure the function's old name matches the function call we're translating
            if function.old_name != function_name {
                return false;
            }
    
            // Ensure the supplied function call args match the function's parameters
            if parameters.len() != function_parameters.entries.len() {
                return false;
            }
    
            for (i, value_type_name) in parameter_types.iter().enumerate() {
                let Some(parameter_type_name) = function_parameters.entries[i].type_name.as_ref()
                else {
                    continue;
                };
    
                //
                // If `parameter_type_name` is `Identity`, but `container` is an abi cast expression,
                // then we need to de-cast it, so `container` turns into the 2nd parameter of the abi cast,
                // and `value_type_name` turns into `Identity`.
                //
    
                if let sway::TypeName::Identifier {
                    name: parameter_type_name,
                    generic_parameters: None,
                } = parameter_type_name {
                    match parameter_type_name.as_str() {
                        "Bytes" => match value_type_name {
                            sway::TypeName::StringSlice => {
                                // Bytes::from(raw_slice::from_parts::<u8>(s.as_ptr(), s.len()))
                                parameters[i] = sway::Expression::create_function_calls(
                                    None,
                                    &[(
                                        "Bytes::from",
                                        Some((
                                            None,
                                            vec![sway::Expression::create_function_calls(
                                                None,
                                                &[(
                                                    "raw_slice::from_parts",
                                                    Some((
                                                        Some(sway::GenericParameterList {
                                                            entries: vec![sway::GenericParameter {
                                                                type_name: sway::TypeName::Identifier {
                                                                    name: "u8".into(),
                                                                    generic_parameters: None,
                                                                },
                                                                implements: None,
                                                            }],
                                                        }),
                                                        vec![
                                                            sway::Expression::create_function_calls(
                                                                Some(parameters[i].clone()),
                                                                &[("as_ptr", Some((None, vec![])))],
                                                            ),
                                                            sway::Expression::create_function_calls(
                                                                Some(parameters[i].clone()),
                                                                &[("len", Some((None, vec![])))],
                                                            ),
                                                        ],
                                                    )),
                                                )],
                                            )],
                                        )),
                                    )],
                                );
                                continue;
                            }
                            _ => {}
                        },
    
                        "Identity" => {
                            if let sway::Expression::FunctionCall(function_call) = parameters[i].clone()
                            {
                                if let sway::Expression::Identifier(function_name) =
                                    &function_call.function
                                {
                                    if function_name == "abi" {
                                        parameters[i] = function_call.parameters[1].clone();
                                        continue;
                                    }
                                }
                            }
                        }
    
                        _ => {}
                    }
                }
    
                // HACK: StorageKey<*> -> *
                if let Some(value_type) = value_type_name.storage_key_type() {
                    if !parameter_type_name.is_storage_key()
                        && parameter_type_name.is_compatible_with(&value_type)
                    {
                        parameters[i] = sway::Expression::create_function_calls(
                            Some(parameters[i].clone()),
                            &[("read", Some((None, vec![])))],
                        );
                        continue;
                    }
                }
    
                // HACK: [u8; 32] -> b256
                if let Some(32) = value_type_name.u8_array_length() {
                    if parameter_type_name.is_b256() {
                        parameters[i] = sway::Expression::create_function_calls(
                            None,
                            &[(
                                "b256::from_be_bytes",
                                Some((None, vec![parameters[i].clone()])),
                            )],
                        );
                        continue;
                    }
                }
    
                // HACK: [u*; N]
                if let sway::TypeName::Array { type_name: value_element_type, length: value_element_length } = &value_type_name {
                    if let sway::TypeName::Array { type_name: parameter_element_type, length: parameter_element_length } = parameter_type_name {
                        if value_element_length != parameter_element_length {
                            return false;
                        }
                        
                        if value_element_type.is_uint() && parameter_element_type.is_uint() {
                            return true;
                        }
    
                        if value_element_type.is_compatible_with(parameter_element_type) {
                            return true;
                        }
                    }
                }
    
                if let Some(expr) = coerce_expression(&parameters[i], value_type_name, parameter_type_name) {
                    parameters[i] = expr;
                    continue;
                } 

                if !value_type_name.is_compatible_with(parameter_type_name) {
                    return false;
                }
            }
    
            true
        });
    }

    let Some(function) = function else {
        return Ok(None);
    };

    *translated_definition.function_call_counts
        .entry(function.name.clone())
        .or_insert(0) += 1;

    translated_definition.functions_called
        .entry(
            translated_definition
                .current_functions
                .last()
                .cloned()
                .unwrap(),
        )
        .or_insert_with(|| Vec::new())
        .push(function.name.clone());

    let parameters = parameters_cell.borrow().clone();

    Ok(Some(sway::Expression::create_function_calls(
        Some(sway::Expression::create_function_calls(None, &[
            ("abi", Some((None, vec![
                sway::Expression::Identifier(abi.name.clone()),
                sway::Expression::create_function_calls(Some(container.clone()), &[
                    ("as_contract_id", Some((None, vec![]))),
                    ("unwrap", Some((None, vec![]))),
                    ("into", Some((None, vec![])))
                ]),
            ]))),
        ])),
        &[(
            function.name.as_str(),
            Some((None, parameters)),
        )],
    )))
}

#[inline]
pub fn resolve_function_call(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &Rc<RefCell<TranslationScope>>,
    external_scope: &Rc<RefCell<TranslationScope>>,
    function_name: &str,
    named_arguments: Option<&[solidity::NamedArgument]>,
    mut parameters: Vec<sway::Expression>,
    mut parameter_types: Vec<sway::TypeName>,
) -> Result<Option<sway::Expression>, Error> {
    if let Some(named_arguments) = named_arguments {
        let mut named_parameters = vec![];

        for arg in named_arguments {
            named_parameters.push((
                crate::translate::translate_naming_convention(&arg.name.name, Case::Snake),
                translate_expression(project, translated_definition, scope, &arg.expr)?,
            ));
        }

        if let Some(function) = external_scope.borrow().find_function(|f| {
            let f = f.borrow();

            let sway::TypeName::Function {
                parameters: f_parameters,
                ..
            } = &f.type_name
            else {
                panic!("Invalid function type name: {:#?}", f.type_name)
            };

            if f.old_name != function_name {
                return false;
            }

            if f_parameters.entries.len() != named_parameters.len() {
                return false;
            }

            f_parameters
                .entries
                .iter()
                .all(|p| named_parameters.iter().any(|(name, _)| p.name == *name))
        }) {
            let function = function.borrow();

            let sway::TypeName::Function {
                parameters: function_parameters,
                ..
            } = &function.type_name
            else {
                panic!("Invalid function type name: {:#?}", function.type_name)
            };

            parameters.clear();
            parameter_types.clear();

            for parameter in function_parameters.entries.iter() {
                let arg = named_arguments
                    .iter()
                    .find(|a| {
                        let new_name =
                            crate::translate::translate_naming_convention(&a.name.name, Case::Snake);
                        new_name == parameter.name
                    })
                    .unwrap();

                let parameter = translate_expression(project, translated_definition, scope, &arg.expr)?;
                let parameter_type = translated_definition.get_expression_type(scope, &parameter)?;

                parameters.push(parameter);
                parameter_types.push(parameter_type);
            }
        } else if let Some(variable) = external_scope.borrow().find_variable(|f| {
            if f.borrow().old_name != function_name {
                return false;
            }

            if let sway::TypeName::Function { .. } = &f.borrow().type_name {
                return true;
            }

            false
        }) {
            let variable = variable.borrow();
            let sway::TypeName::Function {
                parameters: fn_parameters,
                ..
            } = &variable.type_name
            else {
                unreachable!()
            };

            parameters.clear();
            parameter_types.clear();

            for parameter in fn_parameters.entries.iter() {
                let arg = named_arguments
                    .iter()
                    .find(|a| {
                        let new_name =
                            crate::translate::translate_naming_convention(&a.name.name, Case::Snake);
                        new_name == parameter.name
                    })
                    .unwrap();

                let parameter = translate_expression(project, translated_definition, scope, &arg.expr)?;
                let parameter_type = translated_definition.get_expression_type(scope, &parameter)?;

                parameters.push(parameter);
                parameter_types.push(parameter_type);
            }
        }
    }

    let parameters_cell = Rc::new(RefCell::new(parameters));

    if let Some(function) = external_scope.borrow().find_function(|function| {
        let function = function.borrow();

        let sway::TypeName::Function {
            parameters: function_parameters,
            ..
        } = &function.type_name
        else {
            panic!("Invalid function type name: {:#?}", function.type_name)
        };

        let mut parameters = parameters_cell.borrow_mut();

        // Ensure the function's old name matches the function call we're translating
        if function.old_name != function_name {
            return false;
        }

        // Ensure the supplied function call args match the function's parameters
        if parameters.len() != function_parameters.entries.len() {
            return false;
        }

        for (i, value_type_name) in parameter_types.iter().enumerate() {
            let Some(parameter_type_name) = function_parameters.entries[i].type_name.as_ref()
            else {
                continue;
            };

            //
            // If `parameter_type_name` is `Identity`, but `container` is an abi cast expression,
            // then we need to de-cast it, so `container` turns into the 2nd parameter of the abi cast,
            // and `value_type_name` turns into `Identity`.
            //

            if let sway::TypeName::Identifier {
                name: parameter_type_name,
                generic_parameters: None,
            } = parameter_type_name
            {
                match parameter_type_name.as_str() {
                    "Bytes" => match value_type_name {
                        sway::TypeName::StringSlice => {
                            // Bytes::from(raw_slice::from_parts::<u8>(s.as_ptr(), s.len()))
                            parameters[i] = sway::Expression::create_function_calls(
                                None,
                                &[(
                                    "Bytes::from",
                                    Some((
                                        None,
                                        vec![sway::Expression::create_function_calls(
                                            None,
                                            &[(
                                                "raw_slice::from_parts",
                                                Some((
                                                    Some(sway::GenericParameterList {
                                                        entries: vec![sway::GenericParameter {
                                                            type_name: sway::TypeName::Identifier {
                                                                name: "u8".into(),
                                                                generic_parameters: None,
                                                            },
                                                            implements: None,
                                                        }],
                                                    }),
                                                    vec![
                                                        sway::Expression::create_function_calls(
                                                            Some(parameters[i].clone()),
                                                            &[("as_ptr", Some((None, vec![])))],
                                                        ),
                                                        sway::Expression::create_function_calls(
                                                            Some(parameters[i].clone()),
                                                            &[("len", Some((None, vec![])))],
                                                        ),
                                                    ],
                                                )),
                                            )],
                                        )],
                                    )),
                                )],
                            );
                            continue;
                        }
                        _ => {}
                    },

                    "Identity" => {
                        if let sway::Expression::FunctionCall(function_call) = parameters[i].clone()
                        {
                            if let sway::Expression::Identifier(function_name) =
                                &function_call.function
                            {
                                if function_name == "abi" {
                                    parameters[i] = function_call.parameters[1].clone();
                                    continue;
                                }
                            }
                        }
                    }

                    _ => {}
                }
            }

            // HACK: StorageKey<*> -> *
            if let Some(value_type) = value_type_name.storage_key_type() {
                if !parameter_type_name.is_storage_key()
                    && parameter_type_name.is_compatible_with(&value_type)
                {
                    parameters[i] = sway::Expression::create_function_calls(
                        Some(parameters[i].clone()),
                        &[("read", Some((None, vec![])))],
                    );
                    continue;
                }
            }

            // HACK: [u8; 32] -> b256
            if let Some(32) = value_type_name.u8_array_length() {
                if parameter_type_name.is_b256() {
                    parameters[i] = sway::Expression::create_function_calls(
                        None,
                        &[(
                            "b256::from_be_bytes",
                            Some((None, vec![parameters[i].clone()])),
                        )],
                    );
                    continue;
                }
            }

            // HACK: [u*; N]
            if let sway::TypeName::Array { type_name: value_element_type, length: value_element_length } = &value_type_name {
                if let sway::TypeName::Array { type_name: parameter_element_type, length: parameter_element_length } = parameter_type_name {
                    if value_element_length != parameter_element_length {
                        return false;
                    }
                    
                    if value_element_type.is_uint() && parameter_element_type.is_uint() {
                        return true;
                    }

                    if value_element_type.is_compatible_with(parameter_element_type) {
                        return true;
                    }
                }
            }

            if let Some(expr) = coerce_expression(&parameters[i], value_type_name, parameter_type_name) {
                parameters[i] = expr;
                continue;
            } 

            if !value_type_name.is_compatible_with(parameter_type_name) {
                return false;
            }
        }

        true
    }) {
        let function = function.borrow();

        *translated_definition
            .function_call_counts
            .entry(function.new_name.clone())
            .or_insert(0) += 1;
        translated_definition
            .functions_called
            .entry(
                translated_definition
                    .current_functions
                    .last()
                    .cloned()
                    .unwrap(),
            )
            .or_insert_with(|| Vec::new())
            .push(function.new_name.clone());

        return Ok(Some(sway::Expression::create_function_calls(
            None,
            &[(
                function.new_name.as_str(),
                Some((None, parameters_cell.borrow().clone())),
            )],
        )));
    } else if let Some(variable) = external_scope.borrow().find_variable(|v| {
        if v.borrow().old_name != function_name {
            return false;
        }

        let sway::TypeName::Function {
            parameters: fn_parameters,
            ..
        } = &v.borrow().type_name
        else {
            return false;
        };

        let mut parameters = parameters_cell.borrow_mut();

        // Ensure the function's old name matches the function call we're translating
        if v.borrow().old_name != function_name {
            return false;
        }

        // Ensure the supplied function call args match the function's parameters
        if parameters.len() != fn_parameters.entries.len() {
            return false;
        }

        for (i, value_type_name) in parameter_types.iter().enumerate() {
            let Some(parameter_type_name) = fn_parameters.entries[i].type_name.as_ref() else {
                continue;
            };

            //
            // If `parameter_type_name` is `Identity`, but `container` is an abi cast expression,
            // then we need to de-cast it, so `container` turns into the 2nd parameter of the abi cast,
            // and `value_type_name` turns into `Identity`.
            //

            if let sway::TypeName::Identifier {
                name: parameter_type_name,
                generic_parameters: None,
            } = parameter_type_name
            {
                match parameter_type_name.as_str() {
                    "Bytes" => match value_type_name {
                        sway::TypeName::StringSlice => {
                            // Bytes::from(raw_slice::from_parts::<u8>(s.as_ptr(), s.len()))
                            parameters[i] = sway::Expression::create_function_calls(
                                None,
                                &[(
                                    "Bytes::from",
                                    Some((
                                        None,
                                        vec![sway::Expression::create_function_calls(
                                            None,
                                            &[(
                                                "raw_slice::from_parts",
                                                Some((
                                                    Some(sway::GenericParameterList {
                                                        entries: vec![sway::GenericParameter {
                                                            type_name: sway::TypeName::Identifier {
                                                                name: "u8".into(),
                                                                generic_parameters: None,
                                                            },
                                                            implements: None,
                                                        }],
                                                    }),
                                                    vec![
                                                        sway::Expression::create_function_calls(
                                                            Some(parameters[i].clone()),
                                                            &[("as_ptr", Some((None, vec![])))],
                                                        ),
                                                        sway::Expression::create_function_calls(
                                                            Some(parameters[i].clone()),
                                                            &[("len", Some((None, vec![])))],
                                                        ),
                                                    ],
                                                )),
                                            )],
                                        )],
                                    )),
                                )],
                            );
                            continue;
                        }
                        _ => {}
                    },

                    "Identity" => {
                        if let sway::Expression::FunctionCall(function_call) = parameters[i].clone()
                        {
                            if let sway::Expression::Identifier(function_name) =
                                &function_call.function
                            {
                                if function_name == "abi" {
                                    parameters[i] = function_call.parameters[1].clone();
                                    continue;
                                }
                            }
                        }
                    }

                    _ => {}
                }
            }

            // HACK: StorageKey<*> -> *
            if let Some(value_type) = value_type_name.storage_key_type() {
                if !parameter_type_name.is_storage_key()
                    && parameter_type_name.is_compatible_with(&value_type)
                {
                    parameters[i] = sway::Expression::create_function_calls(
                        Some(parameters[i].clone()),
                        &[("read", Some((None, vec![])))],
                    );
                    continue;
                }
            }

            // HACK: [u8; 32] -> b256
            if let Some(32) = value_type_name.u8_array_length() {
                if parameter_type_name.is_b256() {
                    parameters[i] = sway::Expression::create_function_calls(
                        None,
                        &[(
                            "b256::from_be_bytes",
                            Some((None, vec![parameters[i].clone()])),
                        )],
                    );
                    continue;
                }
            }

            if let Some(expr) = coerce_expression(&parameters[i], value_type_name, parameter_type_name) {
                parameters[i] = expr;
                continue;
            } 

            if !value_type_name.is_compatible_with(parameter_type_name) {
                return false;
            }
        }

        true
    }) {
        let variable = variable.borrow();

        *translated_definition
            .function_call_counts
            .entry(variable.new_name.clone())
            .or_insert(0) += 1;
        translated_definition
            .functions_called
            .entry(
                translated_definition
                    .current_functions
                    .last()
                    .cloned()
                    .unwrap(),
            )
            .or_insert_with(|| Vec::new())
            .push(variable.new_name.clone());

        return Ok(Some(sway::Expression::create_function_calls(
            None,
            &[(
                variable.new_name.as_str(),
                Some((None, parameters_cell.borrow().clone())),
            )],
        )));
    }

    Ok(None)
}

#[inline]
pub fn resolve_struct_constructor(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &Rc<RefCell<TranslationScope>>,
    structs: &[Rc<RefCell<sway::Struct>>],
    struct_name: &str,
    named_arguments: Option<&[solidity::NamedArgument]>,
    mut parameters: Vec<sway::Expression>,
    mut parameter_types: Vec<sway::TypeName>,
) -> Result<Option<sway::Expression>, Error> {
    let Some(struct_definition) = structs.iter().find(|s| s.borrow().name == struct_name) else {
        return Ok(None);
    };

    if parameters.len() != struct_definition.borrow().fields.len() {
        if let Some(named_arguments) = named_arguments {
            if named_arguments.len() != struct_definition.borrow().fields.len() {
                return Ok(None);
            } else {
                parameters = vec![];
                parameter_types = vec![];

                for field in struct_definition.borrow().fields.iter() {
                    let arg = named_arguments
                        .iter()
                        .find(|a| {
                            let new_name =
                                crate::translate::translate_naming_convention(&a.name.name, Case::Snake);
                            new_name == field.name
                        })
                        .unwrap();

                    let parameter = translate_expression(
                        project,
                        translated_definition,
                        scope,
                        &arg.expr,
                    )?;
                    let parameter_type =
                        translated_definition.get_expression_type(scope, &parameter)?;

                    parameters.push(parameter);
                    parameter_types.push(parameter_type);
                }
            }
        } else {
            return Ok(None);
        }
    }

    // Attempt to coerce each parameter value to the struct's field type
    for ((parameter, parameter_type), field) in parameters
        .iter_mut()
        .zip(parameter_types.iter_mut())
        .zip(struct_definition.borrow().fields.iter())
    {
        match coerce_expression(parameter, parameter_type, &field.type_name) {
            Some(expression) => *parameter = expression,
            None => return Ok(None),
        }
    }

    translated_definition.ensure_struct_included(project, &struct_definition.clone());

    Ok(Some(sway::Expression::from(sway::Constructor {
        type_name: sway::TypeName::Identifier {
            name: struct_definition.borrow().name.clone(),
            generic_parameters: None,
        },

        fields: struct_definition
            .borrow()
            .fields
            .iter()
            .zip(parameters.iter())
            .map(|(field, value)| sway::ConstructorField {
                name: field.name.clone(),
                value: value.clone(),
            })
            .collect(),
    })))
}

/// Matches bits for numeric types
pub fn match_bits(bits: usize, signed: bool) -> Option<usize> {
    let (name, ty, ident) = if signed {
        ("signed", "int", "I")
    } else {
        ("unsigned", "uint", "U")
    };
    match bits {
        0..=8 => {
            if bits != 8 {
                eprintln!(
                    "WARNING: unsupported {name} integer type `{ty}{bits}`, using `{ident}8`..."
                );
            }
            Some(8)
        }
        9..=16 => {
            if bits != 16 {
                eprintln!(
                    "WARNING: unsupported {name} integer type `{ty}{bits}`, using `{ident}16`..."
                );
            }
            Some(16)
        }
        17..=32 => {
            if bits != 32 {
                eprintln!(
                    "WARNING: unsupported {name} integer type `{ty}{bits}`, using `{ident}32`..."
                );
            }
            Some(32)
        }
        33..=64 => {
            if bits != 64 {
                eprintln!(
                    "WARNING: unsupported {name} integer type `{ty}{bits}`, using `{ident}64`..."
                );
            }
            Some(64)
        }
        65..=128 => {
            if bits != 128 {
                eprintln!(
                    "WARNING: unsupported {name} integer type `{ty}{bits}`, using `{ident}128`..."
                );
            }
            Some(128)
        }
        129..=256 => {
            if bits != 256 {
                eprintln!(
                    "WARNING: unsupported {name} integer type `{ty}{bits}`, using `{ident}256`..."
                );
            }
            Some(256)
        }
        _ => None,
    }
}

/// Coerces a argument type to a parameter type
pub fn coerce_expression(
    expression: &sway::Expression,
    from_type_name: &sway::TypeName,
    to_type_name: &sway::TypeName,
) -> Option<sway::Expression> {
    if from_type_name.is_compatible_with(to_type_name) {
        return Some(expression.clone());
    }
    
    let is_uint = from_type_name.is_uint();
    let is_int = from_type_name.is_int();

    let mut expression = expression.clone();

    //
    // If `to_type_name` is `Identity`, but `expression` is an abi cast expression,
    // then we need to de-cast it, so `expression` turns into the 2nd parameter of the abi cast,
    // and `from_type_name` turns into `Identity`.
    //
    if let (_, sway::TypeName::Identifier { name, generic_parameters }) =  (from_type_name, to_type_name) {
        match (name.as_str(), generic_parameters.as_ref()) {
            ("Identity", None) => match &expression {
                sway::Expression::FunctionCall(f) => {
                    if let sway::Expression::Identifier(ident) = &f.function {
                        if ident == "abi" && f.parameters.len() == 2 {
                            let rhs = f.parameters[1].clone();
                            if let sway::Expression::FunctionCall(f) = &rhs {
                                if let sway::Expression::MemberAccess(e) = &f.function {
                                    if e.member == "into" {
                                        if let sway::Expression::FunctionCall(f) = &e.expression {
                                            if let sway::Expression::MemberAccess(e) = &f.function {
                                                if e.member == "unwrap" {
                                                    if let sway::Expression::FunctionCall(f) = &e.expression {
                                                        if let sway::Expression::MemberAccess(e) = &f.function {
                                                            if e.member == "as_contract_id" {
                                                                return Some(e.expression.clone());
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                _ => {},
            }

            _ => {}
        }
    }

    match (from_type_name, to_type_name) {
        (sway::TypeName::Undefined, sway::TypeName::Undefined) => {},

        (sway::TypeName::Identifier {
            name: lhs_name,
            generic_parameters: lhs_generic_parameters,
        }, sway::TypeName::Identifier {
            name: rhs_name,
            generic_parameters: rhs_generic_parameters,
        }) => {
            if lhs_generic_parameters.is_some() != rhs_generic_parameters.is_some() {
                // From `StorageKey<T>` to `T`
                if let Some(storage_key_type) = from_type_name.storage_key_type() {
                    if !storage_key_type.is_compatible_with(to_type_name) {
                        return None;
                    }
                    
                    expression = sway::Expression::create_function_calls(Some(expression.clone()), &[
                        ("read", Some((None, vec![]))),
                    ]);
                } else {
                    return None;
                }
            }

            if let (Some(lhs_generic_parameters), Some(rhs_generic_parameters)) = (lhs_generic_parameters.as_ref(), rhs_generic_parameters.as_ref()) {
                if lhs_generic_parameters.entries.len() != rhs_generic_parameters.entries.len() {
                    return None;
                }
            }

            if lhs_name == rhs_name {
                return Some(expression.clone());
            }

            // From uint to int
            if from_type_name.is_uint() && !to_type_name.is_uint() {
                if to_type_name.is_int() {
                    let lhs_bits: usize = lhs_name.trim_start_matches('u').trim_start_matches('U').trim_start_matches('I').parse().unwrap();
                    let rhs_bits: usize = rhs_name.trim_start_matches('u').trim_start_matches('U').trim_start_matches('I').parse().unwrap();
                    
                    if lhs_bits > rhs_bits {
                        expression = sway::Expression::create_function_calls(None, &[
                            (format!("u{rhs_bits}::try_from").as_str(), Some((None, vec![expression.clone()]))),
                            ("unwrap", Some((None, vec![])))
                        ]);
                            
                    } else if lhs_bits < rhs_bits {
                        expression = sway::Expression::create_function_calls(Some(expression.clone()), &[
                            (format!("as_u{rhs_bits}").as_str(), Some((None, vec![]))),
                        ]);
                    }

                    expression = sway::Expression::create_function_calls(None, &[
                        (format!("I{rhs_bits}::from_uint").as_str(), Some((None, vec![expression.clone()]))),
                    ]);
                } else {
                    return None;
                }
            }
            
            // From int to uint
            if is_int && !to_type_name.is_int() {
                if to_type_name.is_uint() {
                    let lhs_bits: usize = lhs_name.trim_start_matches('u').trim_start_matches('U').trim_start_matches('I').parse().unwrap();
                    let rhs_bits: usize = rhs_name.trim_start_matches('u').trim_start_matches('U').trim_start_matches('I').parse().unwrap();
                    
                    
                    if lhs_bits > rhs_bits {
                        expression = sway::Expression::create_function_calls(None, &[
                            (format!("u{rhs_bits}::try_from").as_str(), Some((None, vec![
                                sway::Expression::from(sway::MemberAccess{ expression: expression.clone(), member: "underlying".to_string() })
                            ]))),
                            ("unwrap", Some((None, vec![])))
                        ]);
                            
                    } else if lhs_bits < rhs_bits {
                        expression = sway::Expression::create_function_calls(Some(expression.clone()), &[
                            ("underlying", None),
                            (format!("as_u{rhs_bits}").as_str(), Some((None, vec![]))),
                        ]);
                    }

                    expression = sway::Expression::create_function_calls(None, &[
                        (format!("I{rhs_bits}::from_uint").as_str(), Some((None, vec![expression.clone()]))),
                    ]);
                } else {
                    return None;
                }
            }
            
            // From uint/int of different bit lengths
            if (is_uint && to_type_name.is_uint()) || (is_int && to_type_name.is_int()) {
                let lhs_bits: usize = lhs_name.trim_start_matches('u').trim_start_matches('U').trim_start_matches('I').parse().unwrap();
                let rhs_bits: usize = rhs_name.trim_start_matches('u').trim_start_matches('U').trim_start_matches('I').parse().unwrap();

                match &expression {
                    sway::Expression::Literal(sway::Literal::DecInt(i, suffix)) => {
                        if suffix.is_none() {
                            expression = sway::Expression::Literal(sway::Literal::DecInt(
                                i.clone(), 
                                Some(format!("{}{}", rhs_name.chars().nth(0).unwrap(), rhs_bits))
                            ));
                        } else {
                            if lhs_bits > rhs_bits {
                                // x.as_u256()
                                // u64::try_from(x).unwrap()
                                expression = sway::Expression::create_function_calls(None, &[
                                    (format!("{to_type_name}::try_from").as_str(), Some((None, vec![expression.clone()]))),
                                    ("unwrap", Some((None, vec![])))
                                ]);
                            } else if lhs_bits < rhs_bits {
                                expression = sway::Expression::create_function_calls(Some(expression.clone()), &[
                                    (format!("as_{to_type_name}").as_str(), Some((None, vec![]))),
                                ]);
                            }
                        }
                    }
                    _ => if lhs_bits > rhs_bits {
                        // x.as_u256()
                        // u64::try_from(x).unwrap()
                        expression = sway::Expression::create_function_calls(None, &[
                            (format!("{to_type_name}::try_from").as_str(), Some((None, vec![expression.clone()]))),
                            ("unwrap", Some((None, vec![])))
                        ]);
                    } else if lhs_bits < rhs_bits {
                        expression = sway::Expression::create_function_calls(Some(expression.clone()), &[
                            (format!("as_{to_type_name}").as_str(), Some((None, vec![]))),
                        ]);
                    }
                }
            }
        },

        (sway::TypeName::Array {
            type_name: lhs_type_name,
            length: lhs_len,
        }, sway::TypeName::Array {
            type_name: rhs_type_name,
            length: rhs_len,
        }) => {
            if lhs_len != rhs_len || !lhs_type_name.is_compatible_with(rhs_type_name) {
                todo!("Handle conversion from {from_type_name} to {to_type_name}")
            }

            match expression {
                sway::Expression::Array(array) => {
                    expression = sway::Expression::from(sway::Array {
                        elements: array.elements.iter().map(|e| {
                            coerce_expression(e, lhs_type_name, rhs_type_name).unwrap()
                        }).collect(),
                    });
                }

                _ => {
                    expression = sway::Expression::from(sway::Array {
                        elements: (0..*rhs_len).map(|i| {
                            coerce_expression(
                                &sway::Expression::from(sway::ArrayAccess {
                                    expression: expression.clone(),
                                    index: sway::Expression::from(sway::Literal::DecInt(i.into(), None)),
                                }),
                                lhs_type_name,
                                rhs_type_name,
                            ).unwrap()
                        }).collect()
                    });
                }
            }
        }

        (sway::TypeName::Tuple {
            type_names: lhs_type_names,
        },sway::TypeName::Tuple {
            type_names: rhs_type_names,
        },)=> {
            match expression {
                sway::Expression::Identifier(_) => {
                    let component_names = ('a'..='z')
                        .enumerate()
                        .take_while(|(i, _)| *i < lhs_type_names.len())
                        .map(|(_, c)| sway::LetIdentifier{ is_mutable: false, name: c.to_string() })
                        .collect::<Vec<_>>();
                    
                    let let_stmt = sway::Statement::from(sway::Let {
                        pattern: sway::LetPattern::Tuple(component_names.clone()),
                        type_name: None,
                        value: expression.clone(),
                    });

                    let exprs = component_names.iter().enumerate()
                        .map(|(i, c)| {
                            let expr = sway::Expression::Identifier(c.name.clone());
                            coerce_expression(&expr, &lhs_type_names[i], &rhs_type_names[i])
                        })
                        .collect::<Vec<_>>();
                    
                    if exprs.iter().any(|x| x.is_none()) {
                        return None;
                    }

                    expression = sway::Expression::from(sway::Block {
                        statements: vec![let_stmt],
                        final_expr: Some(sway::Expression::Tuple(exprs.iter().flatten().cloned().collect())),
                    });
                }, 

                sway::Expression::Tuple(expressions) => {
                    let mut expressions = expressions.clone();

                    if expressions.len() != rhs_type_names.len() {
                        return None;
                    }
                    
                    for (i, (lhs, rhs)) in lhs_type_names.iter().zip(rhs_type_names).enumerate() {
                        match coerce_expression(&expressions[i], lhs, rhs) {
                            Some(expr) => expressions[i] = expr,
                            None => return None,
                        }
                    }

                    expression = sway::Expression::Tuple(expressions);
                },
                
                _ => {
                    return None;
                }
            }
        },

        (sway::TypeName::StringSlice, sway::TypeName::StringSlice) => {},

        (sway::TypeName::StringArray {
            length: lhs_len,
        }, sway::TypeName::StringArray {
            length: rhs_len,
        }) => {
            if lhs_len != rhs_len {
                todo!("Handle coersion from str[{lhs_len}] to str[{rhs_len}]")
            }

            // otherwise it's the same length and we don't need to do anything
        }

        (sway::TypeName::Function {
            generic_parameters: _lhs_generic_parameters,
            parameters: _lhs_parameters_list,
            return_type: _lhs_return_type,
        }, sway::TypeName::Function {
            generic_parameters: _rhs_generic_parameters,
            parameters: _rhs_parameters_list,
            return_type: _rhs_return_type,
        }) => todo!(),

        (_, sway::TypeName::Identifier { name, generic_parameters }) => match (name.as_str(), generic_parameters.as_ref()) {
            ("Bytes", None) => match from_type_name {
                sway::TypeName::StringSlice => {
                    // Bytes::from(raw_slice::from_parts::<u8>((s.as_ptr(), s.len())))
                    expression = sway::Expression::create_function_calls(None, &[
                        ("Bytes::from", Some((None, vec![
                            sway::Expression::create_function_calls(None, &[
                                ("raw_slice::from_parts", Some((Some(sway::GenericParameterList{ 
                                    entries: vec![
                                        sway::GenericParameter { 
                                            type_name: sway::TypeName::Identifier { 
                                                name: "u8".into(), 
                                                generic_parameters: None, 
                                            }, 
                                            implements: None 
                                        }
                                    ] 
                                }), vec![
                                    sway::Expression::create_function_calls(Some(expression.clone()), &[
                                        ("as_ptr", Some((None, vec![])))
                                    ]),
                                    sway::Expression::create_function_calls(Some(expression.clone()), &[
                                        ("len", Some((None, vec![])))
                                    ])
                                ])))
                            ])
                        ])))
                    ]);
                },
                
                _ => return None,
            }

            ("String", None) => match from_type_name {
                sway::TypeName::Identifier { name, generic_parameters } => match (name.as_str(), generic_parameters.as_ref()) {
                    ("todo!", None) => {}
                    ("String", None) => {}
                    _ => todo!(),
                }
                
                // String::from_ascii_str(x)
                sway::TypeName::StringSlice => {
                    expression = sway::Expression::from(sway::FunctionCall{
                        function: sway::Expression::Identifier("String::from_ascii_str".into()),
                        generic_parameters: None,
                        parameters: vec![
                            expression.clone(),
                        ],
                    });
                }
                
                // String::from_ascii_str(from_str_array(x))
                sway::TypeName::StringArray { .. } => {
                    expression = sway::Expression::from(sway::FunctionCall{
                        function: sway::Expression::Identifier("String::from_ascii_str".into()),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier("from_str_array".into()),
                                generic_parameters: None,
                                parameters: vec![
                                    expression.clone(),
                                ],
                            }),
                        ],
                    });
                }
                
                _ => todo!("{}", sway::TabbedDisplayer(&to_type_name)),
            }
            
            _ => return None,
        }
        
        _ => return None,
    }

    Some(expression)
}

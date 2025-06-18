use crate::{ir, project::Project, sway, translate::*};
use std::{cell::RefCell, rc::Rc};

#[derive(Clone, Debug)]
pub enum Symbol {
    TypeDefinition(String),
    Enum(String),
    Event(String),
    Error(String),
    Struct(String),
}

#[derive(Clone, Debug)]
pub enum SymbolData {
    TypeDefinition(sway::TypeDefinition),
    Enum(ir::Enum),
    Event {
        type_name: sway::TypeName,
        variant: sway::EnumVariant,
    },
    Error {
        type_name: sway::TypeName,
        variant: sway::EnumVariant,
    },
    Struct(Rc<RefCell<sway::Struct>>),
}

pub fn resolve_symbol(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    symbol: Symbol,
) -> Option<SymbolData> {
    match &symbol {
        Symbol::TypeDefinition(name) => {
            if let Some(type_definition) = module
                .borrow()
                .type_definitions
                .iter()
                .find(|t| t.signature.to_string() == *name)
            {
                if let Some(implementation) = type_definition.implementation.as_ref() {
                    return Some(SymbolData::TypeDefinition(implementation.clone()));
                }
            }
        }

        Symbol::Enum(name) => {
            if let Some(enum_definition) = module
                .borrow()
                .enums
                .iter()
                .find(|e| e.signature.to_string() == *name)
            {
                return Some(SymbolData::Enum(
                    enum_definition.implementation.clone().unwrap(),
                ));
            }
        }

        Symbol::Event(name) => {
            if let Some(event_enum) = module
                .borrow()
                .events_enums
                .iter()
                .find(|e| e.0.borrow().variants.iter().any(|v| v.name == *name))
            {
                let variant = event_enum
                    .0
                    .borrow()
                    .variants
                    .iter()
                    .find(|v| v.name == *name)
                    .cloned()
                    .unwrap();

                return Some(SymbolData::Event {
                    type_name: sway::TypeName::Identifier {
                        name: event_enum.0.borrow().name.clone(),
                        generic_parameters: None,
                    },
                    variant,
                });
            }
        }

        Symbol::Error(name) => {
            if let Some(error_enum) = module
                .borrow()
                .errors_enums
                .iter()
                .find(|e| e.0.borrow().variants.iter().any(|v| v.name == *name))
            {
                let variant = error_enum
                    .0
                    .borrow()
                    .variants
                    .iter()
                    .find(|v| v.name == *name)
                    .cloned()
                    .unwrap();

                return Some(SymbolData::Error {
                    type_name: sway::TypeName::Identifier {
                        name: error_enum.0.borrow().name.clone(),
                        generic_parameters: None,
                    },
                    variant,
                });
            }
        }

        Symbol::Struct(name) => {
            if let Some(struct_definition) = module
                .borrow()
                .structs
                .iter()
                .find(|s| s.signature.to_string() == *name)
            {
                return Some(SymbolData::Struct(
                    struct_definition.implementation.clone().unwrap(),
                ));
            }
        }
    }

    for use_expr in module.borrow().uses.iter() {
        if let Some(imported_module) = project.resolve_use(use_expr) {
            if let Some(symbol) = resolve_symbol(
                project,
                imported_module.clone(),
                scope.clone(),
                symbol.clone(),
            ) {
                return Some(symbol);
            }
        }
    }

    None
}

#[inline]
pub fn resolve_abi_function_call(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    abi: &sway::Abi,
    contract_id: Option<&sway::Expression>,
    function_name: &str,
    named_arguments: Option<&[solidity::NamedArgument]>,
    mut parameters: Vec<sway::Expression>,
    mut parameter_types: Vec<sway::TypeName>,
) -> Result<Option<sway::Expression>, Error> {
    let functions = match contract_id {
        Some(_) => abi.functions.clone(),
        None => module
            .borrow()
            .functions
            .iter()
            .map(|f| {
                let sway::TypeName::Function {
                    old_name,
                    new_name,
                    generic_parameters,
                    parameters,
                    return_type,
                } = &f.signature
                else {
                    unreachable!()
                };
                sway::Function {
                    attributes: None,
                    is_public: false,
                    old_name: old_name.clone(),
                    name: new_name.clone(),
                    generic_parameters: generic_parameters.clone(),
                    parameters: parameters.clone(),
                    return_type: return_type.as_ref().map(|x| x.as_ref().clone()),
                    body: None,
                }
            })
            .collect(),
    };

    let mut function = None;

    if let Some(named_arguments) = named_arguments {
        let mut named_parameters = vec![];

        for arg in named_arguments {
            named_parameters.push((
                translate_naming_convention(&arg.name.name, Case::Snake),
                translate_expression(project, module.clone(), scope.clone(), &arg.expr)?,
            ));
        }

        if let Some(abi_function) = functions.iter().find(|f| {
            f.old_name != function_name
                && f.parameters.entries.len() != named_parameters.len()
                && f.parameters
                    .entries
                    .iter()
                    .all(|p| named_parameters.iter().any(|(name, _)| p.name == *name))
        }) {
            let function_parameters = &abi_function.parameters;

            parameters.clear();
            parameter_types.clear();

            for parameter in function_parameters.entries.iter() {
                let arg = named_arguments
                    .iter()
                    .find(|a| {
                        let new_name = translate_naming_convention(&a.name.name, Case::Snake);
                        new_name == parameter.name
                    })
                    .unwrap();

                let parameter =
                    translate_expression(project, module.clone(), scope.clone(), &arg.expr)?;
                let parameter_type =
                    get_expression_type(project, module.clone(), scope.clone(), &parameter)?;

                parameters.push(parameter);
                parameter_types.push(parameter_type);
            }

            function = Some(abi_function);
        }
    }

    let parameters_cell = Rc::new(RefCell::new(parameters));

    if function.is_none() {
        function = functions.iter().find(|function| {
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
                                                                type_name:
                                                                    sway::TypeName::Identifier {
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
                            if let sway::Expression::FunctionCall(function_call) =
                                parameters[i].clone()
                            {
                                if let Some(function_name) = function_call.function.as_identifier()
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
                if let sway::TypeName::Array {
                    type_name: value_element_type,
                    length: value_element_length,
                } = &value_type_name
                {
                    if let sway::TypeName::Array {
                        type_name: parameter_element_type,
                        length: parameter_element_length,
                    } = parameter_type_name
                    {
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

                if let Some(expr) =
                    coerce_expression(&parameters[i], value_type_name, parameter_type_name)
                {
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

    let parameters = parameters_cell.borrow().clone();

    match contract_id.cloned() {
        Some(mut contract_id) => {
            let type_name =
                get_expression_type(project, module.clone(), scope.clone(), &contract_id)?;

            if type_name.is_identity() {
                contract_id = sway::Expression::create_function_calls(
                    None,
                    &[(
                        "abi",
                        Some((
                            None,
                            vec![
                                sway::Expression::create_identifier(abi.name.clone()),
                                sway::Expression::create_function_calls(
                                    Some(contract_id.clone()),
                                    &[
                                        ("as_contract_id", Some((None, vec![]))),
                                        ("unwrap", Some((None, vec![]))),
                                        ("into", Some((None, vec![]))),
                                    ],
                                ),
                            ],
                        )),
                    )],
                );
            }

            Ok(Some(sway::Expression::create_function_calls(
                Some(contract_id),
                &[(function.name.as_str(), Some((None, parameters)))],
            )))
        }

        None => Ok(Some(sway::Expression::create_function_calls(
            None,
            &[(function.name.as_str(), Some((None, parameters)))],
        ))),
    }
}

#[inline]
pub fn resolve_function_call(
    module: Rc<RefCell<ir::Module>>,
    function_name: &str,
    named_arguments: Option<&[solidity::NamedArgument]>,
    parameters: Vec<sway::Expression>,
    parameter_types: Vec<sway::TypeName>,
) -> Result<Option<sway::Expression>, Error> {
    //
    // TODO: handle named arguments
    //

    let parameters_cell = Rc::new(RefCell::new(parameters));

    if let Some(function) = module.borrow().functions.iter().find(|function| {
        let sway::TypeName::Function {
            old_name,
            parameters: function_parameters,
            ..
        } = &function.signature
        else {
            unreachable!()
        };

        let mut parameters = parameters_cell.borrow_mut();

        // Ensure the function's old name matches the function call we're translating
        if old_name != function_name {
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
                            if let Some(function_name) = function_call.function.as_identifier() {
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
            if let sway::TypeName::Array {
                type_name: value_element_type,
                length: value_element_length,
            } = &value_type_name
            {
                if let sway::TypeName::Array {
                    type_name: parameter_element_type,
                    length: parameter_element_length,
                } = parameter_type_name
                {
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

            if let Some(expr) =
                coerce_expression(&parameters[i], value_type_name, parameter_type_name)
            {
                parameters[i] = expr;
                continue;
            }

            if !value_type_name.is_compatible_with(parameter_type_name) {
                return false;
            }
        }

        true
    }) {
        let sway::TypeName::Function { new_name, .. } = &function.signature else {
            unreachable!()
        };

        return Ok(Some(sway::Expression::create_function_calls(
            None,
            &[(&new_name, Some((None, parameters_cell.borrow().clone())))],
        )));
    }

    Ok(None)
}

#[inline]
pub fn resolve_struct_constructor(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    structs: &[ir::Item<Rc<RefCell<sway::Struct>>>],
    struct_name: &str,
    named_arguments: Option<&[solidity::NamedArgument]>,
    mut parameters: Vec<sway::Expression>,
    mut parameter_types: Vec<sway::TypeName>,
) -> Result<Option<sway::Expression>, Error> {
    let Some(struct_definition) = structs
        .iter()
        .find(|s| s.signature.to_string() == struct_name)
    else {
        return Ok(None);
    };

    if parameters.len()
        != struct_definition
            .implementation
            .as_ref()
            .unwrap()
            .borrow()
            .fields
            .len()
    {
        if let Some(named_arguments) = named_arguments {
            if named_arguments.len()
                != struct_definition
                    .implementation
                    .as_ref()
                    .unwrap()
                    .borrow()
                    .fields
                    .len()
            {
                return Ok(None);
            } else {
                parameters = vec![];
                parameter_types = vec![];

                for field in struct_definition
                    .implementation
                    .as_ref()
                    .unwrap()
                    .borrow()
                    .fields
                    .iter()
                {
                    let arg = named_arguments
                        .iter()
                        .find(|a| {
                            let new_name = translate_naming_convention(&a.name.name, Case::Snake);
                            new_name == field.name
                        })
                        .unwrap();

                    let parameter =
                        translate_expression(project, module.clone(), scope.clone(), &arg.expr)?;
                    let parameter_type =
                        get_expression_type(project, module.clone(), scope.clone(), &parameter)?;

                    parameters.push(parameter);
                    parameter_types.push(parameter_type);
                }
            }
        } else {
            return Ok(None);
        }
    }

    // Attempt to coerce each parameter value to the struct's field type
    for ((parameter, parameter_type), field) in
        parameters.iter_mut().zip(parameter_types.iter_mut()).zip(
            struct_definition
                .implementation
                .as_ref()
                .unwrap()
                .borrow()
                .fields
                .iter(),
        )
    {
        match coerce_expression(parameter, parameter_type, &field.type_name) {
            Some(expression) => *parameter = expression,
            None => return Ok(None),
        }
    }

    Ok(Some(sway::Expression::from(sway::Constructor {
        type_name: sway::TypeName::Identifier {
            name: struct_definition
                .implementation
                .as_ref()
                .unwrap()
                .borrow()
                .name
                .clone(),
            generic_parameters: None,
        },

        fields: struct_definition
            .implementation
            .as_ref()
            .unwrap()
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

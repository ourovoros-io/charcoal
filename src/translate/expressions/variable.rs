use crate::{error::Error, project::Project, sway, translate::*};
use convert_case::Case;
use solang_parser::{helpers::CodeLocation, pt as solidity};
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_variable_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
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
        "_" => {
            // Modifier body insertion variable
            return Ok(sway::Expression::create_identifier("_".into()));
        }

        "now" => {
            // now => std::block::timestamp().as_u256()
            return Ok(sway::Expression::create_function_calls(
                None,
                &[
                    ("std::block::timestamp", Some((None, vec![]))),
                    ("as_u256", Some((None, vec![]))),
                ],
            ));
        }

        _ => {}
    }

    let Some(ir::VariableAccess {
        variable,
        expression,
    }) = translate_variable_access_expression(project, module.clone(), scope.clone(), expression)?
    else {
        panic!(
            "{}: ERROR: Variable not found in scope: \"{}\"",
            project.loc_to_file_location_string(module.clone(), &expression.loc()),
            sway::TabbedDisplayer(&expression),
        );
    };

    if let Some(variable) = variable {
        variable.borrow_mut().read_count += 1;
    }

    Ok(expression)
}

pub fn translate_variable_access_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    solidity_expression: &solidity::Expression,
) -> Result<Option<ir::VariableAccess>, Error> {
    // println!(
    //     "{}: Translating variable access expression: {solidity_expression}",
    //     project.loc_to_file_location_string(module.clone(), &solidity_expression.loc()),
    // );

    match solidity_expression {
        solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
            // Attempt to find a value source matching the name of the variable
            if let Some(symbol) = resolve_symbol(
                project,
                module.clone(),
                scope.clone(),
                Symbol::ValueSource(name.into()),
            ) {
                let variable = match &symbol {
                    SymbolData::Variable(variable) => Some(variable.clone()),
                    _ => None,
                };

                let mut expression: sway::Expression = symbol.try_into()?;

                let expression_type =
                    get_expression_type(project, module.clone(), scope.clone(), &expression)?;

                if expression_type.is_storage_key() {
                    scope
                        .borrow_mut()
                        .set_function_storage_accesses(module.clone(), true, false);

                    expression = sway::Expression::create_function_calls(
                        Some(expression),
                        &[("read", Some((None, vec![])))],
                    );
                }

                return Ok(Some(ir::VariableAccess {
                    variable,
                    expression,
                }));
            }

            // Check to see if the variable refers to a function
            if let Some(function) = module.borrow().functions.iter().find(|f| {
                let sway::TypeName::Function { old_name, .. } = &f.signature else {
                    unreachable!()
                };
                old_name == name
            }) {
                return Ok(Some(ir::VariableAccess {
                    variable: None,
                    expression: sway::Expression::create_identifier({
                        let sway::TypeName::Function { new_name, .. } = &function.signature else {
                            unreachable!()
                        };
                        new_name.clone()
                    }),
                }));
            }

            Ok(None)
        }

        solidity::Expression::ArraySubscript(_, array_expression, Some(index)) => {
            let mut index =
                translate_expression(project, module.clone(), scope.clone(), index.as_ref())?;

            let Some(ir::VariableAccess {
                variable,
                mut expression,
            }) = translate_variable_access_expression(
                project,
                module.clone(),
                scope.clone(),
                array_expression,
            )?
            else {
                return Ok(None);
            };

            // HACK: remove `.read()` if present
            if let sway::Expression::FunctionCall(function_call) = &expression
                && let sway::Expression::MemberAccess(member_access) = &function_call.function
                && member_access.member == "read"
                && function_call.parameters.is_empty()
            {
                let container_type = get_expression_type(
                    project,
                    module.clone(),
                    scope.clone(),
                    &member_access.expression,
                )?;

                if container_type.is_storage_key() {
                    expression = member_access.expression.clone();
                }
            }

            let type_name =
                get_expression_type(project, module.clone(), scope.clone(), &expression)?;

            Ok(Some(ir::VariableAccess {
                variable,
                expression: match &type_name {
                    sway::TypeName::Identifier {
                        name,
                        generic_parameters,
                    } => match (name.as_str(), generic_parameters.as_ref()) {
                        ("Bytes", None) => sway::Expression::create_function_calls(
                            Some(expression),
                            &[
                                ("get", Some((None, vec![index]))),
                                ("unwrap", Some((None, vec![]))),
                            ],
                        ),

                        ("Option", Some(generic_parameters))
                            if generic_parameters.entries.len() == 1 =>
                        {
                            if let Some(storage_key_type) =
                                generic_parameters.entries[0].type_name.storage_key_type()
                                && storage_key_type.is_storage_map()
                            {
                                sway::Expression::create_function_calls(
                                    Some(expression),
                                    &[
                                        ("unwrap", Some((None, vec![]))),
                                        ("get", Some((None, vec![index]))),
                                    ],
                                )
                            } else {
                                todo!()
                            }
                        }

                        ("StorageKey", Some(generic_parameters))
                            if generic_parameters.entries.len() == 1 =>
                        {
                            match &generic_parameters.entries[0].type_name {
                                sway::TypeName::Identifier {
                                    name,
                                    generic_parameters,
                                } => match (name.as_str(), generic_parameters.as_ref()) {
                                    ("StorageMap", Some(_)) => {
                                        sway::Expression::create_function_calls(
                                            Some(expression),
                                            &[("get", Some((None, vec![index])))],
                                        )
                                    }

                                    ("StorageVec", Some(_)) => {
                                        let index_type_name = get_expression_type(
                                            project,
                                            module.clone(),
                                            scope.clone(),
                                            &index,
                                        )?;

                                        let u64_type = sway::TypeName::Identifier {
                                            name: "u64".to_string(),
                                            generic_parameters: None,
                                        };

                                        index = coerce_expression(
                                            project,
                                            module.clone(),
                                            scope.clone(),
                                            &index,
                                            &index_type_name,
                                            &u64_type,
                                        )
                                        .unwrap();

                                        sway::Expression::create_function_calls(
                                            Some(expression),
                                            &[
                                                ("get", Some((None, vec![index]))),
                                                ("unwrap", Some((None, vec![]))),
                                            ],
                                        )
                                    }

                                    (name, _) => panic!(
                                        "{}: TODO: translate {name} array subscript expression: {solidity_expression} - {} {expression:#?}",
                                        project.loc_to_file_location_string(
                                            module.clone(),
                                            &solidity_expression.loc()
                                        ),
                                        sway::TabbedDisplayer(&expression),
                                    ),
                                },

                                sway::TypeName::Array { .. } => {
                                    scope.borrow_mut().set_function_storage_accesses(
                                        module.clone(),
                                        true,
                                        false,
                                    );

                                    sway::Expression::from(sway::ArrayAccess {
                                        expression: sway::Expression::create_function_calls(
                                            Some(expression),
                                            &[("read", Some((None, vec![])))],
                                        ),
                                        index,
                                    })
                                }

                                _ => todo!(
                                    "{}: TODO: translate {} array subscript expression: {solidity_expression} - {} {expression:#?}",
                                    project.loc_to_file_location_string(
                                        module.clone(),
                                        &solidity_expression.loc()
                                    ),
                                    type_name,
                                    sway::TabbedDisplayer(&expression),
                                ),
                            }
                        }

                        ("Vec", Some(generic_parameters))
                            if generic_parameters.entries.len() == 1 =>
                        {
                            let index_type_name = get_expression_type(
                                project,
                                module.clone(),
                                scope.clone(),
                                &index,
                            )?;

                            let u64_type = sway::TypeName::Identifier {
                                name: "u64".to_string(),
                                generic_parameters: None,
                            };

                            index = coerce_expression(
                                project,
                                module.clone(),
                                scope.clone(),
                                &index,
                                &index_type_name,
                                &u64_type,
                            )
                            .unwrap();

                            sway::Expression::create_function_calls(
                                Some(expression),
                                &[
                                    ("get", Some((None, vec![index]))),
                                    ("unwrap", Some((None, vec![]))),
                                ],
                            )
                        }

                        (name, _) => todo!(
                            "{}: TODO: translate {name} array subscript expression: {solidity_expression} - {} {expression:#?}",
                            project.loc_to_file_location_string(
                                module.clone(),
                                &solidity_expression.loc()
                            ),
                            sway::TabbedDisplayer(&expression),
                        ),
                    },

                    _ => sway::Expression::from(sway::ArrayAccess { expression, index }),
                },
            }))
        }

        solidity::Expression::MemberAccess(_, container, member) => {
            let mut translated_container =
                translate_expression(project, module.clone(), scope.clone(), container)?;

            let mut container_type_name = get_expression_type(
                project,
                module.clone(),
                scope.clone(),
                &translated_container,
            )?;

            if let Some(container_type) = container_type_name.storage_key_type() {
                scope
                    .borrow_mut()
                    .set_function_storage_accesses(module.clone(), true, false);

                translated_container = sway::Expression::create_function_calls(
                    Some(translated_container),
                    &[("read", Some((None, vec![])))],
                );
                container_type_name = container_type;
            }

            let container_type_name_string = container_type_name.to_string();

            let Some(ir::VariableAccess { variable, .. }) = translate_variable_access_expression(
                project,
                module.clone(),
                scope.clone(),
                container,
            )?
            else {
                return Ok(None);
            };

            // Check if container is a struct
            let field_name = translate_naming_convention(member.name.as_str(), Case::Snake);

            if let Some(struct_definition) =
                project.find_struct(module.clone(), scope.clone(), &container_type_name_string)
                && struct_definition
                    .borrow()
                    .fields
                    .iter()
                    .any(|f| f.new_name == field_name)
            {
                return Ok(Some(ir::VariableAccess {
                    variable,
                    expression: sway::Expression::from(sway::MemberAccess {
                        expression: translated_container,
                        member: field_name,
                    }),
                }));
            }

            panic!(
                "{}: TODO: translate variable {container_type_name_string} member access expression: {solidity_expression}",
                project.loc_to_file_location_string(module.clone(), &solidity_expression.loc()),
            )
        }

        solidity::Expression::FunctionCall(_, function, arguments) => {
            let parameters = arguments
                .iter()
                .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
                .collect::<Result<Vec<_>, _>>()?;

            let parameter_types = parameters
                .iter()
                .map(|p| get_expression_type(project, module.clone(), scope.clone(), p))
                .collect::<Result<Vec<_>, _>>()?;

            // Check for explicit contract function calls
            if let solidity::Expression::MemberAccess(_, container, member) = function.as_ref()
                && let solidity::Expression::Variable(container) = container.as_ref()
                && let Some(external_contract) =
                    project.find_contract(module.clone(), container.name.as_str())
            {
                let abi = external_contract.borrow().abi.clone();

                if let Some(result) = resolve_abi_function_call(
                    project,
                    module.clone(),
                    scope.clone(),
                    &abi,
                    None,
                    member.name.as_str(),
                    None,
                    parameters.clone(),
                    parameter_types.clone(),
                )? {
                    return Ok(Some(ir::VariableAccess {
                        variable: None,
                        expression: result,
                    }));
                }
            }

            match translate_variable_access_expression(
                project,
                module.clone(),
                scope.clone(),
                function,
            )? {
                Some(ir::VariableAccess {
                    variable,
                    expression,
                }) => Ok(Some(ir::VariableAccess {
                    variable,
                    expression: sway::Expression::from(sway::FunctionCall {
                        function: expression,
                        generic_parameters: None,
                        parameters,
                    }),
                })),

                None => Ok(Some(ir::VariableAccess {
                    variable: None,
                    expression: translate_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        solidity_expression,
                    )?,
                })),
            }
        }

        solidity::Expression::Type(_, _) => panic!(
            "type expression as variable access expression: {solidity_expression} - {solidity_expression:#?}"
        ),

        _ => todo!(
            "translate variable access expression: {solidity_expression} - {solidity_expression:#?}"
        ),
    }
}

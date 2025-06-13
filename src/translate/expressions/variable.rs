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
            if let Some(variable) = scope.borrow().get_variable_from_old_name(name) {
                let variable_name = variable.borrow().new_name.clone();

                return Ok(Some(ir::VariableAccess {
                    variable: Some(variable),
                    expression: sway::Expression::create_identifier(variable_name),
                }));
            }

            let mut module = module.borrow_mut();

            if let Some(storage_namespace) = module.get_storage_namespace(scope.clone()) {
                if let Some(field) = storage_namespace
                    .borrow()
                    .fields
                    .iter()
                    .find(|f| f.old_name == *name)
                {
                    return Ok(Some(ir::VariableAccess {
                        variable: None,
                        expression: sway::Expression::create_function_calls(
                            None,
                            &[
                                (
                                    format!("storage::{}", storage_namespace.borrow().name)
                                        .as_str(),
                                    None,
                                ),
                                (field.name.as_str(), None),
                                ("read", Some((None, vec![]))),
                            ],
                        ),
                    }));
                }
            }

            if let Some(function) = module.functions.iter().find(|f| {
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

            if let Some(constant) = module.constants.iter().find(|c| c.old_name == *name) {
                return Ok(Some(ir::VariableAccess {
                    variable: None,
                    expression: sway::Expression::create_identifier(constant.name.clone()),
                }));
            } else if let Some(configurable) = module.configurable.as_ref() {
                if let Some(field) = configurable.fields.iter().find(|f| f.old_name == *name) {
                    return Ok(Some(ir::VariableAccess {
                        variable: None,
                        expression: sway::Expression::create_identifier(field.name.clone()),
                    }));
                }
            }

            return Ok(None);
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
            if let sway::Expression::FunctionCall(function_call) = &expression {
                if let sway::Expression::MemberAccess(member_access) = &function_call.function {
                    if member_access.member == "read" && function_call.parameters.is_empty() {
                        let container_type = module.borrow_mut().get_expression_type(
                            project,
                            scope.clone(),
                            &member_access.expression,
                        )?;

                        if container_type.is_storage_key() {
                            expression = member_access.expression.clone();
                        }
                    }
                }
            }

            let type_name =
                module
                    .borrow_mut()
                    .get_expression_type(project, scope.clone(), &expression)?;

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
                            {
                                if storage_key_type.is_storage_map() {
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
                                        let index_type_name = module
                                            .borrow_mut()
                                            .get_expression_type(project, scope.clone(), &index)?;
                                        let u64_type = sway::TypeName::Identifier {
                                            name: "u64".to_string(),
                                            generic_parameters: None,
                                        };
                                        index =
                                            coerce_expression(&index, &index_type_name, &u64_type)
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
                            let index_type_name = module.borrow_mut().get_expression_type(
                                project,
                                scope.clone(),
                                &index,
                            )?;
                            let u64_type = sway::TypeName::Identifier {
                                name: "u64".to_string(),
                                generic_parameters: None,
                            };
                            index = coerce_expression(&index, &index_type_name, &u64_type).unwrap();

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

            let mut container_type_name = module.borrow_mut().get_expression_type(
                project,
                scope.clone(),
                &translated_container,
            )?;

            if let Some(container_type) = container_type_name.storage_key_type() {
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
            if let Some(struct_definition) = module
                .borrow()
                .structs
                .iter()
                .find(|s| s.signature.to_string() == container_type_name_string)
            {
                let field_name = translate_naming_convention(member.name.as_str(), Case::Snake);

                if struct_definition
                    .implementation
                    .as_ref()
                    .unwrap()
                    .borrow()
                    .fields
                    .iter()
                    .any(|f| f.name == field_name)
                {
                    return Ok(Some(ir::VariableAccess {
                        variable,
                        expression: sway::Expression::from(sway::MemberAccess {
                            expression: translated_container,
                            member: field_name,
                        }),
                    }));
                }
            }

            panic!(
                "{}: TODO: translate variable {container_type_name_string} member access expression: {solidity_expression}",
                project.loc_to_file_location_string(module.clone(), &solidity_expression.loc()),
            )
        }

        solidity::Expression::FunctionCall(_, function, arguments) => {
            let arguments = arguments
                .iter()
                .map(|a| translate_expression(project, module.clone(), scope.clone(), a))
                .collect::<Result<Vec<_>, _>>()?;

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
                        parameters: arguments,
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

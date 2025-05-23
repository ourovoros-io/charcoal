use crate::{error::Error, project::Project, sway, translate::*};
use convert_case::Case;
use solang_parser::{helpers::CodeLocation, pt as solidity};
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_variable_expression(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: &Rc<RefCell<TranslationScope>>,
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

    let Ok((variable, expression)) =
        translate_variable_access_expression(project, module.clone(), scope, expression)
    else {
        panic!(
            "{}ERROR: Variable not found in scope: \"{}\"",
            match project.loc_to_line_and_column(&module.borrow().path, &expression.loc()) {
                Some((line, col)) => format!(
                    "{}:{}:{} - ",
                    module.borrow().path.to_string_lossy(),
                    line,
                    col
                ),
                None => format!("{} - ", module.borrow().path.to_string_lossy()),
            },
            sway::TabbedDisplayer(&expression),
        );
    };

    if let Some(variable) = variable {
        let mut variable = variable.borrow_mut();

        variable.read_count += 1;

        if variable.storage_namespace.is_some() {
            match &variable.type_name {
                sway::TypeName::Identifier { name, .. } if name == "StorageString" => {
                    Ok(sway::Expression::create_function_calls(
                        Some(expression),
                        &[
                            ("read_slice", Some((None, vec![]))),
                            ("unwrap", Some((None, vec![]))),
                        ],
                    ))
                }

                _ => Ok(sway::Expression::create_function_calls(
                    Some(expression),
                    &[("read", Some((None, vec![])))],
                )),
            }
        } else {
            Ok(expression)
        }
    } else {
        Ok(expression)
    }
}

pub fn translate_variable_access_expression(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: &Rc<RefCell<TranslationScope>>,
    solidity_expression: &solidity::Expression,
) -> Result<(Option<Rc<RefCell<TranslatedVariable>>>, sway::Expression), Error> {
    // println!(
    //     "{}Translating variable access expression: {solidity_expression}",
    //     match project.loc_to_line_and_column(&module.borrow().path, &solidity_expression.loc()) {
    //         Some((line, col)) => format!("{}:{}:{} - ", module.borrow().path.to_string_lossy(), line, col),
    //         None => format!("{} - ", module.borrow().path.to_string_lossy()),
    //     }
    // );

    match solidity_expression {
        solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
            if let Some(variable) = scope.borrow().get_variable_from_old_name(name) {
                let variable_name = variable.borrow().new_name.clone();

                let (is_storage, namespace_name) =
                    match variable.borrow().storage_namespace.as_ref() {
                        Some(namespace_name) => (true, namespace_name.clone()),
                        None => (false, String::new()),
                    };

                return Ok((
                    Some(variable),
                    if is_storage {
                        sway::Expression::from(sway::MemberAccess {
                            expression: sway::Expression::create_identifier(format!(
                                "storage::{namespace_name}"
                            )),
                            member: variable_name,
                        })
                    } else {
                        sway::Expression::create_identifier(variable_name)
                    },
                ));
            } else if let Some(function) = scope
                .borrow()
                .find_function(|f| f.borrow().old_name == *name)
            {
                return Ok((
                    None,
                    sway::Expression::create_identifier(function.borrow().new_name.clone()),
                ));
            }

            return Err(Error::Wrapped(Box::new(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!(
                    "{}error: Variable not found in scope: \"{name}\" - {solidity_expression}",
                    match project
                        .loc_to_line_and_column(&module.borrow().path, &solidity_expression.loc())
                    {
                        Some((line, col)) => format!(
                            "{}:{}:{} - ",
                            module.borrow().path.to_string_lossy(),
                            line,
                            col
                        ),
                        None => format!("{} - ", module.borrow().path.to_string_lossy()),
                    }
                ),
            ))));
        }

        solidity::Expression::ArraySubscript(_, array_expression, Some(index)) => {
            let mut index = translate_expression(project, module.clone(), scope, index.as_ref())?;

            let (variable, expression) = translate_variable_access_expression(
                project,
                module.clone(),
                scope,
                array_expression,
            )?;

            if variable.is_none() {
                return Ok((
                    None,
                    sway::Expression::from(sway::ArrayAccess { expression, index }),
                ));
            }

            let variable = variable.unwrap();
            let type_name = module
                .borrow_mut()
                .get_expression_type(scope, &expression)?;
            let is_storage = variable.borrow().storage_namespace.is_some();

            Ok((
                Some(variable),
                match type_name {
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
                                            .get_expression_type(scope, &index)?;
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

                                    (name, _) => todo!(
                                        "{}TODO: translate {name} array subscript expression: {solidity_expression} - {} {expression:#?}",
                                        match project.loc_to_line_and_column(
                                            &module.borrow().path,
                                            &solidity_expression.loc()
                                        ) {
                                            Some((line, col)) => format!(
                                                "{}:{}:{} - ",
                                                module.borrow().path.to_string_lossy(),
                                                line,
                                                col
                                            ),
                                            None => format!(
                                                "{} - ",
                                                module.borrow().path.to_string_lossy()
                                            ),
                                        },
                                        sway::TabbedDisplayer(&expression),
                                    ),
                                },

                                _ => todo!(
                                    "{}TODO: translate {name} array subscript expression: {solidity_expression} - {} {expression:#?}",
                                    match project.loc_to_line_and_column(
                                        &module.borrow().path,
                                        &solidity_expression.loc()
                                    ) {
                                        Some((line, col)) => format!(
                                            "{}:{}:{} - ",
                                            module.borrow().path.to_string_lossy(),
                                            line,
                                            col
                                        ),
                                        None =>
                                            format!("{} - ", module.borrow().path.to_string_lossy()),
                                    },
                                    sway::TabbedDisplayer(&expression),
                                ),
                            }
                        }

                        ("Vec", Some(generic_parameters))
                            if generic_parameters.entries.len() == 1 =>
                        {
                            let index_type_name =
                                module.borrow_mut().get_expression_type(scope, &index)?;
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
                            "{}TODO: translate {name} array subscript expression: {solidity_expression} - {} {expression:#?}",
                            match project.loc_to_line_and_column(
                                &module.borrow().path,
                                &solidity_expression.loc()
                            ) {
                                Some((line, col)) => format!(
                                    "{}:{}:{} - ",
                                    module.borrow().path.to_string_lossy(),
                                    line,
                                    col
                                ),
                                None => format!("{} - ", module.borrow().path.to_string_lossy()),
                            },
                            sway::TabbedDisplayer(&expression),
                        ),
                    },

                    _ => {
                        if is_storage {
                            sway::Expression::create_function_calls(
                                Some(expression),
                                &[("get", Some((None, vec![index])))],
                            )
                        } else {
                            sway::Expression::from(sway::ArrayAccess { expression, index })
                        }
                    }
                },
            ))
        }

        solidity::Expression::MemberAccess(_, container, member) => {
            if let solidity::Expression::Variable(solidity::Identifier { name, .. }) =
                container.as_ref()
            {
                for external_definition in project.translated_modules.iter() {
                    if !matches!(
                        external_definition.borrow().kind,
                        Some(solidity::ContractTy::Library(_))
                    ) {
                        continue;
                    }

                    if external_definition.borrow().name != *name {
                        continue;
                    }

                    let member_name = translate_naming_convention(&member.name, Case::Snake);
                    let new_name = format!(
                        "{}_{}",
                        translate_naming_convention(
                            &external_definition.borrow().name,
                            Case::Snake
                        ),
                        member_name
                    );

                    return Ok((None, sway::Expression::create_identifier(new_name)));
                }
            }

            let mut translated_container =
                translate_expression(project, module.clone(), scope, container)?;

            let mut container_type_name = module
                .borrow_mut()
                .get_expression_type(scope, &translated_container)?;
            let mut container_type_name_string = container_type_name.to_string();

            let (variable, _) =
                translate_variable_access_expression(project, module.clone(), scope, container)?;

            // HACK: tack `.read()` onto the end if the container is a StorageKey
            if variable
                .as_ref()
                .map(|v| {
                    v.borrow().type_name.is_storage_key() || v.borrow().storage_namespace.is_some()
                })
                .unwrap_or(false)
            {
                let storage_key_type = match container_type_name.storage_key_type() {
                    Some(key_type) => key_type,
                    None => container_type_name,
                };

                let mut has_read = false;

                if let sway::Expression::FunctionCall(f) = &translated_container {
                    if let sway::Expression::MemberAccess(member_access) = &f.function {
                        if member_access.member == "read" && f.parameters.len() == 0 {
                            has_read = true;
                        }
                    }
                }

                if !has_read {
                    translated_container = sway::Expression::create_function_calls(
                        Some(translated_container),
                        &[("read", Some((None, vec![])))],
                    );
                }

                container_type_name = storage_key_type;
                container_type_name_string = container_type_name.to_string();
            }

            // Check if container is a struct
            if let Some(struct_definition) = module
                .borrow()
                .structs
                .iter()
                .find(|s| s.borrow().name == container_type_name_string)
            {
                let field_name = translate_naming_convention(member.name.as_str(), Case::Snake);

                if struct_definition
                    .borrow()
                    .fields
                    .iter()
                    .any(|f| f.name == field_name)
                {
                    return Ok((
                        variable,
                        sway::Expression::from(sway::MemberAccess {
                            expression: translated_container,
                            member: field_name,
                        }),
                    ));
                }
            }

            todo!(
                "{}TODO: translate variable {container_type_name_string} member access expression: {solidity_expression} - {solidity_expression:#?}",
                match project
                    .loc_to_line_and_column(&module.borrow().path, &solidity_expression.loc())
                {
                    Some((line, col)) => format!(
                        "{}:{}:{} - ",
                        module.borrow().path.to_string_lossy(),
                        line,
                        col
                    ),
                    None => format!("{} - ", module.borrow().path.to_string_lossy()),
                },
            )
        }

        solidity::Expression::FunctionCall(_, function, arguments) => {
            let arguments = arguments
                .iter()
                .map(|a| translate_expression(project, module.clone(), scope, a))
                .collect::<Result<Vec<_>, _>>()?;

            match translate_variable_access_expression(project, module.clone(), scope, function) {
                Ok((variable, expression)) => Ok((
                    variable,
                    sway::Expression::from(sway::FunctionCall {
                        function: expression,
                        generic_parameters: None,
                        parameters: arguments,
                    }),
                )),

                Err(_) => Ok((
                    None,
                    translate_expression(project, module.clone(), scope, solidity_expression)?,
                )),
            }
        }

        solidity::Expression::Type(_, _) => Err(Error::Wrapped(Box::new(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!(
                "type expression as variable access expression: {solidity_expression} - {solidity_expression:#?}"
            ),
        )))),

        _ => todo!(
            "translate variable access expression: {solidity_expression} - {solidity_expression:#?}"
        ),
    }
}

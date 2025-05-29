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

    let Some(TranslatedVariableAccess {
        variable,
        expression,
    }) = translate_variable_access_expression(project, module.clone(), scope, expression)?
    else {
        panic!(
            "{}ERROR: Variable not found in scope: \"{}\"",
            match project.loc_to_line_and_column(module.clone(), &expression.loc()) {
                Some((line, col)) => format!(
                    "{}:{}:{}: ",
                    project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy(),
                    line,
                    col
                ),
                None => format!("{}: ", project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy()),
            },
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
    module: Rc<RefCell<TranslatedModule>>,
    scope: &Rc<RefCell<TranslationScope>>,
    solidity_expression: &solidity::Expression,
) -> Result<Option<TranslatedVariableAccess>, Error> {
    // println!(
    //     "{}Translating variable access expression: {solidity_expression}",
    //     match project.loc_to_line_and_column(module.clone(), &solidity_expression.loc()) {
    //         Some((line, col)) => format!("{}:{}:{}: ", project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy() line, col),
    //         None => format!("{}: ", project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy()),
    //     }
    // );

    match solidity_expression {
        solidity::Expression::Variable(solidity::Identifier { name, .. }) => {
            if let Some(variable) = scope.borrow().get_variable_from_old_name(name) {
                let variable_name = variable.borrow().new_name.clone();

                return Ok(Some(TranslatedVariableAccess {
                    variable: Some(variable),
                    expression: sway::Expression::create_identifier(variable_name),
                }));
            } else if let Some(storage) = module.borrow().storage.as_ref() {
                if let Some(namespace) = storage
                    .namespaces
                    .iter()
                    .find(|n| n.fields.iter().any(|f| f.name == *name))
                {
                    return Ok(Some(TranslatedVariableAccess {
                        variable: None,
                        expression: sway::Expression::create_function_calls(
                            None,
                            &[
                                (format!("storage::{}", namespace.name).as_str(), None),
                                (name, None),
                                ("read", Some((None, vec![]))),
                            ],
                        ),
                    }));
                }
            } else if let Some(function) = module.borrow().functions.iter().find(|f| {
                let sway::TypeName::Function { old_name, .. } = &f.signature else {
                    unreachable!()
                };
                old_name == name
            }) {
                return Ok(Some(TranslatedVariableAccess {
                    variable: None,
                    expression: sway::Expression::create_identifier({
                        let sway::TypeName::Function { new_name, .. } = &function.signature else { unreachable!() };
                        new_name.clone()
                    }),
                }));
            } else if let Some(constant) = module
                .borrow()
                .constants
                .iter()
                .find(|c| c.old_name == *name)
            {
                return Ok(Some(TranslatedVariableAccess {
                    variable: None,
                    expression: sway::Expression::create_identifier(constant.name.clone()),
                }));
            } else if let Some(configurable) = module.borrow().configurable.as_ref() {
                if let Some(field) = configurable.fields.iter().find(|f| f.old_name == *name) {
                    return Ok(Some(TranslatedVariableAccess {
                        variable: None,
                        expression: sway::Expression::create_identifier(field.name.clone()),
                    }));
                }
            }

            return Ok(None);
        }

        solidity::Expression::ArraySubscript(_, array_expression, Some(index)) => {
            let mut index = translate_expression(project, module.clone(), scope, index.as_ref())?;

            let Some(TranslatedVariableAccess {
                variable,
                expression,
            }) = translate_variable_access_expression(
                project,
                module.clone(),
                scope,
                array_expression,
            )?
            else {
                return Ok(None);
            };

            if variable.is_none() {
                return Ok(Some(TranslatedVariableAccess {
                    variable: None,
                    expression: sway::Expression::from(sway::ArrayAccess { expression, index }),
                }));
            }

            let variable = variable.unwrap();
            let type_name = module
                .borrow_mut()
                .get_expression_type(scope, &expression)?;

            Ok(Some(TranslatedVariableAccess {
                variable: Some(variable),
                expression: match type_name {
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
                                            module.clone(),
                                            &solidity_expression.loc()
                                        ) {
                                            Some((line, col)) => format!(
                                                "{}:{}:{}: ",
                                                project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy(),
                                                line,
                                                col
                                            ),
                                            None => format!(
                                                "{}: ",
                                                module.borrow().path.to_string_lossy()
                                            ),
                                        },
                                        sway::TabbedDisplayer(&expression),
                                    ),
                                },

                                _ => todo!(
                                    "{}TODO: translate {name} array subscript expression: {solidity_expression} - {} {expression:#?}",
                                    match project.loc_to_line_and_column(
                                        module.clone(),
                                        &solidity_expression.loc()
                                    ) {
                                        Some((line, col)) => format!(
                                            "{}:{}:{}: ",
                                            project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy(),
                                            line,
                                            col
                                        ),
                                        None =>
                                            format!("{}: ", project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy()),
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
                            match project
                                .loc_to_line_and_column(module.clone(), &solidity_expression.loc())
                            {
                                Some((line, col)) => format!(
                                    "{}:{}:{}: ",
                                    project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy(),
                                    line,
                                    col
                                ),
                                None => format!("{}: ", project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy()),
                            },
                            sway::TabbedDisplayer(&expression),
                        ),
                    },

                    _ => sway::Expression::from(sway::ArrayAccess { expression, index }),
                },
            }))
        }

        solidity::Expression::MemberAccess(_, container, member) => {
            if let solidity::Expression::Variable(solidity::Identifier { name, .. }) =
                container.as_ref()
            {
                for external_module in project.translated_modules.iter() {
                    for external_definition in external_module.borrow().contracts.iter() {
                        let external_definition = external_definition.implementation.as_ref().unwrap();

                        if !matches!(external_definition.kind, solidity::ContractTy::Library(_)) {
                            continue;
                        }

                        if external_definition.name != *name {
                            continue;
                        }

                        let new_name = translate_naming_convention(&member.name, Case::Snake);

                        return Ok(Some(TranslatedVariableAccess {
                            variable: None,
                            expression: sway::Expression::create_identifier(new_name),
                        }));
                    }
                }
            }

            let translated_container =
                translate_expression(project, module.clone(), scope, container)?;

            let container_type_name = module
                .borrow_mut()
                .get_expression_type(scope, &translated_container)?;
            let container_type_name_string = container_type_name.to_string();

            let Some(TranslatedVariableAccess { variable, .. }) =
                translate_variable_access_expression(project, module.clone(), scope, container)?
            else {
                return Ok(None);
            };

            // Check if container is a struct
            if let Some(struct_definition) = module.borrow().structs.iter().find(|s| {
                s.implementation.as_ref().unwrap().borrow().name == container_type_name_string
            }) {
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
                    return Ok(Some(TranslatedVariableAccess {
                        variable,
                        expression: sway::Expression::from(sway::MemberAccess {
                            expression: translated_container,
                            member: field_name,
                        }),
                    }));
                }
            }

            todo!(
                "{}TODO: translate variable {container_type_name_string} member access expression: {solidity_expression} - {solidity_expression:#?}",
                match project.loc_to_line_and_column(module.clone(), &solidity_expression.loc()) {
                    Some((line, col)) => format!(
                        "{}:{}:{}: ",
                        project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy(),
                        line,
                        col
                    ),
                    None => format!("{}: ", project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy()),
                },
            )
        }

        solidity::Expression::FunctionCall(_, function, arguments) => {
            let arguments = arguments
                .iter()
                .map(|a| translate_expression(project, module.clone(), scope, a))
                .collect::<Result<Vec<_>, _>>()?;

            match translate_variable_access_expression(project, module.clone(), scope, function)? {
                Some(TranslatedVariableAccess {
                    variable,
                    expression,
                }) => Ok(Some(TranslatedVariableAccess {
                    variable,
                    expression: sway::Expression::from(sway::FunctionCall {
                        function: expression,
                        generic_parameters: None,
                        parameters: arguments,
                    }),
                })),

                None => Ok(Some(TranslatedVariableAccess {
                    variable: None,
                    expression: translate_expression(
                        project,
                        module.clone(),
                        scope,
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

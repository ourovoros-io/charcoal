use crate::{error::Error, project::Project, sway, translate::*};
use convert_case::Case;
use solang_parser::{helpers::CodeLocation, pt as solidity};
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_emit_statement(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &solidity::Expression,
) -> Result<sway::Statement, Error> {
    match expression {
        solidity::Expression::FunctionCall(_, function, arguments) => {
            let event_variant_name = match function.as_ref() {
                solidity::Expression::Variable(solidity::Identifier { name, .. }) => name,

                solidity::Expression::MemberAccess(_, container, member) => {
                    let solidity::Expression::Variable(container_id) = container.as_ref() else {
                        todo!()
                    };

                    let event_variant_name =
                        translate_naming_convention(&member.name, Case::Pascal);

                    // Check if container is contained in an external definition
                    if let Some(external_module) = project
                        .find_module_containing_contract(module.clone(), container_id.name.as_str())
                    {
                        for events_enum in external_module.borrow().events_enums.iter() {
                            for variant in events_enum.0.borrow().variants.clone() {
                                if variant.name == event_variant_name {
                                    return Ok(sway::Statement::from(
                                        sway::Expression::create_function_calls(
                                            None,
                                            &[(
                                                "log",
                                                Some((
                                                    None,
                                                    vec![if arguments.is_empty() {
                                                        sway::Expression::create_identifier(
                                                            format!(
                                                                "{}::{}",
                                                                events_enum.0.borrow().name,
                                                                event_variant_name,
                                                            ),
                                                        )
                                                    } else {
                                                        sway::Expression::create_function_calls(
                                                            None,
                                                            &[(
                                                                format!(
                                                                    "{}::{}",
                                                                    events_enum.0.borrow().name,
                                                                    event_variant_name,
                                                                )
                                                                .as_str(),
                                                                Some((
                                                                    None,
                                                                    vec![if arguments.len() == 1 {
                                                                        let argument =
                                                                            translate_expression(
                                                                                project,
                                                                                module.clone(),
                                                                                scope.clone(),
                                                                                &arguments[0],
                                                                            )?;

                                                                        let argument_type =
                                                                            get_expression_type(
                                                                                project,
                                                                                module.clone(),
                                                                                scope.clone(),
                                                                                &argument,
                                                                            )?;

                                                                        coerce_expression(
                                                                            project,
                                                                            module.clone(),
                                                                            scope.clone(),
                                                                            &argument,
                                                                            &argument_type,
                                                                            &variant.type_name,
                                                                        )
                                                                        .unwrap()
                                                                    } else {
                                                                        let mut arguments = arguments
                                                                                .iter()
                                                                                .map(|p| {
                                                                                    translate_expression(
                                                                                        project,
                                                                                        module.clone(),
                                                                                        scope.clone(),
                                                                                        p,
                                                                                    )
                                                                                })
                                                                                .collect::<Result<Vec<_>, _>>()?;

                                                                        let mut arguments_types = arguments
                                                                            .iter()
                                                                            .map(|a| {
                                                                                get_expression_type(project, module.clone(), scope.clone(), a)
                                                                                    .unwrap()
                                                                            }).collect::<Vec<_>>();

                                                                        let sway::TypeName::Tuple {
                                                                            ref type_names,
                                                                        } = variant.type_name
                                                                        else {
                                                                            panic!(
                                                                                "Expected a tuple"
                                                                            )
                                                                        };

                                                                        let coerced: Vec<sway::Expression> = arguments
                                                                            .iter_mut()
                                                                            .zip(
                                                                                arguments_types
                                                                                    .iter_mut()
                                                                                    .zip(type_names)
                                                                            )
                                                                            .map(|(expr, (from_type_name, to_type_name))| {
                                                                                coerce_expression(project, module.clone(), scope.clone(), expr, from_type_name, to_type_name).unwrap()
                                                                            })
                                                                            .collect();

                                                                        sway::Expression::Tuple(
                                                                            coerced,
                                                                        )
                                                                    }],
                                                                )),
                                                            )],
                                                        )
                                                    }],
                                                )),
                                            )],
                                        ),
                                    ));
                                }
                            }
                        }
                    }

                    todo!()
                }

                _ => todo!(),
            };

            if let Some(SymbolData::EventVariant {
                type_name: event_type_name,
                variant: event_variant,
            }) = resolve_symbol(
                project,
                module.clone(),
                scope.clone(),
                Symbol::Event(event_variant_name.clone()),
            ) {
                return Ok(sway::Statement::from(sway::Expression::from(
                    sway::FunctionCall {
                        function: sway::Expression::create_identifier("log".into()),
                        generic_parameters: None,
                        parameters: vec![if arguments.is_empty() {
                            sway::Expression::create_identifier(format!(
                                "{}::{}",
                                event_type_name, event_variant_name,
                            ))
                        } else {
                            sway::Expression::create_function_calls(
                                None,
                                &[(
                                    format!("{}::{}", event_type_name, event_variant_name).as_str(),
                                    Some((
                                        None,
                                        vec![if arguments.len() == 1 {
                                            let argument = translate_expression(
                                                project,
                                                module.clone(),
                                                scope.clone(),
                                                &arguments[0],
                                            )?;

                                            let argument_type = get_expression_type(
                                                project,
                                                module.clone(),
                                                scope.clone(),
                                                &argument,
                                            )?;

                                            coerce_expression(
                                                project,
                                                module.clone(),
                                                scope.clone(),
                                                &argument,
                                                &argument_type,
                                                &event_variant.type_name,
                                            )
                                            .unwrap()
                                        } else {
                                            let mut arguments = arguments
                                                .iter()
                                                .map(|p| {
                                                    translate_expression(
                                                        project,
                                                        module.clone(),
                                                        scope.clone(),
                                                        p,
                                                    )
                                                })
                                                .collect::<Result<Vec<_>, _>>()?;

                                            let mut arguments_types = arguments
                                                .iter()
                                                .map(|a| {
                                                    get_expression_type(
                                                        project,
                                                        module.clone(),
                                                        scope.clone(),
                                                        a,
                                                    )
                                                    .unwrap()
                                                })
                                                .collect::<Vec<_>>();

                                            let sway::TypeName::Tuple { ref type_names } =
                                                event_variant.type_name
                                            else {
                                                panic!("Expected a tuple")
                                            };

                                            let coerced: Vec<sway::Expression> = arguments
                                                .iter_mut()
                                                .zip(arguments_types.iter_mut().zip(type_names))
                                                .map(|(expr, (from_type_name, to_type_name))| {
                                                    coerce_expression(
                                                        project,
                                                        module.clone(),
                                                        scope.clone(),
                                                        expr,
                                                        from_type_name,
                                                        to_type_name,
                                                    )
                                                    .unwrap()
                                                })
                                                .collect();

                                            sway::Expression::Tuple(coerced)
                                        }],
                                    )),
                                )],
                            )
                        }],
                    },
                )));
            }

            panic!(
                "Failed to find event variant \"{event_variant_name}\" in \"{}\"",
                module.borrow().name,
            )
        }

        _ => panic!(
            "{}: TODO: translate emit statement: {expression} - {expression:#?}",
            project.loc_to_file_location_string(module.clone(), &expression.loc()),
        ),
    }
}

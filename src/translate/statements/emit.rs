use crate::{error::Error, project::Project, sway, translate::*};
use convert_case::Case;
use solang_parser::{helpers::CodeLocation, pt as solidity};
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_emit_statement(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: Rc<RefCell<TranslationScope>>,
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
                    if let Some(external_definition) = project
                        .translated_modules
                        .iter()
                        .find(|d| d.borrow().name == container_id.name)
                        .cloned()
                    {
                        for events_enum in external_definition.borrow().events_enums.iter() {
                            for variant in events_enum.0.borrow().variants.clone() {
                                if variant.name == event_variant_name {
                                    if !module.borrow().events_enums.contains(events_enum) {
                                        module.borrow_mut()
                                            .events_enums
                                            .push(events_enum.clone());
                                    }

                                    return Ok(sway::Statement::from(sway::Expression::create_function_calls(None, &[
                                        ("log", Some((None, vec![
                                            if arguments.is_empty() {
                                                sway::Expression::create_identifier(format!(
                                                    "{}::{}",
                                                    events_enum.0.borrow().name,
                                                    event_variant_name,
                                                ))
                                            } else {
                                                sway::Expression::create_function_calls(None, &[
                                                    (format!(
                                                        "{}::{}",
                                                        events_enum.0.borrow().name,
                                                        event_variant_name,
                                                    ).as_str(), Some((None, vec![
                                                        if arguments.len() == 1 {
                                                            let argument = translate_expression(
                                                                project,
                                                                module.clone(),
                                                                scope.clone(),
                                                                &arguments[0],
                                                            )?;
                                                            
                                                            let argument_type = module.borrow_mut().get_expression_type(scope.clone(), &argument)?;
                                                            
                                                            coerce_expression(&argument, &argument_type, &variant.type_name).unwrap()
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

                                                            let mut arguments_types = arguments.iter().map(|a| module.borrow_mut().get_expression_type(scope.clone(), a).unwrap()).collect::<Vec<_>>();

                                                            let sway::TypeName::Tuple { ref type_names } = variant.type_name else { panic!("Expected a tuple") };

                                                            let coerced: Vec<sway::Expression> = arguments
                                                                .iter_mut().zip(arguments_types.iter_mut().zip(type_names))
                                                                .map(|(expr, (from_type_name, to_type_name))| {                                        
                                                                    coerce_expression(expr, from_type_name, to_type_name).unwrap()
                                                                })
                                                                .collect();
                                                            
                                                            sway::Expression::Tuple(coerced)
                                                        },
                                                    ]))),
                                                ])
                                            },
                                        ])))
                                    ])));
                                }
                            }
                        }
                    }

                    todo!()
                }

                _ => todo!(),
            };

            if let Some(event_variant) = resolve_symbol(project, module.clone(), Symbol::Event(event_variant_name.clone())) {
                let Some((event_enum_name, event_variant)) = event_variant.downcast_ref::<(String, sway::EnumVariant)>() else { panic!("Invalid enum variant") };

                return Ok(sway::Statement::from(sway::Expression::from(
                    sway::FunctionCall {
                        function: sway::Expression::create_identifier("log".into()),
                        generic_parameters: None,
                        parameters: vec![if arguments.is_empty() {
                            sway::Expression::create_identifier(format!(
                                "{}::{}",
                                event_enum_name,
                                event_variant_name,
                            ))
                        } else {
                            sway::Expression::create_function_calls(None, &[
                                (format!("{}::{}", event_enum_name, event_variant_name).as_str(), Some((None, vec![
                                    if arguments.len() == 1 {
                                        let argument = translate_expression(
                                            project,
                                            module.clone(),
                                            scope.clone(),
                                            &arguments[0],
                                        )?;
                                        
                                        let argument_type = module.borrow_mut().get_expression_type(scope.clone(), &argument)?;
                                        
                                        coerce_expression(&argument, &argument_type, &event_variant.type_name).unwrap()
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

                                        let mut arguments_types = arguments.iter().map(|a| module.borrow_mut().get_expression_type(scope.clone(), a).unwrap()).collect::<Vec<_>>();

                                        let sway::TypeName::Tuple { ref type_names } = event_variant.type_name else { panic!("Expected a tuple") };

                                        let coerced: Vec<sway::Expression> = arguments
                                            .iter_mut().zip(arguments_types.iter_mut().zip(type_names))
                                            .map(|(expr, (from_type_name, to_type_name))| {                                        
                                                coerce_expression(expr, from_type_name, to_type_name).unwrap()
                                            })
                                            .collect();
                                        
                                        sway::Expression::Tuple(coerced)
                                    },
                                ]))),
                            ])
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
            "{}TODO: translate emit statement: {expression} - {expression:#?}",
            match project.loc_to_line_and_column(module.clone(), &expression.loc()) {
                Some((line, col)) => format!(
                    "{}:{}:{}: ",
                    project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy(),
                    line,
                    col
                ),
                None => format!("{}: ", project.options.input.join(module.borrow().path.clone()).with_extension("sol").to_string_lossy()),
            },
        ),
    }
}

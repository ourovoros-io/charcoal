use crate::{
    errors::Error,
    project::Project,
    sway,
    translate::{function_call::coerce_expression, translate_expression, TranslatedDefinition, TranslationScope},
};
use convert_case::Case;
use solang_parser::{helpers::CodeLocation, pt as solidity};
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_emit_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
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
                        crate::translate_naming_convention(&member.name, Case::Pascal);

                    // Check if container is contained in an external definition
                    if let Some(external_definition) = project
                        .translated_definitions
                        .iter()
                        .find(|d| d.name == container_id.name)
                        .cloned()
                    {
                        for events_enum in external_definition.events_enums.iter() {
                            for variant in events_enum.0.borrow().variants.clone() {
                                if variant.name == event_variant_name {
                                    if !translated_definition.events_enums.contains(events_enum) {
                                        translated_definition
                                            .events_enums
                                            .push(events_enum.clone());
                                    }

                                    return Ok(sway::Statement::from(sway::Expression::from(
                                        sway::FunctionCall {
                                            function: sway::Expression::Identifier("log".into()),
                                            generic_parameters: None,
                                            parameters: vec![if arguments.is_empty() {
                                                sway::Expression::Identifier(format!(
                                                    "{}::{}",
                                                    events_enum.0.borrow().name,
                                                    event_variant_name,
                                                ))
                                            } else {
                                                sway::Expression::from(sway::FunctionCall {
                                                    function: sway::Expression::Identifier(
                                                        format!(
                                                            "{}::{}",
                                                            events_enum.0.borrow().name,
                                                            event_variant_name,
                                                        ),
                                                    ),
                                                    generic_parameters: None,
                                                    parameters: vec![if arguments.len() == 1 {
                                                        translate_expression(
                                                            project,
                                                            translated_definition,
                                                            scope.clone(),
                                                            &arguments[0],
                                                        )?
                                                    } else {
                                                        sway::Expression::Tuple(
                                                            arguments
                                                                .iter()
                                                                .map(|p| {
                                                                    translate_expression(
                                                                        project,
                                                                        translated_definition,
                                                                        scope.clone(),
                                                                        p,
                                                                    )
                                                                })
                                                                .collect::<Result<Vec<_>, _>>()?,
                                                        )
                                                    }],
                                                })
                                            }],
                                        },
                                    )));
                                }
                            }
                        }
                    }

                    todo!()
                }

                _ => todo!(),
            };

            let Some((events_enum, _)) = translated_definition
                .events_enums
                .iter()
                .find(|(e, _)| {
                    e.borrow()
                        .variants
                        .iter()
                        .any(|v| v.name == *event_variant_name)
                })
                .cloned()
            else {
                panic!(
                    "Failed to find event variant \"{event_variant_name}\" in \"{}\": {:#?}",
                    translated_definition.name, translated_definition.events_enums,
                )
            };

            let events_enum = events_enum.borrow();

            let Some(event_variant) = events_enum.variants.iter().find(|v| v.name == *event_variant_name) else {
                panic!(
                    "Failed to find event variant \"{event_variant_name}\" in \"{}\": {:#?}",
                    translated_definition.name, translated_definition.events_enums,
                )
            };

            return Ok(sway::Statement::from(sway::Expression::from(
                sway::FunctionCall {
                    function: sway::Expression::Identifier("log".into()),
                    generic_parameters: None,
                    parameters: vec![if arguments.is_empty() {
                        sway::Expression::Identifier(format!(
                            "{}::{}",
                            events_enum.name,
                            event_variant_name,
                        ))
                    } else {
                        sway::Expression::from(sway::FunctionCall {
                            function: sway::Expression::Identifier(format!(
                                "{}::{}",
                                events_enum.name,
                                event_variant_name,
                            )),
                            generic_parameters: None,
                            parameters: vec![if arguments.len() == 1 {
                                translate_expression(
                                    project,
                                    translated_definition,
                                    scope.clone(),
                                    &arguments[0],
                                )?
                            } else {
                                let mut arguments = arguments
                                    .iter()
                                    .map(|p| {
                                        translate_expression(
                                            project,
                                            translated_definition,
                                            scope.clone(),
                                            p,
                                        )
                                    })
                                    .collect::<Result<Vec<_>, _>>()?;

                                let mut arguments_types = arguments.iter().map(|a| translated_definition.get_expression_type(scope.clone(), a).unwrap()).collect::<Vec<_>>();

                                let sway::TypeName::Tuple { ref type_names } = event_variant.type_name else { panic!("Expected a tuple") };

                                let coerced: Vec<sway::Expression> = arguments
                                    .iter_mut().zip(arguments_types.iter_mut().zip(type_names))
                                    .map(|(expr, (from_type_name, to_type_name))| {                                        
                                        coerce_expression(expr, from_type_name, &to_type_name);
                                        expr.clone()
                                    })
                                    .collect();
                                
                                sway::Expression::Tuple(coerced)
                            }],
                        })
                    }],
                },
            )));
        }

        _ => panic!(
            "{}TODO: translate emit statement: {expression} - {expression:#?}",
            match project.loc_to_line_and_column(&translated_definition.path, &expression.loc()) {
                Some((line, col)) => format!(
                    "{}:{}:{} - ",
                    translated_definition.path.to_string_lossy(),
                    line,
                    col
                ),
                None => format!("{} - ", translated_definition.path.to_string_lossy()),
            },
        ),
    }
}

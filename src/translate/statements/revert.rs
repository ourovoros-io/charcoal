use std::{cell::RefCell, rc::Rc};
use num_bigint::BigUint;
use num_traits::Zero;
use solang_parser::pt as solidity;
use crate::{errors::Error, project::Project, sway, translate::{translate_expression, TranslatedDefinition, TranslationScope}};

#[inline]
pub fn translate_revert_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    error_type: &Option<solidity::IdentifierPath>,
    parameters: &[solidity::Expression],
) -> Result<sway::Statement, Error> {
    if let Some(error_type) = error_type.as_ref() {
        if !(1..=2).contains(&error_type.identifiers.len()) {
            panic!("Unexpected error type: expected 1 or 2 identifiers, found {}", error_type.identifiers.len());
        }

        let mut ids_iter = error_type.identifiers.iter();

        // Find the error variant
        let (error_variant_name, errors_enum_and_impl) = if error_type.identifiers.len() == 2 {
            let external_definition_name = ids_iter.next().unwrap().name.clone();
            let error_variant_name = ids_iter.next().unwrap().name.clone();
            let external_definition = project.translated_definitions.iter_mut().find(|d| d.name == external_definition_name).unwrap();
            let errors_enum_and_impl = external_definition.errors_enums.iter().find(|(e, _)| e.borrow().variants.iter().any(|v| v.name == error_variant_name)).cloned().unwrap();
            (error_variant_name, errors_enum_and_impl)
        } else {
            let error_variant_name = ids_iter.next().unwrap().name.clone();
            let errors_enum_and_impl = translated_definition.errors_enums.iter().find(|(e, _)| e.borrow().variants.iter().any(|v| v.name == error_variant_name)).cloned().unwrap();
            (error_variant_name, errors_enum_and_impl)
        };

        // Add the error definition to the current definition if we haven't already
        if !translated_definition.errors_enums.contains(&errors_enum_and_impl) {
            translated_definition.errors_enums.push(errors_enum_and_impl.clone());
        }

        let (errors_enum, _) = errors_enum_and_impl;
        
        return Ok(sway::Statement::from(sway::Expression::from(sway::Block {
            statements: vec![
                // 1. log(data)
                sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::Identifier("log".into()),
                    generic_parameters: None,
                    parameters: vec![
                        if parameters.is_empty() {
                            sway::Expression::Identifier(format!(
                                "{}::{}",
                                errors_enum.borrow().name,
                                error_variant_name,
                            ))
                        } else {
                            sway::Expression::from(sway::FunctionCall {
                                function: sway::Expression::Identifier(format!(
                                    "{}::{}",
                                    errors_enum.borrow().name,
                                    error_variant_name,
                                )),
                                generic_parameters: None,
                                parameters: vec![
                                    if parameters.len() == 1 {
                                        translate_expression(project, translated_definition, scope.clone(), &parameters[0])?
                                    } else {
                                        sway::Expression::Tuple(
                                            parameters.iter()
                                                .map(|p| translate_expression(project, translated_definition, scope.clone(), p))
                                                .collect::<Result<Vec<_>, _>>()?
                                        )
                                    },
                                ]
                            })
                        },
                    ]
                })),
                // 2. revert(0)
                sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::Identifier("revert".into()),
                    generic_parameters: None,
                    parameters: vec![
                        sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                    ],
                }))
            ],
            final_expr: None,
        })));
    }

    if parameters.is_empty() {
        return Ok(sway::Statement::from(sway::Expression::from(sway::FunctionCall {
            function: sway::Expression::Identifier("revert".into()),
            generic_parameters: None,
            parameters: vec![
                sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
            ],
        })))
    }

    match parameters.first().as_ref() {
        Some(solidity::Expression::StringLiteral(reason)) => {
            return Ok(sway::Statement::from(sway::Expression::from(sway::Block {
                statements: vec![
                    // 1. log(reason)
                    sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("log".into()),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::from(sway::Literal::String(
                                reason.iter().map(|s| s.string.clone()).collect::<Vec<_>>().join("")
                            )),
                        ]
                    })),
                    // 2. revert(0)
                    sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("revert".into()),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                        ],
                    }))
                ],
                final_expr: None,
            })));
        },
        Some(x) if matches!(x, solidity::Expression::Variable(_)) => {
            let x_expr = translate_expression(project, translated_definition, scope, x)?;
            return Ok(sway::Statement::from(sway::Expression::from(sway::Block {
                statements: vec![
                    // 1. log(reason)
                    sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("log".into()),
                        generic_parameters: None,
                        parameters: vec![x_expr]
                    })),
                    // 2. revert(0)
                    sway::Statement::from(sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::Identifier("revert".into()),
                        generic_parameters: None,
                        parameters: vec![
                            sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None)),
                        ],
                    }))
                ],
                final_expr: None,
            })));
        },
        _ => {},
    }

    Ok(sway::Statement::from(sway::Expression::create_todo(Some(format!(
        "revert({}{}{})",
        if let Some(error_type) = error_type.as_ref() {
            error_type.to_string()
        } else {
            String::new()
        },
        if error_type.is_some() && !parameters.is_empty() {
            ", "
        } else {
            ""
        },
        parameters.iter().map(|p| p.to_string()).collect::<Vec<_>>().join(", "),
    )))))
}

#[inline]
pub fn translate_revert_named_arguments(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    path: &Option<solidity::IdentifierPath>,
    named_args: &[solidity::NamedArgument]
) -> Result<sway::Statement, Error> {
    // TODO: Keep track of the paramerter names and order them correctly
    let error_identifier = path.as_ref().unwrap().identifiers.first().unwrap().name.clone();
    if translated_definition.errors_enums.iter().any(|e|
        e.0.borrow().variants.iter().any(|v| v.name == error_identifier)
    ) {
        let error_expressions: Vec<_> = named_args.iter().map(|arg| arg.expr.clone()).collect();
        return translate_revert_statement(project, translated_definition, scope, path, &error_expressions)
    }

    todo!("translate revert named arguments: {:#?}", path)
}

use crate::{error::Error, project::Project, sway, translate::*};
use num_bigint::BigUint;
use num_traits::Zero;
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_revert_statement(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    error_type: &Option<solidity::IdentifierPath>,
    parameters: &[solidity::Expression],
) -> Result<sway::Statement, Error> {
    if let Some(error_type) = error_type.as_ref() {
        if !(1..=2).contains(&error_type.identifiers.len()) {
            panic!(
                "Unexpected error type: expected 1 or 2 identifiers, found {}",
                error_type.identifiers.len()
            );
        }

        let mut ids_iter = error_type.identifiers.iter();

        // Check if the error variant is under an explicit contract
        let (error_type_name, error_variant) = if error_type.identifiers.len() == 2 {
            let external_definition_name = ids_iter.next().unwrap().name.clone();
            let error_variant_name = ids_iter.next().unwrap().name.clone();

            let Some(external_module) =
                project.find_module_containing_contract(module.clone(), external_definition_name.as_str())
            else {
                panic!(
                    "Failed to find module containing contract: {}",
                    external_definition_name
                );
            };

            let scope = Rc::new(RefCell::new(ir::Scope::new(
                Some(external_module.borrow().path.clone()),
                Some(&external_definition_name),
                None,
                Some(scope.clone()),
            )));

            let SymbolData::ErrorVariant { type_name, variant } = resolve_symbol(
                project,
                module.clone(),
                scope.clone(),
                Symbol::Error(error_variant_name),
            )
            .unwrap() else {
                unreachable!()
            };

            (type_name, variant)
        }
        // Check if the error variant is defined in the current module
        else {
            let error_variant_name = ids_iter.next().unwrap().name.clone();

            let SymbolData::ErrorVariant { type_name, variant } = resolve_symbol(
                project,
                module.clone(),
                scope.clone(),
                Symbol::Error(error_variant_name),
            )
            .unwrap() else {
                unreachable!()
            };

            (type_name, variant)
        };

        return Ok(sway::Statement::from(sway::Expression::from(sway::Block {
            statements: vec![
                // 1. log(data)
                sway::Statement::from(sway::Expression::create_function_call(
                    "log",
                    None,
                    vec![if parameters.is_empty() {
                        sway::Expression::create_identifier(
                            format!("{}::{}", error_type_name, error_variant.name).as_str(),
                        )
                    } else {
                        sway::Expression::create_function_call(
                            format!("{}::{}", error_type_name, error_variant.name).as_str(),
                            None,
                            vec![if parameters.len() == 1 {
                                let parameter_expression =
                                    translate_expression(project, module.clone(), scope.clone(), &parameters[0])?;
                                let parameter_expression_type =
                                    get_expression_type(project, module.clone(), scope.clone(), &parameter_expression)?;
                                coerce_expression(
                                    project,
                                    module.clone(),
                                    scope.clone(),
                                    &parameter_expression,
                                    &parameter_expression_type,
                                    &error_variant.type_name,
                                )
                                .unwrap()
                            } else {
                                let sway::TypeName::Tuple { type_names } = &error_variant.type_name else {
                                    panic!("Expected type Tuple");
                                };
                                sway::Expression::Tuple(
                                    parameters
                                        .iter()
                                        .zip(type_names.iter())
                                        .map(|(param, type_name)| {
                                            let parameter_expression =
                                                translate_expression(project, module.clone(), scope.clone(), param)
                                                    .unwrap();
                                            let parameter_expression_type = get_expression_type(
                                                project,
                                                module.clone(),
                                                scope.clone(),
                                                &parameter_expression,
                                            )
                                            .unwrap();
                                            coerce_expression(
                                                project,
                                                module.clone(),
                                                scope.clone(),
                                                &parameter_expression,
                                                &parameter_expression_type,
                                                type_name,
                                            )
                                            .unwrap()
                                        })
                                        .collect::<Vec<_>>(),
                                )
                            }],
                        )
                    }],
                )),
                // 2. revert(0)
                sway::Statement::from(sway::Expression::create_function_call(
                    "revert",
                    None,
                    vec![sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None))],
                )),
            ],
            final_expr: None,
        })));
    }

    if parameters.is_empty() {
        return Ok(sway::Statement::from(sway::Expression::from(sway::FunctionCall {
            function: sway::Expression::create_identifier("revert".into()),
            generic_parameters: None,
            parameters: vec![sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None))],
        })));
    }

    match parameters.first().as_ref() {
        Some(solidity::Expression::StringLiteral(reason)) => {
            return Ok(sway::Statement::from(sway::Expression::from(sway::Block {
                statements: vec![
                    // 1. log(reason)
                    sway::Statement::from(sway::Expression::create_function_call(
                        "log",
                        None,
                        vec![sway::Expression::from(sway::Literal::String(
                            reason.iter().map(|s| s.string.clone()).collect::<String>(),
                        ))],
                    )),
                    // 2. revert(0)
                    sway::Statement::from(sway::Expression::create_function_call(
                        "revert",
                        None,
                        vec![sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None))],
                    )),
                ],
                final_expr: None,
            })));
        }
        Some(x) if matches!(x, solidity::Expression::Variable(_)) => {
            let x_expr = translate_expression(project, module.clone(), scope.clone(), x)?;
            return Ok(sway::Statement::from(sway::Expression::from(sway::Block {
                statements: vec![
                    // 1. log(reason)
                    sway::Statement::from(sway::Expression::create_function_call("log", None, vec![x_expr])),
                    // 2. revert(0)
                    sway::Statement::from(sway::Expression::create_function_call(
                        "revert",
                        None,
                        vec![sway::Expression::from(sway::Literal::DecInt(BigUint::zero(), None))],
                    )),
                ],
                final_expr: None,
            })));
        }
        _ => {}
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
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    path: &Option<solidity::IdentifierPath>,
    named_args: &[solidity::NamedArgument],
) -> Result<sway::Statement, Error> {
    // TODO: Keep track of the parameter names and order them correctly
    let error_identifier = path.as_ref().unwrap().identifiers.first().unwrap().name.clone();

    if module
        .borrow()
        .errors_enums
        .iter()
        .any(|e| e.0.borrow().variants.iter().any(|v| v.name == error_identifier))
    {
        let error_expressions: Vec<_> = named_args.iter().map(|arg| arg.expr.clone()).collect();
        return translate_revert_statement(project, module, scope.clone(), path, &error_expressions);
    }

    todo!("translate revert named arguments: {:#?}", path)
}

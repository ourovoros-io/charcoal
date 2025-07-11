use crate::{error::Error, project::Project, sway, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_delete_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    let Some(ir::VariableAccess {
        variable,
        expression,
    }) = translate_variable_access_expression(project, module.clone(), scope.clone(), expression)?
    else {
        panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
    };

    if let sway::Expression::FunctionCall(f) = &expression
        && let sway::Expression::MemberAccess(m) = &f.function
    {
        match m.member.as_str() {
            "get" if f.parameters.len() == 1 => {
                let container_type =
                    get_expression_type(project, module.clone(), scope.clone(), &m.expression)?;

                if let Some(storage_key_type) = container_type.storage_key_type() {
                    match &storage_key_type {
                        sway::TypeName::Identifier {
                            name,
                            generic_parameters,
                        } => match (name.as_str(), generic_parameters.as_ref()) {
                            ("StorageMap", Some(_)) => {
                                if let Some(function_name) = scope.borrow().get_function_name() {
                                    module
                                        .borrow_mut()
                                        .function_storage_accesses
                                        .entry(function_name)
                                        .or_default()
                                        .1 = true;
                                }

                                return Ok(sway::Expression::create_function_calls(
                                    Some(m.expression.clone()),
                                    &[("remove", Some((None, vec![f.parameters[0].clone()])))],
                                ));
                            }

                            _ => {}
                        },

                        _ => {}
                    }
                }
            }

            "unwrap" if f.parameters.is_empty() => {
                if let sway::Expression::FunctionCall(f) = &m.expression
                    && let sway::Expression::MemberAccess(m) = &f.function
                {
                    match m.member.as_str() {
                        "get" if f.parameters.len() == 1 => {
                            let container_type = get_expression_type(
                                project,
                                module.clone(),
                                scope.clone(),
                                &m.expression,
                            )?;

                            if let Some(storage_key_type) = container_type.storage_key_type() {
                                match &storage_key_type {
                                    sway::TypeName::Identifier {
                                        name,
                                        generic_parameters,
                                    } => match (name.as_str(), generic_parameters.as_ref()) {
                                        ("StorageVec", Some(_)) => {
                                            if let Some(function_name) =
                                                scope.borrow().get_function_name()
                                            {
                                                module
                                                    .borrow_mut()
                                                    .function_storage_accesses
                                                    .entry(function_name)
                                                    .or_default()
                                                    .1 = true;
                                            }

                                            return Ok(sway::Expression::create_function_calls(
                                                Some(m.expression.clone()),
                                                &[(
                                                    "remove",
                                                    Some((None, vec![f.parameters[0].clone()])),
                                                )],
                                            ));
                                        }

                                        _ => {}
                                    },

                                    _ => {}
                                }
                            }
                        }

                        _ => {}
                    }
                }
            }

            _ => {}
        }
    }

    let type_name = match variable.as_ref() {
        Some(variable) => variable.borrow().type_name.clone(),
        None => get_expression_type(project, module.clone(), scope.clone(), &expression)?,
    };

    // todo!(
    //     "{}: delete from {type_name} expression: {}",
    //     project.loc_to_file_location_string(module.clone(), &solidity_expression.loc()),
    //     sway::TabbedDisplayer(&expression)
    // )

    let value = create_value_expression(project, module.clone(), scope.clone(), &type_name, None);

    create_assignment_expression(
        project,
        module.clone(),
        scope.clone(),
        "=",
        &expression,
        variable,
        &value,
        &type_name,
    )
}

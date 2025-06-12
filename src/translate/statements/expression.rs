use crate::{error::Error, project::Project, sway, translate::*};
use convert_case::Case;
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_expression_statement(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    expression: &solidity::Expression,
) -> Result<sway::Statement, Error> {
    match expression {
        // Check for an assignment expression where lhs is a list expression
        solidity::Expression::Assign(_, lhs, rhs) => {
            if let solidity::Expression::List(_, parameters) = lhs.as_ref() {
                // Check for a pure assignment without new variable declarations
                if parameters
                    .iter()
                    .all(|(_, p)| p.as_ref().map_or(true, |p| p.name.is_none()))
                {
                    return Ok(sway::Statement::from(sway::Expression::from(
                        sway::BinaryExpression {
                            operator: "=".into(),
                            lhs: sway::Expression::Tuple(
                                parameters
                                    .iter()
                                    .map(|(_, p)| match p.as_ref() {
                                        Some(p) => translate_expression(
                                            project,
                                            module.clone(),
                                            scope.clone(),
                                            &p.ty,
                                        ),
                                        None => Ok(sway::Expression::create_identifier("_".into())),
                                    })
                                    .collect::<Result<Vec<_>, _>>()?,
                            ),
                            rhs: translate_expression(project, module.clone(), scope.clone(), rhs)?,
                        },
                    )));
                }

                // Collect variable translations for the scope
                let mut variables = vec![];

                for (_, p) in parameters.iter() {
                    let Some(p) = p.as_ref() else { continue };
                    let Some(name) = p.name.as_ref() else {
                        continue;
                    };

                    variables.push(Rc::new(RefCell::new(ir::Variable {
                        old_name: name.name.clone(),
                        new_name: translate_naming_convention(name.name.as_str(), Case::Snake),
                        type_name: translate_type_name(
                            project,
                            module.clone(),
                            scope.clone(),
                            &p.ty,
                            false,
                            false,
                        ),
                        ..Default::default()
                    })));
                }

                for variable in variables {
                    scope.borrow_mut().add_variable(variable);
                }

                // Create the variable declaration statement
                return Ok(sway::Statement::from(sway::Let {
                    pattern: sway::LetPattern::Tuple(
                        parameters
                            .iter()
                            .map(|(_, p)| sway::LetIdentifier {
                                is_mutable: false,
                                name: if let Some(p) = p.as_ref() {
                                    if let Some(name) = p.name.as_ref() {
                                        translate_naming_convention(name.name.as_str(), Case::Snake)
                                    } else {
                                        "_".into()
                                    }
                                } else {
                                    "_".into()
                                },
                            })
                            .collect(),
                    ),

                    type_name: Some(sway::TypeName::Tuple {
                        type_names: parameters
                            .iter()
                            .map(|(_, p)| {
                                if let Some(p) = p.as_ref() {
                                    translate_type_name(
                                        project,
                                        module.clone(),
                                        scope.clone(),
                                        &p.ty,
                                        false,
                                        false,
                                    )
                                } else {
                                    sway::TypeName::Identifier {
                                        name: "_".into(),
                                        generic_parameters: None,
                                    }
                                }
                            })
                            .collect(),
                    }),

                    value: translate_expression(
                        project,
                        module.clone(),
                        scope.clone(),
                        rhs.as_ref(),
                    )?,
                }));
            }
        }

        // Check for standalone pre/post decrement statements
        solidity::Expression::PreDecrement(loc, x)
        | solidity::Expression::PostDecrement(loc, x) => {
            return Ok(sway::Statement::from(translate_assignment_expression(
                project,
                module.clone(),
                scope.clone(),
                "-=",
                x,
                &solidity::Expression::NumberLiteral(*loc, "1".into(), String::new(), None),
            )?));
        }

        // Check for standalone pre/post increment statements
        solidity::Expression::PreIncrement(loc, x)
        | solidity::Expression::PostIncrement(loc, x) => {
            return Ok(sway::Statement::from(translate_assignment_expression(
                project,
                module.clone(),
                scope.clone(),
                "+=",
                x,
                &solidity::Expression::NumberLiteral(*loc, "1".into(), String::new(), None),
            )?));
        }

        solidity::Expression::Variable(variable) => {
            return Err(Error::IneffectualStatement(
                module.borrow().path.clone(),
                variable.to_string(),
            ));
        }

        _ => {}
    }

    Ok(sway::Statement::from(translate_expression(
        project,
        module.clone(),
        scope.clone(),
        expression,
    )?))
}

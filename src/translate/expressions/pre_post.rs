use std::{cell::RefCell, rc::Rc};
use solang_parser::pt as solidity;

use crate::{errors::Error, project::Project, sway, translate::{TranslatedDefinition, TranslationScope}};

use super::{assignment::translate_assignment_expression, translate_expression, variable::translate_variable_access_expression};

#[inline]
pub fn translate_pre_or_post_operator_value_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    match expression {
        solidity::Expression::PreIncrement(loc, x) => translate_pre_operator_expression(project, translated_definition, scope.clone(), loc, x, "+="),
        solidity::Expression::PreDecrement(loc, x) => translate_pre_operator_expression(project, translated_definition, scope.clone(), loc, x, "-="),
        solidity::Expression::PostIncrement(loc, x) => translate_post_operator_expression(project, translated_definition, scope.clone(), loc, x, "+="),
        solidity::Expression::PostDecrement(loc, x) => translate_post_operator_expression(project, translated_definition, scope.clone(), loc, x, "-="),
        
        _ => {
            // println!(
            //     "Translating pre- or post-operator value expression: {expression}; from {} - {expression:#?}",
            //     match project.loc_to_line_and_column(&translated_definition.path, &expression.loc()) {
            //         Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
            //         None => format!("{} - ", translated_definition.path.to_string_lossy()),
            //     },
            // );

            let result = translate_expression(project, translated_definition, scope.clone(), expression)?;
            // println!("Translated pre- or post-operator value expression: {}", sway::TabbedDisplayer(&result));
            Ok(result)
        }
    }
}

#[inline]
pub fn translate_pre_operator_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    loc: &solidity::Loc,
    x: &solidity::Expression,
    operator: &str,
) -> Result<sway::Expression, Error> {
    let assignment = sway::Statement::from(
        translate_assignment_expression(project,
            translated_definition,
           scope.clone(),
            operator,
            x,
            &solidity::Expression::NumberLiteral(*loc, "1".into(), "".into(), None),
        )?
    );

    let (variable, expression) = translate_variable_access_expression(project, translated_definition, scope.clone(), x)?;
    if variable.is_none() {
        panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
    }
    let variable = variable.unwrap();
    let mut variable = variable.borrow_mut();

    variable.read_count += 1;

    Ok(sway::Expression::from(sway::Block {
        statements: vec![assignment],
        final_expr: Some(
            if variable.is_storage {
                sway::Expression::from(sway::FunctionCall {
                    function: sway::Expression::from(sway::MemberAccess {
                        expression,
                        member: "read".into(),
                    }),
                    generic_parameters: None,
                    parameters: vec![],
                })
            } else {
                expression
            }
        ),
    }))
}


#[inline]
pub fn translate_post_operator_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    loc: &solidity::Loc,
    x: &solidity::Expression,
    operator: &str,
) -> Result<sway::Expression, Error> {
    let assignment = sway::Statement::from(
        translate_assignment_expression(project,
            translated_definition,
           scope.clone(),
            operator,
            x,
            &solidity::Expression::NumberLiteral(*loc, "1".into(), "".into(), None),
        )?
    );

    let (variable, expression) = translate_variable_access_expression(project, translated_definition, scope.clone(), x)?;
    if variable.is_none() {
        panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
    }
    let variable = variable.unwrap();
    let mut variable = variable.borrow_mut();

    variable.read_count += 1;

    let variable_name = if variable.is_storage {
        variable.new_name.clone()
    } else {
        format!("_{}", variable.new_name)
    };

    Ok(sway::Expression::from(sway::Block {
        statements: vec![
            sway::Statement::from(sway::Let {
                pattern: sway::LetPattern::Identifier(sway::LetIdentifier {
                    is_mutable: false,
                    name: variable_name.clone(),
                }),
                type_name: None,
                value: if variable.is_storage {
                    sway::Expression::from(sway::FunctionCall {
                        function: sway::Expression::from(sway::MemberAccess {
                            expression,
                            member: "read".into(),
                        }),
                        generic_parameters: None,
                        parameters: vec![],
                    })
                } else {
                    expression
                },
            }),
            assignment,
        ],
        final_expr: Some(sway::Expression::Identifier(variable_name)),
    }))
}

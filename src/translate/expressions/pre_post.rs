use crate::{errors::Error, project::Project, sway, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_pre_or_post_operator_value_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    match expression {
        solidity::Expression::PreIncrement(loc, x) => translate_pre_operator_expression(project, translated_definition, scope, loc, x, "+="),
        solidity::Expression::PreDecrement(loc, x) => translate_pre_operator_expression(project, translated_definition, scope, loc, x, "-="),
        solidity::Expression::PostIncrement(loc, x) => translate_post_operator_expression(project, translated_definition, scope, loc, x, "+="),
        solidity::Expression::PostDecrement(loc, x) => translate_post_operator_expression(project, translated_definition, scope, loc, x, "-="),
        
        _ => {
            // println!(
            //     "Translating pre- or post-operator value expression: {expression}; from {} - {expression:#?}",
            //     match project.loc_to_line_and_column(&translated_definition.path, &expression.loc()) {
            //         Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
            //         None => format!("{} - ", translated_definition.path.to_string_lossy()),
            //     },
            // );

            let result = translate_expression(project, translated_definition, scope, expression)?;
            // println!("Translated pre- or post-operator value expression: {}", sway::TabbedDisplayer(&result));
            Ok(result)
        }
    }
}

#[inline]
pub fn translate_pre_operator_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &Rc<RefCell<TranslationScope>>,
    loc: &solidity::Loc,
    x: &solidity::Expression,
    operator: &str,
) -> Result<sway::Expression, Error> {
    let assignment = sway::Statement::from(
        translate_assignment_expression(
            project,
            translated_definition,
            scope,
            operator,
            x,
            &solidity::Expression::NumberLiteral(*loc, "1".into(), String::new(), None),
        )?
    );

    let (variable, expression) = translate_variable_access_expression(project, translated_definition, scope, x)?;

    let Some(variable) = variable else {
        panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
    };

    let mut variable = variable.borrow_mut();
    variable.read_count += 1;

    Ok(sway::Expression::from(sway::Block {
        statements: vec![assignment],
        final_expr: Some(
            if variable.storage_namespace.is_some() {
                sway::Expression::create_function_calls(Some(expression), &[("read", Some((None, vec![])))])
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
    scope: &Rc<RefCell<TranslationScope>>,
    loc: &solidity::Loc,
    x: &solidity::Expression,
    operator: &str,
) -> Result<sway::Expression, Error> {
    let assignment = sway::Statement::from(
        translate_assignment_expression(project,
            translated_definition,
           scope,
            operator,
            x,
            &solidity::Expression::NumberLiteral(*loc, "1".into(), String::new(), None),
        )?
    );

    let (variable, expression) = translate_variable_access_expression(project, translated_definition, scope, x)?;
    if variable.is_none() {
        panic!("Variable not found: {}", sway::TabbedDisplayer(&expression));
    }
    
    let variable = variable.unwrap();
    let mut variable = variable.borrow_mut();

    variable.read_count += 1;

    let variable_name = if variable.storage_namespace.is_some() {
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
                value: if variable.storage_namespace.is_some() {
                    sway::Expression::create_function_calls(Some(expression), &[("read", Some((None, vec![])))])
                } else {
                    expression
                },
            }),
            assignment,
        ],
        final_expr: Some(sway::Expression::create_identifier(variable_name)),
    }))
}

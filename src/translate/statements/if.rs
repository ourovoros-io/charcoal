use std::{cell::RefCell, rc::Rc};
use solang_parser::pt as solidity;
use crate::{errors::Error, project::Project, sway, translate::{translate_expression, TranslatedDefinition, TranslationScope}};
use super::translate_statement;

#[inline]
pub fn translate_if_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    condition: &solidity::Expression,
    then_body: &solidity::Statement,
    else_if: &Option<Box<solidity::Statement>>,
) -> Result<sway::Statement, Error> {
    let condition = translate_expression(project, translated_definition, scope.clone(), condition)?;
    
    let then_body = match translate_statement(project, translated_definition, scope.clone(), then_body)? {
        sway::Statement::Expression(sway::Expression::Block(block)) => *block,
        
        statement => sway::Block {
            statements: vec![statement],
            final_expr: None,
        }
    };

    let else_if = if let Some(else_if) = else_if.as_ref() {
        match translate_statement(project, translated_definition, scope.clone(), else_if.as_ref())? {
            sway::Statement::Expression(sway::Expression::If(else_if)) => Some(else_if.clone()),
            sway::Statement::Expression(sway::Expression::Block(block)) => Some(Box::new(sway::If {
                condition: None,
                then_body: *block,
                else_if: None,
            })),
            statement => Some(Box::new(sway::If {
                condition: None,
                then_body: sway::Block {
                    statements: vec![statement],
                    final_expr: None,
                },
                else_if: None,
            })),
        }
    } else {
        None
    };

    Ok(sway::Statement::from(sway::Expression::from(sway::If {
        condition: Some(condition),
        then_body,
        else_if,
    })))
}

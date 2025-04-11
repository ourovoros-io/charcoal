use std::{cell::RefCell, rc::Rc};
use solang_parser::pt as solidity;
use crate::{errors::Error, project::Project, sway, translate::{expressions::translate_expression, TranslatedDefinition, TranslationScope}};
use super::translate_statement;

#[inline]
pub fn translate_while_statement(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &Rc<RefCell<TranslationScope>>,
    condition: &solidity::Expression,
    body: &solidity::Statement,
) -> Result<sway::Statement, Error> {
    Ok(sway::Statement::from(sway::Expression::from(sway::While {
        condition: translate_expression(project, translated_definition, scope, condition)?,
        body: match translate_statement(project, translated_definition, scope, body)? {
            sway::Statement::Expression(sway::Expression::Block(block)) => *block,
            statement => sway::Block {
                statements: vec![statement],
                final_expr: None,
            }
        },
    })))
}

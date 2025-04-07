use std::{cell::RefCell, rc::Rc};
use solang_parser::pt as solidity;
use crate::{errors::Error, project::Project, sway, translate::{TranslatedDefinition, TranslationScope}};
use super::translate_expression;

#[inline]
pub fn translate_conditional_operator_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: &Rc<RefCell<TranslationScope>>,
    condition: &solidity::Expression,
    then_value: &solidity::Expression,
    else_value: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    // if condition { then_value } else { else_value }
    Ok(sway::Expression::from(sway::If {
        condition: Some(translate_expression(project, translated_definition, scope, condition)?),
        then_body: sway::Block {
            statements: vec![],
            final_expr: Some(
                translate_expression(project, translated_definition, scope, then_value)?
            ),
        },
        else_if: Some(Box::new(sway::If {
            condition: None,
            then_body: sway::Block {
                statements: vec![],
                final_expr: Some(
                    translate_expression(project, translated_definition, scope, else_value)?
                ),
            },
            else_if: None,
        })),
    }))
}

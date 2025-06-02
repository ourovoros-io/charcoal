use crate::{error::Error, project::Project, sway, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_conditional_operator_expression(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: Rc<RefCell<TranslationScope>>,
    condition: &solidity::Expression,
    then_value: &solidity::Expression,
    else_value: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    // if condition { then_value } else { else_value }
    Ok(sway::Expression::from(sway::If {
        condition: Some(translate_expression(project, module.clone(), scope.clone(), condition)?),
        then_body: sway::Block {
            statements: vec![],
            final_expr: Some(
                translate_expression(project, module.clone(), scope.clone(), then_value)?
            ),
        },
        else_if: Some(Box::new(sway::If {
            condition: None,
            then_body: sway::Block {
                statements: vec![],
                final_expr: Some(
                    translate_expression(project, module.clone(), scope.clone(), else_value)?
                ),
            },
            else_if: None,
        })),
    }))
}

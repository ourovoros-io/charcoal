use crate::{error::Error, project::Project, sway, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_do_while_statement(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: Rc<RefCell<TranslationScope>>,
    body: &solidity::Statement,
    condition: &solidity::Expression,
) -> Result<sway::Statement, Error> {
    Ok(sway::Statement::from(sway::Expression::from(sway::While {
        condition: sway::Expression::from(sway::Literal::Bool(true)),
        body: {
            let mut body = match translate_statement(project, module.clone(), scope.clone(), body)? {
                sway::Statement::Expression(sway::Expression::Block(block)) => *block,
                statement => sway::Block {
                    statements: vec![statement],
                    final_expr: None,
                },
            };

            body.statements
                .push(sway::Statement::from(sway::Expression::from(sway::If {
                    condition: Some(sway::Expression::from(sway::UnaryExpression {
                        operator: "!".into(),
                        expression: translate_expression(project, module, scope.clone(), condition)?,
                    })),
                    then_body: sway::Block {
                        statements: vec![sway::Statement::from(sway::Expression::Break)],
                        final_expr: None,
                    },
                    else_if: None,
                })));

            body
        },
    })))
}

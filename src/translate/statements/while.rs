use crate::{error::Error, project::Project, sway, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_while_statement(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    condition: &solidity::Expression,
    body: &solidity::Statement,
) -> Result<sway::Statement, Error> {
    Ok(sway::Statement::from(sway::Expression::from(sway::While {
        condition: translate_expression(project, module.clone(), scope.clone(), condition)?,
        body: match translate_statement(project, module.clone(), scope.clone(), body)? {
            sway::Statement::Expression(sway::Expression::Block(block)) => *block,
            statement => sway::Block {
                statements: vec![statement],
                final_expr: None,
            },
        },
    })))
}

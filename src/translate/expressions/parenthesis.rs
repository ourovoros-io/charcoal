use crate::{error::Error, project::Project, sway, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_parenthesis_expression(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    scope: &Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    let sway_expr = translate_expression(project, module.clone(), scope, expression)?;
    
    match &expression {
        solidity::Expression::Assign(_, lhs, _) => {
            // function add(uint x, uint y) internal pure returns (uint z) {
            //    require((z = x + y) >= x, "ds-math-add-overflow");
            // }
            //
            // fn add(x: u256, y: u256) -> u256 {
            //    let mut z: u256 = 0;
            //    require({z = x + y; z } >= x, "ds-math-add-overflow");
            //    z
            // }
            //
            let sway_lhs = translate_expression(project, module.clone(), scope, lhs)?;
            
            Ok(sway::Expression::from(sway::Block {
                statements: vec![
                    sway::Statement::from(sway_expr),
                ],
                final_expr:  Some(sway_lhs),
            }))
        },
        _ => {
            // (x)
            Ok(sway::Expression::Tuple(vec![
                sway_expr,
            ]))
        }
    }
}

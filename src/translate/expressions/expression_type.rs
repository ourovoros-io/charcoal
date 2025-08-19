use crate::{error::Error, project::Project, sway, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_type_expression(
    _project: &mut Project,
    _module: Rc<RefCell<ir::Module>>,
    _scope: Rc<RefCell<ir::Scope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    //
    // NOTE:
    // Type expressions should never be encountered on their own.
    // They should be handled in a higher level expression.
    //

    unimplemented!("type expression: {expression} - {expression:#?}")
}

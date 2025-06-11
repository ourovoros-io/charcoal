use crate::{error::Error, project::Project, sway, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_args_statement(
    _project: &mut Project,
    _module: Rc<RefCell<ir::Module>>,
    _scope: Rc<RefCell<ir::Scope>>,
    _named_arguments: &[solidity::NamedArgument],
) -> Result<sway::Statement, Error> {
    todo!("translate args statement")
}

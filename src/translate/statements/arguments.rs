use crate::{errors::Error, project::Project, sway, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_args_statement(
    _project: &mut Project,
    _translated_definition: &mut TranslatedDefinition,
    _scope: &Rc<RefCell<TranslationScope>>,
    _named_arguments: &[solidity::NamedArgument],
) -> Result<sway::Statement, Error> {
    todo!("translate args statement")
}

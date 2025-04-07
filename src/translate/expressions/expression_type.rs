use std::{cell::RefCell, rc::Rc};
use solang_parser::pt as solidity;
use crate::{errors::Error, project::Project, sway, translate::{TranslatedDefinition, TranslationScope}};

#[inline]
pub fn translate_type_expression(
    _project: &mut Project,
    _translated_definition: &mut TranslatedDefinition,
    _scope: &Rc<RefCell<TranslationScope>>,
    expression: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    //
    // NOTE:
    // Type expressions should never be encountered on their own.
    // They should be handled in a higher level expression.
    //

    unimplemented!("type expression: {expression} - {expression:#?}")
}

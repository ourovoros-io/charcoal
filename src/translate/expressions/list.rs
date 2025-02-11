use std::{cell::RefCell, rc::Rc};
use solang_parser::pt as solidity;
use crate::{errors::Error, project::Project, sway, translate::{TranslatedDefinition, TranslationScope}};
use super::translate_expression;

#[inline]
pub fn translate_list_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    parameters: &[(solidity::Loc, Option<solidity::Parameter>)],
) -> Result<sway::Expression, Error> {
    //
    // NOTE:
    // Assignments are handled at the statement level, since it's an assignment to a list of variable declarations.
    //

    // Ensure all elements of the list have no name (value-only tuple)
    if !parameters.iter().all(|(_, p)| p.as_ref().unwrap().name.is_none()) {
        unimplemented!("non-value list expression")
    }

    // Create a tuple expression
    Ok(sway::Expression::Tuple(
        parameters.iter()
            .map(|(_, p)| translate_expression(project, translated_definition, scope.clone(), &p.as_ref().unwrap().ty))
            .collect::<Result<Vec<_>, _>>()?
    ))
}

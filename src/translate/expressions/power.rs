use std::{cell::RefCell, rc::Rc};
use solang_parser::pt as solidity;

use crate::{errors::Error, project::Project, sway, translate::{TranslatedDefinition, TranslationScope}};

use super::translate_expression;

#[inline]
pub fn translate_power_expression(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    scope: Rc<RefCell<TranslationScope>>,
    lhs: &solidity::Expression,
    rhs: &solidity::Expression,
) -> Result<sway::Expression, Error> {
    // lhs ** rhs => lhs.pow(rhs)

    // Ensure std::math::Power is imported for the pow function
    translated_definition.ensure_use_declared("std::math::Power");

    let lhs = translate_expression(project, translated_definition, scope.clone(), lhs)?;
    let rhs = translate_expression(project, translated_definition, scope.clone(), rhs)?;

    Ok(sway::Expression::from(sway::FunctionCall {
        function: sway::Expression::from(sway::MemberAccess {
            expression: lhs,
            member: "pow".into(),
        }),
        generic_parameters: None,
        parameters: vec![
            rhs,
        ],
    }))
}

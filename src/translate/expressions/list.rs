use crate::{error::Error, project::Project, sway, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_list_expression(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
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
        parameters
            .iter()
            .map(|(_, p)| translate_expression(project, module.clone(), scope.clone(), &p.as_ref().unwrap().ty))
            .collect::<Result<Vec<_>, _>>()?,
    ))
}

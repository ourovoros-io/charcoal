use crate::{error::Error, ir, project::Project, sway, translate::*};
use solang_parser::pt as solidity;

#[inline]
pub fn translate_type_definition(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    type_definition: &solidity::TypeDefinition,
) -> Result<sway::TypeDefinition, Error> {
    let underlying_type =
        translate_type_name(project, module.clone(), &type_definition.ty, false, false);

    Ok(sway::TypeDefinition {
        is_public: true,
        name: sway::TypeName::Identifier {
            name: type_definition.name.name.clone(),
            generic_parameters: None,
        },
        underlying_type: Some(underlying_type),
    })
}

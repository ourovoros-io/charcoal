use crate::{error::Error, project::Project, sway, translate::*};
use solang_parser::pt as solidity;

#[inline]
pub fn translate_type_definition(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    type_definition: &solidity::TypeDefinition,
) -> Result<(), Error> {
    let underlying_type =
        translate_type_name(project, module.clone(), &type_definition.ty, false, false);

    module
        .borrow_mut()
        .type_definitions
        .push(sway::TypeDefinition {
            is_public: true,
            name: sway::TypeName::Identifier {
                name: type_definition.name.name.clone(),
                generic_parameters: None,
            },
            underlying_type: Some(underlying_type),
        });

    Ok(())
}

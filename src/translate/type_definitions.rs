use super::{translate_type_name, TranslatedDefinition};
use crate::{project::Project, sway, Error};
use solang_parser::pt as solidity;

#[inline]
pub fn translate_type_definition(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    type_definition: &solidity::TypeDefinition,
) -> Result<(), Error> {
    let underlying_type = translate_type_name(project, translated_definition, &type_definition.ty, false);

    translated_definition.type_definitions.push(sway::TypeDefinition {
        is_public: true,
        name: sway::TypeName::Identifier {
            name: type_definition.name.name.clone(),
            generic_parameters: None,
        },
        underlying_type: Some(underlying_type),
    });

    Ok(())
}

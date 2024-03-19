use super::{translate_type_name, TranslatedDefinition};
use crate::{project::Project, sway, Error};
use convert_case::Case;
use solang_parser::pt as solidity;

#[inline]
pub fn translate_struct_definition(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    struct_definition: &solidity::StructDefinition,
) -> Result<(), Error> {
    let struct_definition = sway::Struct {
        attributes: None,
        is_public: true,
        name: struct_definition.name.as_ref().unwrap().name.clone(),
        generic_parameters: None,
        fields: struct_definition.fields.iter().map(|f| {
            sway::StructField {
                is_public: true,
                name: crate::translate_naming_convention(f.name.as_ref().unwrap().name.as_str(), Case::Snake), // TODO: keep track of original name
                type_name: translate_type_name(project, translated_definition, &f.ty, false, false),
            }
        }).collect(),
    };

    translated_definition.structs.push(struct_definition);

    Ok(())
}

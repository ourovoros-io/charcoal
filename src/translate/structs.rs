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
        is_public: false,
        name: struct_definition.name.as_ref().unwrap().name.clone(),
        generic_parameters: None,
        fields: struct_definition.fields.iter().map(|f| {
            sway::StructField {
                is_public: false,
                name: crate::translate_naming_convention(f.name.as_ref().unwrap().name.as_str(), Case::Snake), // TODO: keep track of original name
                type_name: translate_type_name(project, translated_definition, &f.ty, false, false),
            }
        }).collect(),
    };
    for field in struct_definition.fields.iter() {
        match &field.type_name {
            sway::TypeName::Identifier{ name, .. } => {
                if !translated_definition.structs.iter().any(|s| s.name == *name) {
                    for external_definition in project.translated_definitions.iter() {
                        for s in external_definition.structs.iter() {
                            if s.name == *name {
                                translated_definition.structs.push(s.clone());
                            }
                        }
                    }                        
                }
            },
            
            _ => {}
        }
    }

    translated_definition.structs.push(struct_definition);

    Ok(())
}

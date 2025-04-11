use crate::{project::Project, sway, Error};
use convert_case::Case;
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

use super::{type_names::translate_type_name, TranslatedDefinition};

#[inline]
pub fn translate_struct_definition(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    struct_definition: &solidity::StructDefinition,
) -> Result<(), Error> {
    let mut fields = vec![];

    for field in struct_definition.fields.iter() {
        // TODO: keep track of original struct name?
        let name = crate::translate::translate_naming_convention(field.name.as_ref().unwrap().name.as_str(), Case::Snake);
        let mut type_name = translate_type_name(project, translated_definition, &field.ty, false, false);

        if let sway::TypeName::Identifier { name, generic_parameters } = &type_name {
            match (name.as_str(), generic_parameters.as_ref()) {
                ("StorageMap" | "StorageVec", Some(_)) => {
                    // HACK: wrap storage types in a StorageKey
                    type_name = sway::TypeName::Identifier {
                        name: "StorageKey".into(),
                        generic_parameters: Some(sway::GenericParameterList {
                            entries: vec![
                                sway::GenericParameter {
                                    type_name,
                                    implements: None,
                                },
                            ],
                        }),
                    };
                }

                (name, generic_parameters) => {
                    // HACK: import field types if we haven't already
                    if generic_parameters.is_none() && !translated_definition.structs.iter().any(|s| s.borrow().name == *name) {
                        'lookup: for external_definition in project.translated_definitions.iter() {
                            // Check if the field type is a struct
                            for external_struct in external_definition.structs.iter() {
                                if external_struct.borrow().name == *name {
                                    translated_definition.ensure_struct_included(project, &external_struct.clone());
                                    break 'lookup;
                                }
                            }
                        }
                    }
                }
            }
        }

        if type_name.is_storage_key() {
            type_name = sway::TypeName::Identifier {
                name: "Option".into(),
                generic_parameters: Some(sway::GenericParameterList {
                    entries: vec![sway::GenericParameter {
                        type_name,
                        implements: None,
                    }],
                }),
            }
        }

        fields.push(sway::StructField {
            is_public: false,
            name,
            type_name,
        });
    }

    translated_definition.structs.push(Rc::new(RefCell::new(sway::Struct {
        attributes: None,
        is_public: false,
        name: struct_definition.name.as_ref().unwrap().name.clone(),
        generic_parameters: None,
        fields,
    })));

    Ok(())
}

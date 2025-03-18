use super::{translate_type_name, TranslatedDefinition};
use crate::{project::Project, sway, translate_naming_convention, Error};
use convert_case::Case;
use num_bigint::BigUint;
use num_traits::Zero;
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_struct_definition(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    struct_definition: &solidity::StructDefinition,
) -> Result<(), Error> {
    let mut fields = vec![];

    for field in struct_definition.fields.iter() {
        // TODO: keep track of original struct name?
        let name = crate::translate_naming_convention(field.name.as_ref().unwrap().name.as_str(), Case::Snake);
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
                                    translated_definition.ensure_struct_included(project, external_struct.clone());
                                    break 'lookup;
                                }
                            }
                        }
                    }
                }
            }
        }

        if let Some(storage_type) = type_name.storage_key_type() {
            let struct_name = translate_naming_convention(&struct_definition.name.as_ref().map(|n| n.name.clone()).unwrap(), Case::Snake);
            if !translated_definition.mapping_names.iter().any(|(n, _)| *n == struct_name) {
                translated_definition.mapping_names.push((struct_name.clone(), vec![]));
            }

            let mapping_names = translated_definition.mapping_names.iter_mut().find(|m| m.0 == struct_name).unwrap();
            mapping_names.1.push(name.clone());

            let instance_field_name = format!("{}_instance_count", struct_name);
            let mapping_field_name = format!("{}_{}s", struct_name, name);

            let storage = translated_definition.get_storage();

            if let Some(field) = storage.fields.iter().find(|f| f.name == instance_field_name) {
                if !field.type_name.is_u64() {
                    panic!("Instance count field already exists : {field:#?}");
                }
            } else {
                storage.fields.push(
                    sway::StorageField { 
                        name: instance_field_name, 
                        type_name: sway::TypeName::Identifier { 
                            name: "u64".into(), 
                            generic_parameters: None 
                        }, 
                        value: sway::Expression::Literal(sway::Literal::DecInt(BigUint::zero(), None)) 
                    }
                );
            }

            if let Some(field) = storage.fields.iter().find(|f| f.name == mapping_field_name) {
                if let Some((k, v)) = field.type_name.storage_map_type() {
                    if !k.is_u64() && !v.is_compatible_with(&storage_type) {
                        panic!("Instance mapping field already exists : {field:#?}");
                    }
                } else {
                    panic!("Instance mapping field already exists : {field:#?}");
                }
            } else {
                storage.fields.push(
                    sway::StorageField { 
                        name: mapping_field_name, 
                        type_name: sway::TypeName::Identifier { 
                            name: "StorageMap".into(), 
                            generic_parameters: Some(sway::GenericParameterList{ 
                                entries: vec![
                                    sway::GenericParameter { 
                                        type_name: sway::TypeName::Identifier { 
                                            name: "u64".into(), 
                                            generic_parameters: None, 
                                        }, 
                                        implements: None,
                                    },
                                    sway::GenericParameter { 
                                        type_name: type_name.storage_key_type().unwrap(), 
                                        implements: None, 
                                    }
                                ] 
                            })
                        }, 
                        value: sway::Expression::from(sway::Constructor { 
                            type_name: sway::TypeName::Identifier { 
                                name: "StorageMap".into(), 
                                generic_parameters: None,
                            }, 
                            fields: vec![] 
                        })
                    }
                );
            }

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

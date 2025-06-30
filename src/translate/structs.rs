use crate::{error::Error, ir, project::Project, sway, translate::*};
use convert_case::Case;
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_struct_definition(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    contract_name: Option<&str>,
    struct_definition: &solidity::StructDefinition,
) -> Result<Rc<RefCell<sway::Struct>>, Error> {
    let mut fields = vec![];

    let scope = Rc::new(RefCell::new(ir::Scope::new(contract_name, None, None)));

    for field in struct_definition.fields.iter() {
        let old_name = field.name.as_ref().unwrap().name.clone();
        let new_name = translate_naming_convention(old_name.as_str(), Case::Snake);

        let mut type_name = translate_type_name(
            project,
            module.clone(),
            scope.clone(),
            &field.ty,
            field.storage.as_ref(),
        );

        if let sway::TypeName::Identifier {
            name,
            generic_parameters,
        } = &type_name
        {
            match (name.as_str(), generic_parameters.as_ref()) {
                ("StorageMap" | "StorageVec", Some(_)) => {
                    // HACK: wrap storage types in a StorageKey
                    type_name = sway::TypeName::Identifier {
                        name: "StorageKey".into(),
                        generic_parameters: Some(sway::GenericParameterList {
                            entries: vec![sway::GenericParameter {
                                type_name,
                                implements: None,
                            }],
                        }),
                    };
                }

                _ => {}
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
            new_name,
            old_name,
            type_name,
        });
    }

    Ok(Rc::new(RefCell::new(sway::Struct {
        attributes: None,
        is_public: false,
        name: struct_definition.name.as_ref().unwrap().name.clone(),
        generic_parameters: None,
        fields,
    })))
}

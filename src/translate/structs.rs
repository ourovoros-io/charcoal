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
) -> Result<Rc<RefCell<ir::Struct>>, Error> {
    let mut memory_fields = vec![];
    let mut storage_fields = vec![];

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
            if let ("StorageMap" | "StorageVec", Some(_)) =
                (name.as_str(), generic_parameters.as_ref())
            {
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

        memory_fields.push(sway::StructField {
            is_public: true,
            new_name: new_name.clone(),
            old_name: old_name.clone(),
            type_name: type_name.clone(),
        });

        fn memory_to_storage_type(type_name: sway::TypeName) -> sway::TypeName {
            if let Some(mut vec_type) = type_name.vec_type() {
                vec_type = memory_to_storage_type(vec_type);

                return sway::TypeName::Identifier {
                    name: "Option".into(),
                    generic_parameters: Some(sway::GenericParameterList {
                        entries: vec![sway::GenericParameter {
                            type_name: sway::TypeName::Identifier {
                                name: "StorageKey".to_string(),
                                generic_parameters: Some(sway::GenericParameterList {
                                    entries: vec![sway::GenericParameter {
                                        type_name: sway::TypeName::Identifier {
                                            name: "StorageVec".to_string(),
                                            generic_parameters: Some(sway::GenericParameterList {
                                                entries: vec![sway::GenericParameter {
                                                    type_name: vec_type,
                                                    implements: None,
                                                }],
                                            }),
                                        },
                                        implements: None,
                                    }],
                                }),
                            },
                            implements: None,
                        }],
                    }),
                };
            }

            if type_name.is_bytes() {
                return sway::TypeName::Identifier {
                    name: "Option".into(),
                    generic_parameters: Some(sway::GenericParameterList {
                        entries: vec![sway::GenericParameter {
                            type_name: sway::TypeName::Identifier {
                                name: "StorageKey".to_string(),
                                generic_parameters: Some(sway::GenericParameterList {
                                    entries: vec![sway::GenericParameter {
                                        type_name: sway::TypeName::Identifier {
                                            name: "StorageBytes".to_string(),
                                            generic_parameters: None,
                                        },
                                        implements: None,
                                    }],
                                }),
                            },
                            implements: None,
                        }],
                    }),
                };
            }

            if type_name.is_string() {
                return sway::TypeName::Identifier {
                    name: "Option".into(),
                    generic_parameters: Some(sway::GenericParameterList {
                        entries: vec![sway::GenericParameter {
                            type_name: sway::TypeName::Identifier {
                                name: "StorageKey".to_string(),
                                generic_parameters: Some(sway::GenericParameterList {
                                    entries: vec![sway::GenericParameter {
                                        type_name: sway::TypeName::Identifier {
                                            name: "StorageString".to_string(),
                                            generic_parameters: None,
                                        },
                                        implements: None,
                                    }],
                                }),
                            },
                            implements: None,
                        }],
                    }),
                };
            }

            type_name
        }

        storage_fields.push(sway::StructField {
            is_public: true,
            new_name,
            old_name,
            type_name: memory_to_storage_type(type_name),
        });
    }

    let struct_name = struct_definition.name.as_ref().unwrap().name.clone();

    Ok(Rc::new(RefCell::new(ir::Struct {
        name: struct_name.clone(),
        memory: sway::Struct {
            attributes: None,
            is_public: true,
            name: struct_name.clone(),
            generic_parameters: None,
            fields: memory_fields,
        },
        storage: sway::Struct {
            attributes: None,
            is_public: true,
            name: format!("Storage{struct_name}"),
            generic_parameters: None,
            fields: storage_fields,
        },
    })))
}

pub fn create_struct_constructor(
    project: &mut Project,
    module: Rc<RefCell<ir::Module>>,
    scope: Rc<RefCell<ir::Scope>>,
    contract_name: &str,
) -> Option<sway::Constructor> {
    let mut fields = vec![];

    let contract = project.find_contract(module.clone(), contract_name)?;

    let storage_namespace_name = contract.borrow().name.to_case(Case::Snake);

    let storage_namespace = contract
        .borrow()
        .storage
        .as_ref()?
        .borrow()
        .namespaces
        .iter()
        .find(|s| s.borrow().name == storage_namespace_name)
        .cloned()?;

    let storage_struct = contract.borrow().storage_struct.clone()?;

    for field in storage_struct.borrow().storage.fields.iter() {
        if !storage_namespace
            .borrow()
            .fields
            .iter()
            .any(|f| f.name == field.new_name)
            && field.new_name != "constructor_called"
        {
            let inherited_contract_name = field.new_name.to_case(Case::Pascal);
            let field = sway::ConstructorField {
                name: field.new_name.clone(),
                value: sway::Expression::from(create_struct_constructor(
                    project,
                    module.clone(),
                    scope.clone(),
                    &inherited_contract_name,
                )?),
            };
            fields.push(field);
        } else {
            fields.push(sway::ConstructorField {
                name: field.new_name.clone(),
                value: sway::Expression::from(sway::MemberAccess {
                    expression: sway::Expression::from(sway::PathExpr {
                        root: sway::PathExprRoot::Identifier("storage".to_string()),
                        segments: vec![sway::PathExprSegment {
                            name: storage_namespace.borrow().name.clone(),
                            generic_parameters: None,
                        }],
                    }),
                    member: field.new_name.clone(),
                }),
            })
        }
    }

    Some(sway::Constructor {
        type_name: sway::TypeName::Identifier {
            name: format!("{contract_name}Storage"),
            generic_parameters: None,
        },
        fields,
    })
}

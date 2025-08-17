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

        // Wrap storage types in a `StorageKey<T>`
        if type_name.is_storage_bytes()
            || type_name.is_storage_map()
            || type_name.is_storage_string()
            || type_name.is_storage_vec()
        {
            type_name = type_name.to_storage_key();
        }

        memory_fields.push(sway::StructField {
            is_public: true,
            new_name: new_name.clone(),
            old_name: old_name.clone(),
            type_name: type_name.clone(),
        });

        // HACK: Wrap `StorageKey<T>` in `Option<T>` so it can be deferred
        if type_name.is_storage_key() {
            type_name = type_name.to_option();
        }

        storage_fields.push(sway::StructField {
            is_public: true,
            new_name,
            old_name,
            type_name: type_name.to_storage_compatible_type(),
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
        type_name: sway::TypeName::create_identifier(format!("{contract_name}Storage").as_str()),
        fields,
    })
}

use crate::{error::Error, project::Project, translate::*};
use solang_parser::pt as solidity;
use std::{cell::RefCell, rc::Rc};

#[inline]
pub fn translate_state_variable(
    project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    variable_definition: &solidity::VariableDefinition,
) -> Result<(), Error> {
    // Collect information about the variable from its attributes
    let is_public = variable_definition.attrs.iter().any(|x| {
        matches!(
            x,
            solidity::VariableAttribute::Visibility(
                solidity::Visibility::External(_) | solidity::Visibility::Public(_)
            )
        )
    });

    let is_constant = variable_definition
        .attrs
        .iter()
        .any(|x| matches!(x, solidity::VariableAttribute::Constant(_)));

    let is_immutable = variable_definition
        .attrs
        .iter()
        .any(|x| matches!(x, solidity::VariableAttribute::Immutable(_)));

    // If the state variable is not constant or immutable, it is a storage field
    let storage_namespace = if !is_constant && !is_immutable {
        Some(translate_naming_convention(
            &module.borrow().name,
            Case::Snake,
        ))
    } else {
        None
    };

    let is_storage = storage_namespace.is_some();

    // Translate the variable's naming convention
    let old_name = variable_definition.name.as_ref().unwrap().name.clone();

    let new_name = if is_constant || is_immutable {
        translate_naming_convention(old_name.as_str(), Case::Constant)
    } else {
        translate_naming_convention(old_name.as_str(), Case::Snake)
    };

    // Translate the variable's type name
    let mut variable_type_name = translate_type_name(
        project,
        module.clone(),
        &variable_definition.ty,
        is_storage,
        false,
    );

    // Check if the variable's type is an ABI
    let mut abi_type_name = None;

    if let sway::TypeName::Identifier {
        name,
        generic_parameters: None,
    } = &variable_type_name
    {
        //
        // TODO:
        // Check if type is a contract that hasn't been defined yet
        //

        // if !project.translated_modules.iter().any(|module| {
        //     module
        //         .borrow()
        //         .contracts
        //         .iter()
        //         .any(|contract| contract.name == *name)
        // }) {
        //     project.translate(Some(name), &module.path).unwrap();
        // }

        if let Some(external_definition) = project.translated_modules.iter().find(|module| {
            module.borrow().contracts.iter().any(|contract| contract.name == *name)
        }) {
            // TODO:
            // for entry in external_definition.uses.iter() {
            //     if !module.uses.contains(entry) {
            //         module.uses.push(entry.clone());
            //     }
            // }

            //
            // TODO: keep track of this
            //
            
            abi_type_name = Some(variable_type_name.clone());

            variable_type_name = sway::TypeName::Identifier {
                name: "Identity".into(),
                generic_parameters: None,
            };
        }
    }

    // Translate the variable's initial value
    let value_scope = Rc::new(RefCell::default());

    let value = if let Some(x) = variable_definition.initializer.as_ref() {
        let value = translate_expression(project, module.clone(), &value_scope, x)?;
        create_value_expression(
            module.clone(),
            value_scope.clone(),
            &variable_type_name,
            Some(&value),
        )
    } else {
        create_value_expression(
            module.clone(),
            value_scope.clone(),
            &variable_type_name,
            None,
        )
    };

    // Handle constant variable definitions
    if is_constant {
        let scope = Rc::new(RefCell::new(TranslationScope::default()));

        // Evaluate the value ahead of time in order to generate an appropriate constant value expression
        let value = evaluate_expression(module.clone(), scope, &variable_type_name, &value);

        module.borrow_mut().constants.push(sway::Constant {
            is_public,
            name: new_name.clone(),
            type_name: variable_type_name.clone(),
            value: Some(value),
        });
    }
    // Handle immutable variable definitions
    else if is_immutable {
        //
        // TODO: we need to check if the value is supplied to the constructor and remove it from there
        //

        module
            .borrow_mut()
            .get_configurable()
            .fields
            .push(sway::ConfigurableField {
                name: new_name.clone(),
                type_name: variable_type_name.clone(),
                value,
            });
    }
    // Handle regular state variable definitions
    else if storage_namespace.is_some() {
        module
            .borrow_mut()
            .get_storage_namespace()
            .fields
            .push(sway::StorageField {
                name: new_name.clone(),
                type_name: variable_type_name.clone(),
                value,
            });
    }

    Ok(())
}

#[inline]
pub fn translate_storage_name(
    _project: &mut Project,
    module: Rc<RefCell<TranslatedModule>>,
    name: &str,
) -> String {
    let mut module = module.borrow_mut();
    if !module.storage_fields_names.contains_key(name) {
        let mut new_name = translate_naming_convention(name, Case::Snake);

        let count = module
            .storage_fields_name_counts
            .entry(new_name.clone())
            .or_insert(0);
        *count += 1;

        if *count > 1 {
            new_name = format!("{new_name}_{}", *count);
        }

        module.storage_fields_names.insert(name.into(), new_name);
    }

    module.storage_fields_names.get(name).unwrap().clone()
}

use super::{TranslatedDefinition, TranslatedFunction};
use crate::{errors::Error, project::Project, sway};
use solang_parser::{helpers::CodeLocation, pt as solidity};
use std::{cell::RefCell, path::{Path, PathBuf}, rc::Rc};

pub fn resolve_import(
    project: &mut Project,
    definition_name: &String,
    source_unit_path: &Path,
) -> Result<Option<TranslatedDefinition>, Error> {
    let mut source_unit_path = PathBuf::from(source_unit_path);
    let source_unit_directory = source_unit_path.parent().map(PathBuf::from).unwrap();
    
    if !source_unit_path.to_string_lossy().starts_with('.') {
        source_unit_path = project.get_project_type_path(&source_unit_directory, source_unit_path.to_string_lossy().to_string().as_str())?;
    } else {
        source_unit_path = source_unit_directory.join(source_unit_path);
    }
    
    source_unit_path = crate::get_canonical_path(source_unit_path, false, false)
        .map_err(|e| Error::Wrapped(Box::new(e))).unwrap();
    
    if !source_unit_path.exists() {
        return Err(Error::Wrapped(Box::new(std::io::Error::new(std::io::ErrorKind::NotFound, source_unit_path.to_string_lossy()))));
    }

    if let Some(t) = project.translated_definitions.iter().find(|t| t.name == *definition_name && t.path == source_unit_path).cloned() {
        return Ok(Some(t));
    }
    
    project.translate(Some(definition_name), &source_unit_path)?;

    if let Some(t) = project.translated_definitions.iter().find(|t| t.name == *definition_name && t.path == source_unit_path).cloned() {
        return Ok(Some(t));
    }

    Ok(None)
}

fn process_import(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    symbol: Option<&str>,
    import_path: &Path,
) -> Result<bool, Error> {
    if !project.translated_definitions.iter().any(|t| t.path == import_path) {
        project.translate(None, &import_path)?;
    }

    let mut found = false;

    for external_definition in project.translated_definitions.iter() {
        if external_definition.path != import_path {
            continue;
        }

        let include_all = matches!(external_definition.kind, Some(solidity::ContractTy::Abstract(_) | solidity::ContractTy::Library(_)));

        if let Some(symbol) = symbol {
            if external_definition.name == symbol {
                found = true;
            }
        }

        // Extend type definitions
        for type_definition in external_definition.type_definitions.iter() {
            if let Some(symbol) = symbol {
                if !include_all && symbol != type_definition.name.to_string() {
                    continue;
                }
            }

            found = true;

            if !translated_definition.type_definitions.contains(type_definition) {
                translated_definition.type_definitions.push(type_definition.clone());
            }
        }

        // Extend struct definitions
        for struct_definition in external_definition.structs.iter() {
            if let Some(symbol) = symbol {
                if !include_all && symbol != struct_definition.borrow().name {
                    continue;
                }
            }

            found = true;

            if !translated_definition.structs.contains(struct_definition) {
                translated_definition.structs.push(struct_definition.clone());
                translated_definition.struct_names.push(struct_definition.borrow().name.clone());
            }
        }

        // Extend enum definitions
        for enum_definition in external_definition.enums.iter() {
            if let Some(symbol) = symbol {
                if !include_all && symbol != enum_definition.type_definition.name.to_string() {
                    continue;
                }
            }

            found = true;

            if !translated_definition.enums.contains(enum_definition) {
                translated_definition.enums.push(enum_definition.clone());
            }
        }

        //
        // TODO: events and errors
        //

        let mut extend_function = |function_definition: &sway::Function, scope_entry: Rc<RefCell<TranslatedFunction>>| {
            if translated_definition.toplevel_scope.borrow().find_function(|f| f.borrow().new_name == function_definition.name).is_none() {
                let external_function = scope_entry.borrow();

                translated_definition.toplevel_scope.borrow_mut().functions.push(Rc::new(RefCell::new(TranslatedFunction {
                    old_name: external_function.old_name.clone(),
                    new_name: external_function.new_name.clone(),
                    attributes: external_function.attributes.clone(),
                    constructor_calls: external_function.constructor_calls.clone(),
                    modifiers: external_function.modifiers.clone(),
                    type_name: external_function.type_name.clone(),
                })));
            }

            if !translated_definition.functions.contains(function_definition) {
                translated_definition.functions.push(function_definition.clone());
            }

            if let Some(entry) = external_definition.function_names.iter().find(|&(_, v)| *v == function_definition.name) {
                if !translated_definition.function_names.contains_key(entry.0) {
                    translated_definition.function_names.insert(entry.0.clone(), entry.1.clone());
                }
            }

            if let Some(count) = external_definition.function_call_counts.get(&function_definition.name) {
                let entry = translated_definition.function_call_counts.entry(function_definition.name.clone()).or_default();
                *entry += *count;
            }

            if let Some(functions_called) = external_definition.functions_called.get(&function_definition.name) {
                let entry = translated_definition.functions_called.entry(function_definition.name.clone()).or_default();
                
                for function_called in functions_called.iter() {
                    if !entry.contains(function_called) {
                        entry.push(function_called.clone());
                    }
                }
            }
        };

        // Extend the modifiers
        for modifier_definition in external_definition.modifiers.iter() {
            if let Some(symbol) = symbol {
                if !include_all && symbol != modifier_definition.old_name {
                    continue;
                }
            }

            found = true;

            if !translated_definition.modifiers.contains(modifier_definition) {
                translated_definition.modifiers.push(modifier_definition.clone());
            }

            //
            // TODO: extend toplevel modifier functions
            //

            match (modifier_definition.pre_body.as_ref(), modifier_definition.post_body.as_ref()) {
                (Some(_), Some(_)) => {
                    // TODO: separate pre and post functions
                }

                _ => {
                    // TODO: single functions
                }
            }
        }

        // Extend the functions
        for function_definition in external_definition.functions.iter() {
            let Some(scope_entry) = external_definition.toplevel_scope.borrow().find_function(|f| f.borrow().new_name == function_definition.name) else {
                external_definition.toplevel_scope.borrow().dump(false, true);
                panic!("Failed to find function: {}", function_definition.name)
            };

            if let Some(symbol) = symbol {
                if !include_all && symbol != scope_entry.borrow().old_name {
                    continue;
                }
            }

            found = true;
            extend_function(function_definition, scope_entry.clone());
        }

        // Extend the abis
        if let Some(abi) = external_definition.abi.as_ref() {
            if !include_all && symbol.is_none() || symbol.unwrap() == abi.name {
                found = true;
        
                if !translated_definition.abis.contains(abi) {
                    translated_definition.abis.push(abi.clone());
                }
            }
        }

        for abi in external_definition.abis.iter() {
            if let Some(symbol) = symbol {
                if !include_all && symbol != abi.name {
                    continue;
                }
            }

            found = true;

            if !translated_definition.abis.contains(abi) {
                translated_definition.abis.push(abi.clone());
            }
        }
    }

    Ok(found)
}

#[inline]
pub fn translate_import_directives(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    import_directives: &[solidity::Import],
) -> Result<(), Error> {
    let source_unit_directory = translated_definition.path.parent().map(PathBuf::from).unwrap();

    for import_directive in import_directives.iter() {
        match import_directive {
            solidity::Import::Plain(solidity::ImportPath::Filename(filename), _) => {
                let import_path = project.canonicalize_import_path(&source_unit_directory, &filename.string)?;
                let found = process_import(project, translated_definition, None, &import_path)?;
                if !found {
                    panic!(
                        "{}ERROR: failed to resolve import directive from `{import_directive}`",
                        match project.loc_to_line_and_column(&translated_definition.path, &import_directive.loc()) {
                            Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                            None => format!("{} - ", translated_definition.path.to_string_lossy()),
                        }
                    );
                }
            }

            solidity::Import::Rename(solidity::ImportPath::Filename(filename), identifiers, _) => {
                for (identifier, alias_identifier) in identifiers.iter() {
                    if alias_identifier.is_some() {
                        todo!("Handle import aliases");
                    }
                    
                    let import_path = project.canonicalize_import_path(&source_unit_directory, &filename.string)?;
                    let found = process_import(project, translated_definition, Some(&identifier.name), &import_path)?;
                    
                    if !found {
                        project.translate(None, &import_path)?;
                        let found = process_import(project, translated_definition, Some(&identifier.name), &import_path)?;

                        if !found {
                            panic!(
                                "{}ERROR: failed to resolve import directive from `{import_directive}`",
                                match project.loc_to_line_and_column(&translated_definition.path, &import_directive.loc()) {
                                    Some((line, col)) => format!("{}:{}:{} - ", translated_definition.path.to_string_lossy(), line, col),
                                    None => format!("{} - ", translated_definition.path.to_string_lossy()),
                                }
                            );
                        }
                    }
                }
            }

            _ => panic!("Unsupported import directive: {import_directive:#?}"),
        }
    }

    Ok(())
}

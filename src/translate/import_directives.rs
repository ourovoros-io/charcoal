use super::TranslatedDefinition;
use crate::{errors::Error, project::Project};
use solang_parser::pt as solidity;
use std::path::{Path, PathBuf};

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
    
    if let Some(import_directives) = project.import_directives.get(&source_unit_path).cloned() {
        for (path, definition_names) in import_directives.iter() {
            match definition_names.as_ref() {
                Some(definition_names) => {
                    if definition_names.iter().any(|n| n == definition_name) {
                        return resolve_import(project, definition_name, path);
                    }
                }

                None => {
                    if let Ok(Some(t)) = resolve_import(project, definition_name, path) {
                        return Ok(Some(t));
                    }
                }
            }
        }
    }

    project.translate(Some(definition_name), &source_unit_path)?;

    if let Some(t) = project.translated_definitions.iter().find(|t| t.name == *definition_name && t.path == source_unit_path).cloned() {
        return Ok(Some(t));
    }
    
    Ok(None)
}

#[inline]
pub fn translate_import_directives(
    project: &mut Project,
    translated_definition: &mut TranslatedDefinition,
    import_directives: &[solidity::Import],
) -> Result<(), Error> {
    let source_unit_directory = translated_definition.path.parent().map(PathBuf::from).unwrap();

    for import_directive in import_directives.iter() {
        fn translate_import_directive(
            project: &mut Project,
            definition_name: Option<&String>,
            import_path: &Path,
        ) -> Result<(), Error> {
            if !project.translated_definitions.iter().any(|t| definition_name.map(|n| *n == t.name).unwrap_or(true) && t.path == import_path) {
                let mut resolved = None;
                
                if let Some(definition_name) = definition_name {
                    resolved = resolve_import(project, definition_name, &import_path)?;
                } 

                if resolved.is_none() {
                    project.translate(definition_name, &import_path)?;
                }
            }

            Ok(())
        }

        match import_directive {
            solidity::Import::Plain(solidity::ImportPath::Filename(filename), _) => {
                let mut import_path = PathBuf::from(filename.string.clone());

                if !import_path.to_string_lossy().starts_with('.') {
                    import_path = project.get_project_type_path(source_unit_directory.as_path(), filename.string.as_str())?;
                } else {
                    import_path = source_unit_directory.join(import_path);
                }
                
                import_path = crate::get_canonical_path(import_path, false, false)
                    .map_err(|e| Error::Wrapped(Box::new(e))).unwrap();
                
                if !import_path.exists() {
                    return Err(Error::Wrapped(Box::new(
                        std::io::Error::new(
                            std::io::ErrorKind::NotFound,
                            format!("File not found: {}", import_path.to_string_lossy()),
                        )
                    )));
                }

                translate_import_directive(project, None, &import_path)?;

                for external_definition in project.translated_definitions.iter() {
                    if external_definition.path != import_path {
                        continue;
                    }

                    for type_definition in external_definition.type_definitions.iter() {
                        // Extend type definitions
                        if !translated_definition.type_definitions.contains(type_definition) {
                            translated_definition.type_definitions.push(type_definition.clone());
                        }
                    }
                }
            }

            solidity::Import::Rename(solidity::ImportPath::Filename(filename), identifiers, _) => {
                for (identifier, _) in identifiers.iter() {
                    let mut import_path = PathBuf::from(filename.string.clone());
    
                    if !import_path.to_string_lossy().starts_with('.') {
                        import_path = project.get_project_type_path(source_unit_directory.as_path(), filename.string.as_str())?;
                    } else {
                        import_path = source_unit_directory.join(import_path);
                    }
                    
                    import_path = crate::get_canonical_path(import_path, false, false)
                        .map_err(|e| Error::Wrapped(Box::new(e))).unwrap();
                    
                    if !import_path.exists() {
                        return Err(Error::Wrapped(Box::new(
                            std::io::Error::new(
                                std::io::ErrorKind::NotFound,
                                format!("File not found: {}", import_path.to_string_lossy()),
                            )
                        )));
                    }
    
                    translate_import_directive(project, Some(&identifier.name), &import_path)?;

                    for external_definition in project.translated_definitions.iter() {
                        if external_definition.path != import_path {
                            continue;
                        }
    
                        // Extend type definitions
                        for type_definition in external_definition.type_definitions.iter() {
                            if identifier.name == type_definition.name.to_string() {
                                if !translated_definition.type_definitions.contains(type_definition) {
                                    translated_definition.type_definitions.push(type_definition.clone());
                                }
                                break;
                            }
                        }
                    }
                }
            }

            _ => panic!("Unsupported import directive: {import_directive:#?}"),
        }
    }

    Ok(())
}

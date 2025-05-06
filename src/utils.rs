use std::path::{Path, PathBuf};
use convert_case::Case;
use crate::{errors::Error, project::{Project, ProjectKind}, sway};

use solang_parser::pt as solidity;

/// Returns a canonical path in the `path` location as a [`PathBuf`]
#[inline]
pub fn get_canonical_path<P: AsRef<Path>>(path: P, is_dir: bool, create_if_necessary: bool) -> std::io::Result<PathBuf> {
    let mut path_string = path.as_ref().to_string_lossy().to_string();

    while path_string.contains("\\\\") {
        path_string = path_string.replace("\\\\", "\\");
    }

    path_string = path_string.replace('\\', "/");

    while path_string.contains("//") {
        path_string = path_string.replace("//", "/");
    }

    if is_dir && !path_string.ends_with('/') {
        path_string = format!("{path_string}/");
    }

    let path = PathBuf::from(path_string);

    if create_if_necessary && !path.exists() {
        if is_dir {
            std::fs::create_dir_all(path.clone())?;
        } else {
            std::fs::File::create(path.clone())?;
        }
    }
    
    path.canonicalize()
}

pub fn generate_forc_project<P1: AsRef<Path>, P2: AsRef<Path>>(
    project: &mut Project,
    output_directory: P1,
    definition_name: Option<&String>,
    source_unit_path: P2,
) -> Result<(), Error> {
    let output_directory = get_canonical_path(output_directory, true, true)
        .map_err(|e| Error::Wrapped(Box::new(e)))?;

    for translated_definition in project.collect_translated_definitions(definition_name, source_unit_path) {
        let definition_snake_name = crate::translate::translate_naming_convention(translated_definition.name.as_str(), Case::Snake);
        let dependencies = translated_definition.dependencies.clone();
        
        let module: sway::Module = translated_definition.into();

        let project_path = get_canonical_path(output_directory.join(definition_snake_name.as_str()), true, true)
            .map_err(|e| Error::Wrapped(Box::new(e)))?;
        
        let src_dir_path = get_canonical_path(project_path.join("src"), true, true)
            .map_err(|e| Error::Wrapped(Box::new(e)))?;
        
        std::fs::write(
            src_dir_path.join("main.sw"),
            sway::TabbedDisplayer(&module).to_string(),
        )
        .map_err(|e| Error::Wrapped(Box::new(e)))?;
    
        std::fs::write(
            project_path.join(".gitignore"),
            "out\ntarget\nForc.lock\n",
        )
        .map_err(|e| Error::Wrapped(Box::new(e)))?;

        std::fs::write(
            project_path.join("Forc.toml"),
            format!(
                "[project]\n\
                authors = [\"\"]\n\
                entry = \"main.sw\"\n\
                license = \"Apache-2.0\"\n\
                name = \"{definition_snake_name}\"\n\
                \n\
                [dependencies]\n\
                {}\
                \n\
                ",
                dependencies.join("\n"),
            ),
        )
        .map_err(|e| Error::Wrapped(Box::new(e)))?;
    }

    Ok(())
}

/// Recursively search for .sol files in the given directory
pub fn collect_source_unit_paths(
    path: &Path,
    project_kind: &ProjectKind,
) -> std::io::Result<Vec<PathBuf>> {
    let mut source_unit_paths = vec![];

    if let ProjectKind::Hardhat | ProjectKind::Truffle = project_kind {
        // Skip the node_modules folder. Only translate things that are imported explicitly.
        if path.to_string_lossy().replace('\\', "/").contains("/node_modules/") {
            return Ok(vec![]);
        }
    }

    if !path.is_dir() {
        assert!(path.extension().unwrap() == "sol", "Only solidity files are supported.");
        
        if !path.exists() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("File not found: {}", path.to_string_lossy()),
            ));
        }

        source_unit_paths.push(get_canonical_path(path, false, false)?);
    } else {
        for entry in std::fs::read_dir(path)? {
            let entry = entry?;
            let path = entry.path();
    
            if path.is_dir() {
                source_unit_paths.extend(collect_source_unit_paths(&path, project_kind)?);
                continue;
            }
            
            if let Some(extension) = path.extension() {
                if extension == "sol" {
                    source_unit_paths.push(get_canonical_path(path, false, false)?);
                }
            }
        }
    }

    Ok(source_unit_paths)
}

pub fn create_usage_queue(
    project: &mut crate::project::Project,
    mut paths: Vec<PathBuf>,
) -> Result<Vec<PathBuf>, Error> {
    let mut results = vec![];

    while let Some(source_unit_path) = paths.pop() {
        project.parse_solidity_source_unit(&source_unit_path)?;

        let ast = project.solidity_source_units.borrow()
            .get(&source_unit_path).cloned()
            .unwrap();
        let import_paths = get_import_paths(project, &ast, &source_unit_path)?;

        for import_path in import_paths.iter() {
            if !paths.contains(import_path) && !results.iter().any(|(r, _)| r == import_path) {
                paths.push(import_path.clone());
            }
        }

        results.push((source_unit_path.clone(), import_paths));
    }

    results.sort_by(|a, b| a.1.len().cmp(&b.1.len()));

    let mut new_results: Vec<PathBuf> = vec![];

    while !results.is_empty() {
        let mut queue = vec![];

        let import_count = results[0].1.len();

        while !results.is_empty() && results[0].1.len() == import_count {
            let item = results[0].clone();
            results.remove(0);
            queue.push(item);
        }

        for item in queue {
            if !new_results.iter().any(|r| r == &item.0) {
                new_results.push(item.0.clone());
            }

            for item_import_path in item.1 {
                let mut depends = vec![];

                for result in results.iter() {
                    if result.0 == item_import_path {
                        depends.push(result.0.clone());
                    }
                }
                new_results.extend(depends);
            }
        }
    }

    Ok(new_results)
}

pub fn get_import_paths(
    project: &mut Project,
    ast: &solidity::SourceUnit,
    source_unit_path: &Path,
) -> Result<Vec<PathBuf>, Error> {
    let mut import_paths = Vec::new();
    if let Some(import_directives) = get_contract_imports(&ast) {
        for import_directive in &import_directives {
            let import_path = match import_directive {
                solang_parser::pt::Import::Plain(import_path, _) => import_path,
                solang_parser::pt::Import::GlobalSymbol(import_path, _, _) => import_path,
                solang_parser::pt::Import::Rename(import_path, _, _) => import_path,
            };

            let import_path = match import_path {
                solang_parser::pt::ImportPath::Filename(path) => {
                    std::path::PathBuf::from(path.to_string())
                }
                solang_parser::pt::ImportPath::Path(path) => {
                    std::path::PathBuf::from(path.to_string())
                }
            };

            // Clean the import path and remove quotes
            let mut import_path = import_path.to_str().unwrap().replace('\"', "");

            if let ProjectKind::Unknown = project.kind {
                // Join the import path with the source unit path
                import_path = source_unit_path
                    .parent()
                    .unwrap()
                    .join(import_path)
                    .to_str()
                    .unwrap()
                    .to_string();
            } else {
                
                // If we have detected a framework we need to resolve the path based on the remappings if found
                import_path = project.get_project_type_path(
                    source_unit_path.parent().unwrap(),
                    &import_path,
                )?
                .to_str()
                .unwrap()
                .to_string();
            }

            // Normalize the import path
            let import_path = std::fs::canonicalize(import_path).map_err(|e| Error::Wrapped(Box::new(e)))?;

            import_paths.push(import_path);
        }
    }
    Ok(import_paths)
}

/// Returns all the contract imports
pub fn get_contract_imports(source_unit: &solidity::SourceUnit) -> Option<Vec<solidity::Import>> {
    let imports: Vec<solidity::Import> = source_unit
        .0
        .iter()
        .filter_map(|part| match part {
            solidity::SourceUnitPart::ImportDirective(import) => Some(import),
            _ => None,
        })
        .cloned()
        .collect();

    (!imports.is_empty()).then_some(imports)
}
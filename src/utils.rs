use crate::{
    errors::Error,
    project::{Project, ProjectKind},
    sway,
    translate::*,
};
use convert_case::Case;
use solang_parser::pt as solidity;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

/// Returns a canonical path in the `path` location as a [`PathBuf`]
#[inline]
pub fn get_canonical_path<P: AsRef<Path>>(path: P, is_dir: bool, create_if_necessary: bool) -> Result<PathBuf, Error> {
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
            std::fs::create_dir_all(path.clone()).map_err(|e| Error::Wrapped(Box::new(e)))?;
        } else {
            std::fs::File::create(path.clone()).map_err(|e| Error::Wrapped(Box::new(e)))?;
        }
    }
    
    path.canonicalize().map_err(|e| Error::Wrapped(Box::new(e)))
}

pub fn generate_forc_project<P1: AsRef<Path>, P2: AsRef<Path>>(
    project: &mut Project,
    output_directory: P1,
    definition_name: Option<&String>,
    source_unit_path: P2,
) -> Result<(), Error> {
    let output_directory = get_canonical_path(output_directory, true, true)?;

    for translated_definition in project.collect_translated_definitions(definition_name, source_unit_path) {
        let definition_snake_name = translate_naming_convention(translated_definition.name.as_str(), Case::Snake);
        let dependencies = translated_definition.dependencies.clone();
        
        let module: sway::Module = translated_definition.into();

        let project_path = get_canonical_path(output_directory.join(definition_snake_name.as_str()), true, true)?;
        let src_dir_path = get_canonical_path(project_path.join("src"), true, true)?;
        
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
) -> Result<Vec<PathBuf>, Error> {
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
            return Err(Error::Wrapped(Box::new(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("File not found: {}", path.to_string_lossy()),
            ))));
        }

        source_unit_paths.push(get_canonical_path(path, false, false)?);
    } else {
        for entry in std::fs::read_dir(path).map_err(|e| Error::Wrapped(Box::new(e)))? {
            let entry = entry.map_err(|e| Error::Wrapped(Box::new(e)))?;
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
    project: &mut Project,
    paths: Vec<PathBuf>,
) -> Result<Vec<PathBuf>, Error> {
    // Build a mapping of file paths to import paths, and file paths to the number of times that that file is imported from another file
    let mut import_paths = HashMap::new();
    let mut import_counts = HashMap::new();

    fn collect_imports(
        project: &mut Project,
        path: &Path,
        import_paths: &mut HashMap<PathBuf, Vec<PathBuf>>,
        import_counts: &mut HashMap<PathBuf, i32>,
    ) {
        import_counts.entry(path.into()).or_insert(0);

        project.parse_solidity_source_unit(path).unwrap();
        let ast = project.solidity_source_units.borrow().get(path).cloned().unwrap();
        
        if !import_paths.contains_key(path) {
            let paths = get_import_paths(project, &ast, path).unwrap();

            for import_path in paths.iter() {
                collect_imports(project, import_path, import_paths, import_counts);
                *import_counts.entry(import_path.clone()).or_insert(0) += 1;
            }
            
            import_paths.insert(path.into(), paths);
        }
    }

    for path in paths.iter() {
        collect_imports(project, path, &mut import_paths, &mut import_counts);
    }

    // Start with all the files that have an import count of 0 (as in they are never imported by another file)
    // When visiting a file you visit each of its imports first, inserting them higher in the queue before inserting the current file
    // After visiting each import path, add the path of the current file
    fn queue_imports(
        project: &mut Project,
        import_paths: &HashMap<PathBuf, Vec<PathBuf>>,
        path: &Path,
        queue: &mut Vec<PathBuf>,
    ) {
        let current_import_paths =  import_paths.get(path).unwrap();

        for import_path in current_import_paths.iter() {
            queue_imports(project, import_paths, import_path, queue);
        }

        if !queue.iter().any(|p| p == path) {
            queue.push(path.into());
        }
    }

    let mut queue = vec![];

    for (path, _) in  import_counts.iter().filter(|&(_, x)| *x == 0) {
        queue_imports(project, &import_paths, path, &mut queue);
    }

    Ok(queue)
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

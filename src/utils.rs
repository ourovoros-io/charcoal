use crate::{
    error::Result,
    framework::{self, Framework},
};
use path_clean::PathClean;
use solang_parser::pt as solidity;
use std::path::{Path, PathBuf};

/// Recursively collect .sol file paths, excluding `node_modules`
pub fn collect_source_unit_paths(path: &Path) -> Result<Vec<PathBuf>> {
    let mut paths = Vec::new();

    if path.is_dir() {
        for entry in std::fs::read_dir(path)? {
            let entry = entry?;
            paths.extend(collect_source_unit_paths(&entry.path())?);
        }
    } else if path.extension().and_then(|ext| ext.to_str()) == Some("sol") {
        // Validate file existence and extension in one check
        if !path.exists() {
            return Err(format!("File not found: {}", path.display()).into());
        }
        paths.push(path.canonicalize()?);
    }

    Ok(paths)
}

pub fn create_usage_queue(
    project: &mut crate::project::Project,
    mut paths: Vec<PathBuf>,
    framework: Option<&Framework>,
) -> Result<Vec<PathBuf>> {
    let mut results = vec![];

    while let Some(source_unit_path) = paths.pop() {
        if !project
            .solidity_source_units
            .contains_key(&source_unit_path)
        {
            project.solidity_source_units.insert(
                source_unit_path.clone(),
                parse_ast_from_source_unit_path(&source_unit_path)?,
            );
        }
        let ast = project
            .solidity_source_units
            .get(&source_unit_path)
            .unwrap();
        let import_paths = get_import_paths(&ast, &source_unit_path, framework)?;

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

/// Returns a canonical path in the `path` location as a [`PathBuf`]
#[inline]
pub fn get_canonical_path<P: AsRef<Path>>(
    path: P,
    is_dir: bool,
    create_if_necessary: bool,
) -> Result<PathBuf> {
    let mut path = path.as_ref().to_path_buf();

    // Normalize the path using path-clean
    path = path.clean();

    if is_dir && !path.ends_with("/") {
        path.push(""); // Ensure trailing slash for directories
    }

    if create_if_necessary && !path.exists() {
        if is_dir {
            std::fs::create_dir_all(&path)?;
        } else {
            std::fs::File::create(&path)?;
        }
    }

    Ok(path.canonicalize()?)
}

/// Finds the project root folder by checking for specific config files
pub fn find_project_root_folder<P: AsRef<Path>>(path: P) -> Option<PathBuf> {
    let path = path.as_ref();

    if !path.exists() {
        return None;
    }

    static CONFIG_FILES: &[&str] = &[
        framework::Framework::FOUNDRY_CONFIG_FILE,
        framework::Framework::HARDHAT_CONFIG_FILE,
        framework::Framework::HARDHAT_TS_CONFIG_FILE,
        framework::Framework::BROWNIE_CONFIG_FILE,
        framework::Framework::TRUFFLE_CONFIG_FILE,
        framework::Framework::DAPP_CONFIG_FILE,
    ];

    for ancestor in path.ancestors() {
        if CONFIG_FILES
            .iter()
            .any(|&file| ancestor.join(file).is_file())
        {
            return Some(ancestor.to_path_buf());
        }
    }

    None
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

pub fn parse_ast_from_source_unit_path(source_unit_path: &Path) -> Result<solidity::SourceUnit> {
    let source_unit_string = std::fs::read_to_string(source_unit_path)?;
    let (source_unit, _) = solang_parser::parse(&source_unit_string, 0).unwrap();
    Ok(source_unit)
}

pub fn get_import_paths(
    ast: &solidity::SourceUnit,
    source_unit_path: &Path,
    framework: Option<&framework::Framework>,
) -> Result<Vec<PathBuf>> {
    let mut import_paths = Vec::new();
    if let Some(import_directives) = get_contract_imports(&ast) {
        for import_directive in &import_directives {
            let import_path = match import_directive {
                solang_parser::pt::Import::Plain(import_path, _) => import_path,
                solang_parser::pt::Import::GlobalSymbol(import_path, identifier, _) => import_path,
                solang_parser::pt::Import::Rename(import_path, identifiers, _) => import_path,
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

            if let Some(framework) = framework {
                // If we have detected a framework we need to resolve the path based on the remappings if found
                import_path = crate::framework::resolve_framework_path(
                    framework,
                    source_unit_path.parent().unwrap(),
                    &import_path,
                )?
                .to_str()
                .unwrap()
                .to_string();
            } else {
                // Join the import path with the source unit path
                import_path = source_unit_path
                    .parent()
                    .unwrap()
                    .join(import_path)
                    .to_str()
                    .unwrap()
                    .to_string();
            }

            // Normalize the import path
            let import_path = std::fs::canonicalize(import_path)?;

            import_paths.push(import_path);
        }
    }
    Ok(import_paths)
}

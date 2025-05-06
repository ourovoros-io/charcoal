use crate::{error::Result, framework};
use path_clean::PathClean;
use solang_parser::pt as solidity;
use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashMap},
    path::{Path, PathBuf},
};

/// Recursively collect .sol file paths, excluding node_modules
pub fn collect_source_unit_paths(path: &Path) -> Result<Vec<PathBuf>> {
    let mut paths = Vec::new();

    // Early return for node_modules
    if path.to_string_lossy().contains("node_modules") {
        return Ok(paths);
    }

    if path.is_dir() {
        // Use walkdir for more efficient directory traversal
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
pub fn get_contract_imports(
    source_unit_parts: Vec<solidity::SourceUnitPart>,
) -> Option<Vec<solidity::Import>> {
    let imports: Vec<solidity::Import> = source_unit_parts
        .into_iter()
        .filter_map(|part| match part {
            solidity::SourceUnitPart::ImportDirective(import) => Some(import),
            _ => None,
        })
        .collect();

    (!imports.is_empty()).then_some(imports)
}

pub fn parse_ast_from_source_unit_path(
    source_unit_path: &Path,
) -> Result<Vec<solidity::SourceUnitPart>> {
    let source_unit = std::fs::read_to_string(source_unit_path)?;
    let (source_unit_parts, _) = solang_parser::parse(&source_unit, 0).unwrap();
    Ok(source_unit_parts.0)
}

pub fn get_import_paths(
    ast: Vec<solidity::SourceUnitPart>,
    source_unit_path: &PathBuf,
    framework: Option<&framework::Framework>,
) -> Result<Vec<PathBuf>> {
    let mut import_paths = Vec::new();
    if let Some(import_directives) = get_contract_imports(ast) {
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
            let mut import_path = import_path.to_str().unwrap().replace("\"", "");

            if let Some(framework) = framework {
                // If we have detected a framework we need to resolve the path based on the remappings if found
                import_path = crate::framework::resolve_framework_path(framework, &source_unit_path.parent().unwrap(), &import_path)?.to_str().unwrap().to_string();
            } else {
                // Join the import path with the source unit path
                import_path = source_unit_path.parent().unwrap().join(import_path).to_str().unwrap().to_string();
            }

            // Normalize the import path
            let import_path = std::fs::canonicalize(import_path)?;

            import_paths.push(import_path);
        }
    }
    Ok(import_paths)
}

pub fn create_import_usage_queue(results: HashMap<PathBuf, Vec<PathBuf>>) -> Vec<PathBuf> {
    // Step 1: Count the usage of each contract
    let mut usage_count: HashMap<&PathBuf, usize> = HashMap::new();

    for imports in results.values() {
        for import in imports {
            *usage_count.entry(import).or_insert(0) += 1;
        }
    }

    // Step 2: Add all source unit paths with their usage count (default to 0 if not used)
    for path in results.keys() {
        usage_count.entry(path).or_insert(0);
    }

    // Step 3: Create a max-heap to sort by usage count
    let mut heap: BinaryHeap<(usize, Reverse<&PathBuf>)> = BinaryHeap::new();
    for (path, count) in usage_count {
        heap.push((count, Reverse(path)));
    }

    // Step 4: Extract the sorted contracts from the heap
    let mut sorted_queue = Vec::new();
    while let Some((_, Reverse(path))) = heap.pop() {
        sorted_queue.push(path.clone());
    }

    sorted_queue
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::{self, File};
    use std::os::unix::fs::symlink; // For symbolic link testing (Unix only)
    use tempfile::tempdir;

    #[test]
    fn test_find_project_root_folder_with_config_file() {
        let temp_dir = tempdir().unwrap();
        let root_dir = temp_dir.path().join("project_root");
        let nested_dir = root_dir.join("nested").join("deep");

        // Create directories and a config file
        fs::create_dir_all(&nested_dir).unwrap();
        File::create(root_dir.join("hardhat.config.js")).unwrap();

        // Test finding the root folder
        let result = find_project_root_folder(&nested_dir);
        assert_eq!(
            result.map(|p| p.canonicalize().unwrap()),
            Some(root_dir.canonicalize().unwrap())
        );
    }

    #[test]
    fn test_find_project_root_folder_with_multiple_config_files() {
        let temp_dir = tempdir().unwrap();
        let root_dir = temp_dir.path().join("project_root");
        let nested_dir = root_dir.join("nested").join("deep");

        // Create directories and multiple config files
        fs::create_dir_all(&nested_dir).unwrap();
        File::create(root_dir.join("foundry.toml")).unwrap();
        File::create(root_dir.join("hardhat.config.js")).unwrap();

        // Test finding the root folder
        let result = find_project_root_folder(&nested_dir);
        assert_eq!(
            result.map(|p| p.canonicalize().unwrap()),
            Some(root_dir.canonicalize().unwrap())
        );
    }

    #[test]
    fn test_find_project_root_folder_with_no_config_file() {
        let temp_dir = tempdir().unwrap();
        let root_dir = temp_dir.path().join("project_root");
        let nested_dir = root_dir.join("nested").join("deep");

        // Create directories without any config files
        fs::create_dir_all(&nested_dir).unwrap();

        // Test finding the root folder
        let result = find_project_root_folder(&nested_dir);
        assert_eq!(result, None);
    }

    #[test]
    fn test_find_project_root_folder_with_nonexistent_path() {
        let temp_dir = tempdir().unwrap();
        let nonexistent_path = temp_dir.path().join("nonexistent");

        // Test with a non-existent path
        let result = find_project_root_folder(&nonexistent_path);
        assert_eq!(result, None);
    }

    #[test]
    fn test_find_project_root_folder_with_nested_config_file() {
        let temp_dir = tempdir().unwrap();
        let root_dir = temp_dir.path().join("project_root");
        let nested_dir = root_dir.join("nested").join("deep");

        // Create directories and a config file in a nested directory
        fs::create_dir_all(&nested_dir).unwrap();
        File::create(nested_dir.join("hardhat.config.js")).unwrap();

        // Test finding the root folder
        let result = find_project_root_folder(&nested_dir);
        assert_eq!(
            result.map(|p| p.canonicalize().unwrap()),
            Some(nested_dir.canonicalize().unwrap())
        );
    }

    #[test]
    fn test_collect_source_unit_custom_paths() {
        let path = Path::new("./test_data/paths/contracts");
        let paths = collect_source_unit_paths(path).unwrap();
        assert_eq!(paths.len(), 4);
    }

    #[test]
    fn test_collect_source_unit_paths() {
        let temp_dir = tempdir().unwrap();
        let dir_path = temp_dir.path();

        // Create test files and directories
        let file1 = dir_path.join("file1.sol");
        let file2 = dir_path.join("file2.sol");
        let sub_dir = dir_path.join("subdir");
        let node_modules_dir = dir_path.join("node_modules");
        let file_in_subdir = sub_dir.join("file3.sol");

        File::create(&file1).unwrap();
        File::create(&file2).unwrap();
        fs::create_dir(&sub_dir).unwrap();
        File::create(&file_in_subdir).unwrap();
        fs::create_dir(&node_modules_dir).unwrap();

        // Test function
        let paths = collect_source_unit_paths(dir_path).unwrap();

        // Assert results
        assert_eq!(paths.len(), 3);
        assert!(paths.contains(&file1.canonicalize().unwrap()));
        assert!(paths.contains(&file2.canonicalize().unwrap()));
        assert!(paths.contains(&file_in_subdir.canonicalize().unwrap()));
    }

    #[test]
    fn test_get_canonical_path_file_creation() {
        let temp_dir = tempdir().unwrap();
        let file_path = temp_dir.path().join("test_file.txt");

        // Test file creation
        let canonical_path = get_canonical_path(&file_path, false, true).unwrap();
        assert!(file_path.exists());
        assert_eq!(canonical_path, file_path.canonicalize().unwrap());
    }

    #[test]
    fn test_get_canonical_path_directory_creation() {
        let temp_dir = tempdir().unwrap();
        let dir_path = temp_dir.path().join("test_dir");

        // Test directory creation
        let canonical_path = get_canonical_path(&dir_path, true, true).unwrap();
        assert!(dir_path.exists());
        assert!(dir_path.is_dir());
        assert_eq!(canonical_path, dir_path.canonicalize().unwrap());
    }

    #[test]
    fn test_get_canonical_path_no_creation() {
        let temp_dir = tempdir().unwrap();
        let file_path = temp_dir.path().join("nonexistent_file.txt");

        // Test without creation
        let result = get_canonical_path(&file_path, false, false);
        assert!(result.is_err());
    }

    #[test]
    fn test_get_canonical_path_with_relative_paths() {
        let temp_dir = tempdir().unwrap();
        let base_dir = temp_dir.path().join("base");
        let nested_dir = base_dir.join("nested").join("deep");
        let file_in_base_dir = base_dir.join("file.txt");
        let file_in_nested_dir = nested_dir.join("file.txt");

        // Create directories and files
        fs::create_dir_all(&nested_dir).unwrap();
        File::create(&file_in_base_dir).unwrap();
        File::create(&file_in_nested_dir).unwrap();

        // Test resolving `../../file.txt` to go up two levels
        let relative_path = nested_dir.join("../../file.txt");
        let canonical_path = get_canonical_path(&relative_path, false, false).unwrap();
        assert_eq!(canonical_path, file_in_base_dir.canonicalize().unwrap());

        // Test resolving `./file.txt` in the same directory
        let relative_path_with_dot = nested_dir.join("./file.txt");
        let canonical_path_with_dot =
            get_canonical_path(&relative_path_with_dot, false, false).unwrap();
        assert_eq!(
            canonical_path_with_dot,
            file_in_nested_dir.canonicalize().unwrap()
        );
    }

    #[test]
    fn test_get_canonical_path_with_complex_relative_paths() {
        let temp_dir = tempdir().unwrap();
        let base_dir = temp_dir.path().join("base");
        let nested_dir = base_dir.join("nested").join("deep");
        let file_in_nested_dir = base_dir.join("nested").join("file.txt");

        // Create directories and file
        fs::create_dir_all(&nested_dir).unwrap();
        File::create(&file_in_nested_dir).unwrap();

        // Test resolving a complex relative path
        let complex_relative_path = nested_dir.join("../../nested/./deep/../file.txt");
        let canonical_path = get_canonical_path(&complex_relative_path, false, false).unwrap();
        assert_eq!(canonical_path, file_in_nested_dir.canonicalize().unwrap());
    }

    #[test]
    fn test_collect_source_unit_paths_with_empty_directory() {
        let temp_dir = tempdir().unwrap();
        let dir_path = temp_dir.path();

        // Test with an empty directory
        let paths = collect_source_unit_paths(dir_path).unwrap();
        assert!(paths.is_empty());
    }

    #[test]
    fn test_collect_source_unit_paths_with_invalid_path() {
        let temp_dir = tempdir().unwrap();
        let invalid_path = temp_dir.path().join("nonexistent");

        // Test with a non-existent path
        let result = collect_source_unit_paths(&invalid_path);
        assert!(result.is_ok());
        assert!(result.unwrap().is_empty());
    }

    #[test]
    fn test_get_canonical_path_with_symlink() {
        let temp_dir = tempdir().unwrap();
        let target_dir = temp_dir.path().join("target");
        let symlink_path = temp_dir.path().join("symlink");

        // Create target directory and symlink
        fs::create_dir(&target_dir).unwrap();
        symlink(&target_dir, &symlink_path).unwrap();

        // Test resolving the symlink
        let canonical_path = get_canonical_path(&symlink_path, true, false).unwrap();
        assert_eq!(canonical_path, target_dir.canonicalize().unwrap());
    }

    #[test]
    fn test_get_canonical_path_with_deeply_nested_directories() {
        let temp_dir = tempdir().unwrap();
        let mut nested_dir = temp_dir.path().to_path_buf();

        // Create a deeply nested directory structure
        for i in 0..50 {
            nested_dir = nested_dir.join(format!("level{}", i));
        }
        fs::create_dir_all(&nested_dir).unwrap();

        // Test resolving the deeply nested directory
        let canonical_path = get_canonical_path(&nested_dir, true, false).unwrap();
        assert_eq!(canonical_path, nested_dir.canonicalize().unwrap());
    }

    #[test]
    fn test_get_canonical_path_with_invalid_path() {
        let temp_dir = tempdir().unwrap();
        let invalid_path = temp_dir.path().join("nonexistent");

        // Test resolving a non-existent path without creation
        let result = get_canonical_path(&invalid_path, false, false);
        assert!(result.is_err());
    }

    #[test]
    fn test_get_canonical_path_with_file_creation_in_nested_directories() {
        let temp_dir = tempdir().unwrap();
        let nested_file_path = temp_dir.path().join("nested").join("deep").join("file.txt");

        // Ensure parent directories are created before testing
        fs::create_dir_all(nested_file_path.parent().unwrap()).unwrap();

        // Test creating a file in a nested directory structure
        let canonical_path = get_canonical_path(&nested_file_path, false, true).unwrap();
        assert!(nested_file_path.exists());
        assert_eq!(canonical_path, nested_file_path.canonicalize().unwrap());
    }

    #[test]
    fn test_get_canonical_path_with_directory_creation_in_nested_directories() {
        let temp_dir = tempdir().unwrap();
        let nested_dir_path = temp_dir.path().join("nested").join("deep").join("dir");

        // Test creating a directory in a nested directory structure
        let canonical_path = get_canonical_path(&nested_dir_path, true, true).unwrap();
        assert!(nested_dir_path.exists());
        assert!(nested_dir_path.is_dir());
        assert_eq!(canonical_path, nested_dir_path.canonicalize().unwrap());
    }

    #[test]
    fn test_get_canonical_path_with_trailing_slash() {
        let temp_dir = tempdir().unwrap();
        let dir_path = temp_dir.path().join("test_dir");

        // Test directory creation with a trailing slash
        let canonical_path =
            get_canonical_path(&format!("{}/", dir_path.display()), true, true).unwrap();
        assert!(dir_path.exists());
        assert!(dir_path.is_dir());
        assert_eq!(canonical_path, dir_path.canonicalize().unwrap());
    }

    #[test]
    fn test_get_canonical_path_with_dot_and_double_dot() {
        let temp_dir = tempdir().unwrap();
        let base_dir = temp_dir.path().join("base");
        let nested_dir = base_dir.join("nested");
        let file_in_nested_dir = nested_dir.join("file.txt");

        // Create directories and file
        fs::create_dir_all(&nested_dir).unwrap();
        File::create(&file_in_nested_dir).unwrap();

        // Test resolving `../nested/./file.txt`
        let complex_relative_path = base_dir.join("../base/nested/./file.txt");
        let canonical_path = get_canonical_path(&complex_relative_path, false, false).unwrap();
        assert_eq!(canonical_path, file_in_nested_dir.canonicalize().unwrap());
    }
}

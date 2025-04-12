use std::path::{Path, PathBuf};
use convert_case::Case;
use crate::{errors::Error, project::{Project, ProjectKind}, sway};

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

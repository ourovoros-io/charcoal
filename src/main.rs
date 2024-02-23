pub mod errors;
pub mod project;
pub mod sway;
pub mod translate;

use convert_case::{Case, Casing};
use errors::Error;
use project::Project;
use std::path::{Path, PathBuf};
use structopt::{clap::AppSettings, StructOpt};

#[inline]
pub fn translate_naming_convention(name: &str, case: Case) -> String {
    if name == "_" {
        return "_".into();
    }

    let prefix = name.chars().take_while(|c| *c == '_').collect::<String>();
    let postfix = name.chars().rev().take_while(|c| *c == '_').collect::<String>();
    format!("{prefix}{}{postfix}", name.to_case(case))
}

#[inline]
pub fn get_canonical_path<P: AsRef<Path>>(path: P, is_dir: bool, create_if_necessary: bool) -> std::io::Result<PathBuf> {
    let mut path_string = path.as_ref().to_string_lossy().to_string();

    while path_string.contains("\\\\") {
        path_string = path_string.replace("\\\\", "\\");
    }

    path_string = path_string.replace("\\", "/");

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

#[derive(Default, StructOpt)]
#[structopt(global_settings = &[AppSettings::ColoredHelp, AppSettings::ArgRequiredElseHelp])]
struct Options {
    /// The name of the specific definition to translate. (Optional; Leave unused for all)
    #[structopt(long, short)]
    definition_name: Option<String>,

    /// The Solidity target file or folder to translate.
    #[structopt(long, short)]
    target: PathBuf,

    /// The path to save the translated Forc project to. (Optional; Must be a directory)
    #[structopt(long, short)]
    output_directory: Option<PathBuf>,
}

fn main() {
    if let Err(e) = translate_project() {
        eprintln!("{e}");
    }
}

fn translate_project() -> Result<(), Error> {
    let mut options = Options::from_args_safe()
        .map_err(|e| Error::Wrapped(Box::new(e)))?;

    // If an output directory was supplied, canonicalize it
    if let Some(output_directory) = options.output_directory.as_mut() {
        *output_directory = get_canonical_path(output_directory.clone(), true, true)
            .map_err(|e| Error::Wrapped(Box::new(e)))?;
    }

    let mut project = Project::default();

    let source_unit_paths = collect_source_unit_paths(&options.target)
        .map_err(|e| Error::Wrapped(Box::new(e)))?;
    
    for source_unit_path in &source_unit_paths {
        project.translate(options.definition_name.as_ref(), source_unit_path)?;

        match options.output_directory.as_ref() {
            Some(output_directory) => generate_forc_project(&mut project, output_directory, options.definition_name.as_ref(), source_unit_path)?,

            None => {
                for translated_definition in project.collect_translated_definitions(options.definition_name.as_ref(), source_unit_path) {
                    println!("// Translated from {}", translated_definition.path.to_string_lossy());
                    
                    let module: sway::Module = translated_definition.into();
                    println!("{}", sway::TabbedDisplayer(&module));
                }
            }
        }
    }

    Ok(())
}

fn generate_forc_project<P1: AsRef<Path>, P2: AsRef<Path>>(
    project: &mut Project,
    output_directory: P1,
    definition_name: Option<&String>,
    source_unit_path: P2,
) -> Result<(), Error> {
    let output_directory = get_canonical_path(output_directory, true, true)
        .map_err(|e| Error::Wrapped(Box::new(e)))?;

    for translated_definition in project.collect_translated_definitions(definition_name, source_unit_path) {
        let definition_snake_name = translate_naming_convention(translated_definition.name.as_str(), Case::Snake);
        
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
               \n\
                "
            ),
        )
        .map_err(|e| Error::Wrapped(Box::new(e)))?;
    }

    Ok(())
}

/// Recursively search for .sol files in the given directory
fn collect_source_unit_paths(dir: &Path) -> std::io::Result<Vec<PathBuf>> {
    if !dir.is_dir() {
        if dir.extension().unwrap() != "sol" {
            panic!("Only solidity files are supported.");
        }
        
        if !dir.exists() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("File not found: {}", dir.to_string_lossy()),
            ));
        }

        return Ok(vec![
            get_canonical_path(dir, false, false)?,
        ]);
    }

    let mut source_unit_paths = vec![];

    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.is_dir() {
            source_unit_paths.extend(collect_source_unit_paths(&path)?);
            continue;
        }
        
        if let Some(extension) = path.extension() {
            if extension == "sol" {
                source_unit_paths.push(get_canonical_path(path, false, false)?);
            }
        }
    }

    Ok(source_unit_paths)
}

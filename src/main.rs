pub mod errors;
pub mod project;
pub mod sway;
pub mod translate;

use errors::Error;
use project::Project;
use std::path::{Path, PathBuf};
use structopt::{clap::AppSettings, StructOpt};

#[derive(Default, StructOpt)]
#[structopt(global_settings = &[AppSettings::ColoredHelp, AppSettings::ArgRequiredElseHelp])]
struct Options {
    /// The name of the specific definition to translate. (Optional; Leave unused for all)
    #[structopt(long)]
    definition_name: Option<String>,

    /// The Solidity target file or folder to translate.
    #[structopt(long)]
    target: PathBuf,
}

fn main() {
    if let Err(e) = translate_project() {
        eprintln!("{e}");
    }
}

fn translate_project() -> Result<(), Error> {
    let options = Options::from_args_safe()
        .map_err(|e| Error::Wrapped(Box::new(e)))?;

    let mut project = Project::default();

    let contracts = find_sol_files_in_target(&options.target).unwrap();
    
    for contract_file in &contracts {
        project.translate(options.definition_name.as_ref(), contract_file)?;

        for translated_definition in project.get_translated_definitions(options.definition_name.as_ref(), contract_file) {
            println!("// Translated from {}", translated_definition.path.to_string_lossy());
            
            let module: sway::Module = translated_definition.into();
            println!("{}", sway::TabbedDisplayer(&module));
        }
    }

    Ok(())
}

/// Recursively search for .sol files in the given directory
fn find_sol_files_in_target(dir: &Path) -> std::io::Result<Vec<PathBuf>> {
    let mut sol_files = Vec::new();
    if dir.is_dir() {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() {
                // Recursively search subdirectories
                let mut sub_sol_files = find_sol_files_in_target(&path)?;
                sol_files.append(&mut sub_sol_files);
            } else {
                // Check if the file has a .sol extension
                if let Some(extension) = path.extension() {
                    if extension == "sol" {
                        if !path.exists() {
                            return Err(std::io::Error::new(
                                std::io::ErrorKind::NotFound,
                                format!("File not found: {}", path.to_string_lossy()),
                            ));
                        }
                        sol_files.push(std::fs::canonicalize(path)?);
                    }
                }
            }
        }
    } else {
        if dir.extension().unwrap() == "sol" {
            if !dir.exists() {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("File not found: {}", dir.to_string_lossy()),
                ));
            }
            sol_files.push(std::fs::canonicalize(dir)?);
        } else {
            panic!("Only solidity files are supported.")
        }
    }
    Ok(sol_files)
}

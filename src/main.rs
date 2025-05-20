#![warn(clippy::all, clippy::pedantic)]

use clap::Parser;
use error::Error;
use project::{Project, ProjectKind};

mod cli;
mod error;
mod project;
mod utils;
mod sway;
mod translate;

fn main() {
    if let Err(e) = translate_project() {
        eprintln!("{e}");
    }
}

fn translate_project() -> Result<(), Error> {
    let mut options = cli::Args::parse();

    // If an output directory was supplied, canonicalize it
    if let Some(output_directory) = options.output_directory.as_mut() {
        *output_directory = utils::get_canonical_path(output_directory.clone(), true, true).map_err(|e| Error::Wrapped(e.into()))?;
    }

    let mut project = Project::default();
    
    if let Some(root_folder) = options.root_folder {
        project.detect_project_type(&root_folder)?;
        project.root_folder = Some(std::fs::canonicalize(root_folder.clone()).map_err(|e| Error::Wrapped(e.into()))?);
    } else {
        if let Some(root_path) = project::find_project_root_folder(options.target.as_path()) {
            project.detect_project_type(&root_path)?;
            project.root_folder = Some(root_path);
        } else {
            project.kind = ProjectKind::Unknown;
        }
    }

    
    
    let source_unit_paths = utils::collect_source_unit_paths(&options.target, &project.kind)?;
    let usage_queue = utils::create_usage_queue(&mut project, source_unit_paths)?;

    for source_unit_path in &usage_queue {
        project.translate(source_unit_path)?;

        match options.output_directory.as_ref() {
            Some(output_directory) => {
                utils::generate_forc_project(&mut project, output_directory, source_unit_path)?;
            }

            None => {
                println!("{:#?}", project.translated_modules);
                // for translated_definition in project.collect_translated_definitions(options.definition_name.as_ref(), source_unit_path) {
                //     println!("// Translated from {}", translated_definition.path.to_string_lossy());
                    
                //     let module: sway::Module = translated_definition.into();
                //     println!("{}", sway::TabbedDisplayer(&module));
                // }
            }
        }
    }

    Ok(())
}

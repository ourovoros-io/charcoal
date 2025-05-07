#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::single_match)]

pub mod errors;
pub mod project;
pub mod sway;
pub mod translate;
pub mod utils;
pub mod cli;

use cli::Options;
use errors::Error;
use project::{Project, ProjectKind};
use structopt::StructOpt;
use utils::{collect_source_unit_paths, generate_forc_project, get_canonical_path};

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
    
    if let Some(root_path) = project::find_project_root_folder(options.target.as_path()) {
        project.detect_project_type(root_path)?;
    } else {
        project.kind = ProjectKind::Unknown;
    }
    
    let source_unit_paths = collect_source_unit_paths(&options.target, &project.kind)
        .map_err(|e| Error::Wrapped(Box::new(e)))?;

    let usage_queue = utils::create_usage_queue(&mut project, source_unit_paths)?;

    for source_unit_path in &usage_queue {
        project.translate(options.definition_name.as_ref(), source_unit_path)?;

        match options.output_directory.as_ref() {
            Some(output_directory) => {
                generate_forc_project(&mut project, output_directory, options.definition_name.as_ref(), source_unit_path)?;
            }

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

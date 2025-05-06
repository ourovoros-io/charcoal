#![warn(clippy::all, clippy::pedantic)]

use clap::Parser;

mod cli;
mod error;
mod framework;
mod project;
mod utils;

fn main() {
    if let Err(e) = translate_project() {
        eprintln!("{e}");
    }
}

fn translate_project() -> error::Result<()> {
    let mut args = cli::Args::parse();

    if !args.target.exists() {
        return Err("Target file or directory does not exist".into());
    }

    if let Some(output_directory) = args.output_directory.as_mut() {
        *output_directory = utils::get_canonical_path(output_directory.clone(), true, true)?;
    }

    let mut project = project::Project::default();

    // Check if we can determine the project root folder
    // so we can use it to detect if we have a framework present with potentially remappings
    let framework = if let Some(root_folder) = utils::find_project_root_folder(&args.target) {
        Some(framework::detect_framework(&root_folder)?)
    } else {
        None
    };

    let paths = utils::collect_source_unit_paths(&args.target)?;

    let usage_queue = utils::create_usage_queue(&mut project, paths, framework.as_ref())?;

    println!("Usage queue: {usage_queue:#?}");

    Ok(())
}

#![warn(clippy::all, clippy::pedantic)]

use clap::Parser;

mod cli;
mod error;
mod framework;
mod utils;

fn main() -> error::Result<()> {
    translate_project()?;
    Ok(())
}

fn translate_project() -> error::Result<()> {
    let mut args = cli::Args::parse();

    if !args.target.exists() {
        return Err("Target file or directory does not exist".into());
    }

    if let Some(output_directory) = args.output_directory.as_mut() {
        *output_directory = utils::get_canonical_path(output_directory.clone(), true, true)?;
    }

    // Check if we can determine the project root folder
    // so we can use it to detect if we have a framework present with potentially remappings
    let framework = if let Some(root_folder) = utils::find_project_root_folder(&args.target) {
        Some(framework::detect_framework(&root_folder)?)
    } else {
        None
    };

    let source_unit_paths = utils::collect_source_unit_paths(&args.target)?;

    let mut results = std::collections::HashMap::new();

    for source_unit_path in &source_unit_paths {
        let ast = utils::parse_ast_from_source_unit_path(source_unit_path)?;
        let import_paths = utils::get_import_paths(ast, source_unit_path, framework.as_ref())?;
        results.insert(source_unit_path.clone(), import_paths);
    }

    println!("Results: {:#?}", results);

    // Calculate the usage queue for the imports
    let usage_queue = utils::create_import_usage_queue(&results);
    println!("Usage Queue: {:#?}", usage_queue);

    Ok(())
}

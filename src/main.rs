#![warn(clippy::all, clippy::pedantic)]

use clap::Parser;
use error::Error;
use project::Project;

pub mod cli;
pub mod error;
pub mod project;
pub mod sway;
pub mod translate;

fn main() {
    if let Err(e) = translate_project() {
        eprintln!("{e}");
    }
}

fn translate_project() -> Result<(), Error> {
    let mut options = cli::Args::parse();

    // If an output directory was supplied, canonicalize it
    if let Some(output_directory) = options.output_directory.as_mut() {
        *output_directory = wrapped_err!(output_directory.canonicalize())?;
    }

    Project::new(&options)?.translate()
}

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

    options.canonicalize()?;

    Project::new(&options)?.translate()
}

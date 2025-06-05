use clap::Parser;

use crate::{error::Error, wrapped_err};

#[derive(Parser, Debug, Default, Clone)]
/// Charcoal is a Solidity to Sway translator.
pub struct Args {
    #[arg(short, long)]
    /// The input directory for the project to be translated. The input must be a valid project root.
    pub input: std::path::PathBuf,

    #[arg(short, long)]
    /// The output folder to store the translated contracts.
    pub output_directory: Option<std::path::PathBuf>,

    #[arg(short, long)]
    /// The name of the generated Sway project.
    pub name: Option<String>,
}

impl Args {
    pub fn new() -> Result<Self, Error> {
        let mut result = Self::parse();

        if !result.input.is_dir() {
            panic!("Input should be a directory");
        }

        result.input = wrapped_err!(result.input.canonicalize())?;

        if let Some(output_directory) = result.output_directory.as_mut() {
            if !output_directory.exists() {
                wrapped_err!(std::fs::create_dir_all(&output_directory))?;
            }
            *output_directory = wrapped_err!(output_directory.canonicalize())?;
        }

        Ok(result)
    }
}

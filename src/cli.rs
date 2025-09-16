use crate::{error::Error, wrapped_err};
use clap::Parser;
use convert_case::{Case, Casing};
use std::path::PathBuf;

#[derive(Parser, Debug, Default, Clone)]
/// Charcoal is a Solidity to Sway translator.
pub struct Args {
    #[arg(short, long)]
    /// The input directory for the project to be translated. The input must be a valid project root.
    pub input: PathBuf,

    #[arg(short, long)]
    /// The output folder to store the translated contracts.
    pub output_directory: Option<PathBuf>,

    #[arg(short, long)]
    /// The name of the generated Sway project.
    pub name: Option<String>,
}

impl Args {
    pub fn canonicalize(mut self) -> Result<Self, Error> {
        // Validate input directory
        if !self.input.is_dir() {
            return Err(Error::InvalidInput(format!(
                "Input is not a directory: {}",
                self.input.display()
            )));
        }

        self.input = wrapped_err!(self.input.canonicalize())?;

        // Handle output directory if provided
        if let Some(ref mut output_directory) = self.output_directory {
            // Check if name is provided when output directory is specified
            let Some(name) = self.name.as_mut() else {
                return Err(Error::InvalidInput("No output project name provided".to_string()));
            };

            *name = name.to_case(Case::Snake);

            // Create directory if it doesn't exist
            if !output_directory.exists() {
                wrapped_err!(std::fs::create_dir_all(&output_directory))?;
            }

            *output_directory = wrapped_err!(output_directory.canonicalize())?;
        }

        Ok(self)
    }
}

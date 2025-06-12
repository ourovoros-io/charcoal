use crate::{error::Error, wrapped_err};
use clap::Parser;

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
            return Err(Error::Wrapped(Box::new(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                format!(
                    "Input is not a directory: {}",
                    result.input.to_string_lossy()
                ),
            ))));
        }

        result.input = wrapped_err!(result.input.canonicalize())?;

        if let Some(output_directory) = result.output_directory.as_mut() {
            if result.name.is_none() {
                return Err(Error::Wrapped(Box::new(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    "No output project name provided",
                ))));
            }

            if !output_directory.exists() {
                wrapped_err!(std::fs::create_dir_all(&output_directory))?;
            }

            *output_directory = wrapped_err!(output_directory.canonicalize())?;
        }

        Ok(result)
    }
}

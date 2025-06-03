use clap::Parser;

use crate::{error::Error, wrapped_err};

#[derive(Parser, Debug, Default, Clone)]
/// Charcoal is a Solidity to Sway translator.
pub struct Args {
    #[arg(short, long)]
    /// The contract file or folder of contracts to translate.
    pub input: std::path::PathBuf,

    #[arg(short, long)]
    /// The output folder to store the translated contracts.
    pub output_directory: Option<std::path::PathBuf>,

    #[arg(short, long)]
    /// The name of the generated Sway project.
    pub name: Option<String>,
}

impl Args {
    pub fn canonicalize(&mut self) -> Result<(), Error> {
        if !self.input.is_dir() {
            panic!("Input should be a directory");
        }

        self.input = wrapped_err!(self.input.canonicalize())?;

        if let Some(output_directory) = self.output_directory.as_mut() {
            if !output_directory.exists() {
                wrapped_err!(std::fs::create_dir_all(&output_directory))?;
            }
            *output_directory = wrapped_err!(output_directory.canonicalize())?;
        }

        Ok(())
    }
}

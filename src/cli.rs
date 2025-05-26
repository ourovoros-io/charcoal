use clap::Parser;

use crate::{error::Error, wrapped_err};

#[derive(Parser, Debug)]
/// Charcoal is a Solidity to Sway translator.
pub struct Args {
    #[arg(short, long)]
    /// The contract file or folder of contracts to translate.
    pub target: std::path::PathBuf,

    #[arg(short, long)]
    /// The output folder to store the translated contracts.
    pub output_directory: Option<std::path::PathBuf>,

    #[arg(short, long)]
    /// The project root folder
    pub root_folder: Option<std::path::PathBuf>,
}

impl Args {
    pub fn canonicalize(&mut self) -> Result<(), Error> {
        if let Some(output_directory) = &mut self.output_directory {
            *output_directory = wrapped_err!(output_directory.canonicalize())?;
        }
        if let Some(root_folder) = &mut self.root_folder {
            *root_folder = wrapped_err!(root_folder.canonicalize())?;
        }
        Ok(())
    }
}
use clap::Parser;

#[derive(Parser, Debug)]
/// Charcoal is a Solidity to Sway translator.
pub struct Args {
    #[arg(short, long)]
    /// The contract file or folder of contracts to translate.
    pub target: std::path::PathBuf,

    #[arg(short, long)]
    /// The output folder to store the translated contracts.
    pub output_directory: Option<std::path::PathBuf>,
}

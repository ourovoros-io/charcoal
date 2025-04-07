
use std::path::PathBuf;
use structopt::{clap::AppSettings, StructOpt};

#[derive(Default, StructOpt)]
#[structopt(global_settings = &[AppSettings::ColoredHelp, AppSettings::ArgRequiredElseHelp])]
pub struct Options {
    /// The name of the specific definition to translate. (Optional; Leave unused for all)
    #[structopt(long, short)]
    pub definition_name: Option<String>,

    /// The Solidity target file or folder to translate.
    #[structopt(long, short)]
    pub target: PathBuf,

    /// The path to save the translated Forc project to. (Optional; Must be a directory)
    #[structopt(long, short)]
    pub output_directory: Option<PathBuf>,
}
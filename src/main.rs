pub mod errors;
pub mod project;
pub mod sway;
pub mod translate;

use errors::Error;
use project::Project;
use std::path::PathBuf;
use structopt::{clap::AppSettings, StructOpt};

#[derive(Default, StructOpt)]
#[structopt(global_settings = &[AppSettings::ColoredHelp, AppSettings::ArgRequiredElseHelp])]
pub struct Options {
    #[structopt(long)]
    pub definition_name: Option<String>,

    #[structopt(long)]
    pub contract_files: Vec<PathBuf>,
}

fn main() {
    if let Err(e) = translate_project() {
        eprintln!("{e}");
    }
}

fn translate_project() -> Result<(), Error> {
    let options = Options::from_args_safe()
        .map_err(|e| Error::Wrapped(Box::new(e)))?;

    let mut project = Project::default();
    
    for contract_file in options.contract_files.iter() {
        project.translate(options.definition_name.as_ref(), contract_file)?;

        for translated_definition in project.get_translated_definitions(options.definition_name.as_ref(), contract_file) {
            println!("// Translated from {}", translated_definition.path.to_string_lossy());
            
            let module: sway::Module = translated_definition.into();
            println!("{}", sway::TabbedDisplayer(&module));
        }
    }

    Ok(())
}

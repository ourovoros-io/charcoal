pub mod cli;
pub mod error;
pub mod ir;
pub mod project;
pub mod sway;
pub mod translate;
pub mod utils;

#[cfg(test)]
mod tests;

fn main() {
    if let Err(e) = translate_project() {
        eprintln!("ERROR: {e}");
        std::process::exit(1);
    }
}

fn translate_project() -> Result<(), error::Error> {
    use clap::Parser;
    let options = cli::Args::parse().canonicalize()?;
    let framework = project::Framework::from_path(&options.input)?;
    project::Project::new(options, framework)?.translate()
}

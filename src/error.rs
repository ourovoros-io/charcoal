use solang_parser::diagnostics::Diagnostic;
use std::fmt;
use std::path::{Path, PathBuf};

#[macro_export]
macro_rules! wrapped_err {
    ($e: expr) => {
        $e.map_err(|e| $crate::error::Error::Wrapped(Box::new(e)))
    };
}

#[derive(Debug)]
pub enum Error {
    Wrapped(Box<dyn std::error::Error>),
    MissingContractFile,
    LineNotFound(PathBuf, usize),
    SolangDiagnostics(PathBuf, Vec<(usize, usize)>, Vec<Diagnostic>),
    IneffectualStatement(PathBuf, String),
    UnknownFramework,
    InvalidInput(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Wrapped(e) => write!(f, "{e}"),
            Error::MissingContractFile => f.write_str("error: Contract file not specified"),
            Error::LineNotFound(path, offset) => write!(
                f,
                "error: Offset {offset} not found in file: \"{}\"",
                path.display()
            ),
            Error::SolangDiagnostics(path, line_ranges, diagnostics) => {
                Self::format_diagnostics(f, path, line_ranges, diagnostics)
            }
            Error::IneffectualStatement(path, statement) => write!(
                f,
                "error: Ineffectual statement in file: \"{}\" - {statement}",
                path.display()
            ),
            Error::UnknownFramework => {
                f.write_str("Could not detect a supported Solidity project kind.")
            }
            Error::InvalidInput(msg) => write!(f, "error: {msg}"),
        }
    }
}

impl Error {
    fn format_diagnostics(
        f: &mut fmt::Formatter<'_>,
        path: &Path,
        line_ranges: &[(usize, usize)],
        diagnostics: &[Diagnostic],
    ) -> fmt::Result {
        let loc_offset_to_line = |offset: usize| -> usize {
            line_ranges
                .iter()
                .position(|(start, end)| offset >= *start && offset < *end)
                .map_or(0, |i| i + 1)
        };

        for (i, diagnostic) in diagnostics.iter().enumerate() {
            writeln!(
                f,
                "{} at {}:{}:",
                diagnostic.level,
                path.display(),
                loc_offset_to_line(diagnostic.loc.start()),
            )?;

            write!(f, "\t{}", diagnostic.message)?;

            if i < diagnostics.len() - 1 {
                writeln!(f)?;
            }
        }

        Ok(())
    }
}

impl std::error::Error for Error {}

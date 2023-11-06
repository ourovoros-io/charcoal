use std::path::PathBuf;
use solang_parser::diagnostics::Diagnostic;

pub enum Error {
    Wrapped(Box<dyn std::error::Error>),
    MissingContractFile,
    LineNotFound(PathBuf, usize),
    SolangDiagnostics(PathBuf, Vec<(usize, usize)>, Vec<Diagnostic>),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Wrapped(e) => {
                write!(f, "{e}")
            }

            Error::MissingContractFile => {
                write!(f, "error: Contract file not specified")
            }

            Error::LineNotFound(path, offset) => {
                write!(f, "error: Offset {offset} not found in file: \"{}\"", path.to_string_lossy())
            }
            
            Error::SolangDiagnostics(path, line_ranges, diagnostics) => {
                let loc_offset_to_line = |offset: usize| -> usize {
                    for (i, line_range) in line_ranges.iter().enumerate() {
                        if offset >= line_range.0 && offset < line_range.1 {
                            return i + 1;
                        }
                    }

                    0
                };

                for (i, diagnostic) in diagnostics.iter().enumerate() {
                    writeln!(
                        f,
                        "{} at {}:{}:",
                        diagnostic.level,
                        path.to_string_lossy(),
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
    }
}

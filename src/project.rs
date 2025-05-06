use std::{collections::HashMap, path::PathBuf};

use solang_parser::pt as solidity;

#[derive(Default)]
pub struct Project {
    pub line_ranges: HashMap<PathBuf, Vec<(usize, usize)>>,
    pub solidity_source_units: HashMap<PathBuf, solidity::SourceUnit>,
}

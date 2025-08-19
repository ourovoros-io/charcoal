use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use crate::{error::Error, utils::find_in_toml_value, wrapped_err};

/// Represents the type of project as [Framework] default is [Framework::Unknown]
#[derive(Clone, Debug)]
pub enum Framework {
    Foundry {
        src_path: Option<PathBuf>,
        remappings: HashMap<String, String>,
    },
    Hardhat,
}

impl Framework {
    pub const FOUNDRY_CONFIG_FILE: &'static str = "foundry.toml";
    pub const HARDHAT_CONFIG_FILE: &'static str = "hardhat.config.js";
    pub const HARDHAT_CONFIG_FILE_TS: &'static str = "hardhat.config.ts";

    /// Attempt to get the [Framework] from the supplied path.
    pub fn from_path<P: AsRef<Path>>(path: P) -> Result<Framework, Error> {
        let path = path.as_ref();
        let foundry_config_path = path.join(Framework::FOUNDRY_CONFIG_FILE);

        if foundry_config_path.exists() {
            let foundry_toml_contents = wrapped_err!(std::fs::read_to_string(foundry_config_path))?;
            let foundry_toml: toml::Value = wrapped_err!(toml::from_str(&foundry_toml_contents))?;

            let src_path = find_in_toml_value(&foundry_toml, "profile.default", "src")
                .and_then(|v| v.as_str())
                .map(PathBuf::from);

            let remappings_filename = "remappings.txt";
            let remappings_path = path.join(remappings_filename);

            let lines: Vec<String> = if remappings_path.exists() {
                let remappings_content = wrapped_err!(std::fs::read_to_string(remappings_path))?;
                remappings_content.lines().map(str::to_string).collect()
            } else {
                // Reuse the already parsed foundry_toml instead of reading again
                find_in_toml_value(&foundry_toml, "profile.default", "remappings")
                    .and_then(|v| v.as_array())
                    .map(|arr| {
                        arr.iter()
                            .filter_map(|x| x.as_str())
                            .map(String::from)
                            .collect()
                    })
                    .unwrap_or_default()
            };

            let mut remappings = HashMap::new();

            for line in lines {
                if let Some((from, to)) = line.split_once('=') {
                    remappings.insert(from.to_string(), to.to_string());
                }
            }

            return Ok(Framework::Foundry {
                src_path,
                remappings,
            });
        }

        if path.join(Framework::HARDHAT_CONFIG_FILE).exists()
            || path.join(Framework::HARDHAT_CONFIG_FILE_TS).exists()
        {
            return Ok(Framework::Hardhat);
        }

        Err(Error::UnknownFramework)
    }
}

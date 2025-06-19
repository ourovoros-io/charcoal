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
                .map(|v| {
                    let toml::Value::String(s) = v else {
                        return None;
                    };

                    Some(PathBuf::from(s))
                })
                .flatten();

            let remappings_filename = "remappings.txt";

            let lines: Vec<String> = if path.join(remappings_filename).exists() {
                // Get the remappings.txt file from the root of the project folder
                let remappings_content =
                    wrapped_err!(std::fs::read_to_string(path.join(remappings_filename)))?;

                remappings_content.lines().map(str::to_string).collect()
            } else {
                // Get foundry toml file from the root of the project folder
                let remappings_from_toml_str = wrapped_err!(std::fs::read_to_string(
                    path.join(Framework::FOUNDRY_CONFIG_FILE)
                ))?;

                let remappings_from_toml: toml::Value =
                    wrapped_err!(toml::from_str(&remappings_from_toml_str))?;

                let value =
                    find_in_toml_value(&remappings_from_toml, "profile.default", "remappings");

                let Some(value) = value.as_ref() else {
                    return Ok(Framework::Foundry {
                        src_path,
                        remappings: HashMap::new(),
                    });
                };

                let toml::Value::Array(arr) = value else {
                    panic!("remappings key in foundry.toml should be an array")
                };

                arr.iter()
                    .map(|x| x.as_str().unwrap().to_string())
                    .collect()
            };

            let mut remappings = HashMap::new();

            for line in lines {
                let mut split = line.split('=');
                let from = split.next().unwrap().to_string();
                let to = split.next().unwrap().to_string();
                remappings.insert(from, to);
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

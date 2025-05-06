use std::collections::HashMap;
use std::path::{Component, Path, PathBuf};
use crate::error::Result;
use crate::utils::find_project_root_folder;

/// Represents the type of project as [Framework] default is [Framework::Unknown]
#[derive(Clone, Debug, Default)]
pub enum Framework {
    Foundry {
        remappings: HashMap<String, String>,
    },
    Hardhat,
    HardhatTs,
    Brownie {
        remappings: HashMap<String, String>,
    },
    Truffle,
    Dapp,
    #[default]
    Unknown,
}

impl Framework {
    pub const FOUNDRY_CONFIG_FILE: &'static str = "foundry.toml";
    pub const HARDHAT_CONFIG_FILE: &'static str = "hardhat.config.js";
    pub const HARDHAT_TS_CONFIG_FILE: &'static str = "hardhat.config.ts";
    pub const BROWNIE_CONFIG_FILE: &'static str = "brownie-config.yaml";
    pub const TRUFFLE_CONFIG_FILE: &'static str = "truffle-config.js";
    pub const DAPP_CONFIG_FILE: &'static str = "Dappfile";
}


/// Get the project type from the [`PathBuf`] and return a [`Framework`]
pub(crate) fn detect_framework<P: AsRef<Path>>(path: P) -> Result<Framework> {
    let path = path.as_ref();

    let Some(path) = find_project_root_folder(path) else {
        return Ok(Framework::Unknown);
    };

    if path.join(Framework::FOUNDRY_CONFIG_FILE).exists() {
        Ok(Framework::Foundry {
            remappings: get_remappings(
                &Framework::Foundry {
                    remappings: HashMap::new(),
                },
                &path,
            )?,
        })
    } else if path.join(Framework::HARDHAT_CONFIG_FILE).exists()
        || path
            .join(Framework::HARDHAT_TS_CONFIG_FILE)
            .exists()
    {
        Ok(Framework::Hardhat)
    } else if path.join(Framework::BROWNIE_CONFIG_FILE).exists() {
        Ok(Framework::Foundry {
            remappings: get_remappings(
                &Framework::Brownie {
                    remappings: HashMap::new(),
                },
                &path,
            )?,
        })
    } else if path.join(Framework::TRUFFLE_CONFIG_FILE).exists() {
        Ok(Framework::Truffle)
    } else if path.join(Framework::DAPP_CONFIG_FILE).exists() {
        Ok(Framework::Dapp)
    } else {
        Ok(Framework::Unknown)
    }
}

/// Get the project path from the [`Framework`] and return a [`PathBuf`]
pub(crate) fn resolve_framework_path(
    framework: &Framework,
    contract_directory: &Path,
    import_file_name: &str,
) -> Result<PathBuf> {
    let Some(project_root_folder) = find_project_root_folder(contract_directory) else {
        // If we cant find a project root folder we return the logical contract directory by accessing the parent
        let Some(contract_directory) = contract_directory.parent() else {
            return Err("Failed to get parent from path".into());
        };

        return Ok(contract_directory.join(import_file_name));
    };

    match &framework {
        // Remappings in foundry and brownie are handled using the same pattern
        &Framework::Foundry { remappings } | Framework::Brownie { remappings } => {
            for (k, v) in remappings {
                if import_file_name.contains(k) {
                    let project_full_path = project_root_folder.join(v).join("");
                    let result_path =
                        import_file_name.replace(k, project_full_path.to_string_lossy().as_ref());
                    return Ok(PathBuf::from(result_path));
                }
            }

            Ok(contract_directory.join(import_file_name))
        }

        // Remappings in hardhat and truffle are done using the @ symbol and the node_modules folder
        Framework::Hardhat | Framework::Truffle | Framework::HardhatTs => {
            if import_file_name.starts_with('.') {
                Ok(project_root_folder.join(import_file_name))
            } else {
                Ok(project_root_folder
                    .join("node_modules")
                    .join(import_file_name))
            }
        }

        Framework::Dapp => {
            let filename = PathBuf::from(import_file_name);
            let mut components: Vec<_> = filename.components().collect();

            if components.len() <= 1 {
                return Err(
                    "Dapp filename should have more than one component".into(),
                );
            }

            match &components[0] {
                Component::Normal(_) => {
                    components.insert(1, Component::Normal("src".as_ref()));
                    let component = components
                        .iter()
                        .map(|c| c.as_os_str())
                        .collect::<PathBuf>();
                    Ok(project_root_folder.join("lib").join(component))
                }

                _ => Ok(project_root_folder.join(filename)),
            }
        }

        // If we find that the project type is unknown we return the filename as is
        Framework::Unknown => {
            println!("Unable to detect the project framework with path {import_file_name}.");
            Ok(PathBuf::from(import_file_name))
        }
    }
}

/// Get the re mappings from the re mappings file on the root folder of the project represented by the [`PathBuf`]
fn get_remappings(
    framework: &Framework,
    project_root_folder: &Path,
) -> Result<HashMap<String, String>> {
    match framework {
        Framework::Foundry { .. } => {
            let remappings_filename = "remappings.txt";

            let lines: Vec<String> = if project_root_folder.join(remappings_filename).exists() {
                // Get the remappings.txt file from the root of the project folder
                let remappings_content =
                    std::fs::read_to_string(project_root_folder.join(remappings_filename))
                        ?;

                remappings_content.lines().map(str::to_string).collect()
            } else {
                // Get foundry toml file from the root of the project folder
                let remappings_from_toml_str = std::fs::read_to_string(
                    project_root_folder.join(Framework::FOUNDRY_CONFIG_FILE),
                )
                ?;

                let remappings_from_toml: toml::Value =
                    toml::from_str(&remappings_from_toml_str)?;

                let value = find_in_toml_value(
                    &remappings_from_toml,
                    remappings_filename.strip_suffix(".txt").unwrap(),
                );

                let Some(value) = value.as_ref() else {
                    return Ok(HashMap::new());
                };

                let toml::Value::Array(arr) = value else {
                    panic!("remappings key in foundry.toml should be an array")
                };

                arr.iter()
                    .map(|x| x.as_str().unwrap().to_string())
                    .collect::<Vec<_>>()
            };

            let mut remappings = HashMap::new();

            for line in lines {
                let mut split = line.split('=');
                let from = split.next().unwrap().to_string();
                let to = split.next().unwrap().to_string();
                remappings.insert(from, to);
            }

            Ok(remappings)
        }

        Framework::Brownie { .. } => {
            let remappings_from_yaml_str = std::fs::read_to_string(
                project_root_folder.join(Framework::BROWNIE_CONFIG_FILE),
            )
            ?;

            let remappings_from_yaml: serde_yaml::Value =
                serde_yaml::from_str(&remappings_from_yaml_str)?;

            let Some(compiler) = remappings_from_yaml.get("compiler") else {
                return Err("compiler key not found in brownie-config.yaml".into());
            };

            let Some(solc) = compiler.get("solc") else {
                return Err("solc key not found in brownie-config.yaml".into());
            };

            let Some(remappings) = solc
                .get("solidity.remappings")
                .or_else(|| solc.get("remappings"))
            else {
                return Err("solidity.remappings key not found in brownie-config.yaml".into());
            };

            let serde_yaml::Value::Sequence(seq) = remappings else {
                return Err("solidity.remappings should be a sequence".into());
            };

            let mut remappings = HashMap::new();

            for v in seq {
                let mut split = v.as_str().unwrap().split('=');
                let from = split.next().unwrap().to_string();
                let to = split.next().unwrap().to_string();
                remappings.insert(from, to);
            }

            Ok(remappings)
        }

        _ => Ok(HashMap::new()),
    }
}

/// Find a key in a [`toml::Table`] and return the [`toml::Value`]
fn find_in_toml_value(value: &toml::Value, key: &str) -> Option<toml::Value> {
    match value {
        toml::Value::Table(table) => {
            for (k, v) in table {
                if k == key {
                    return Some(v.clone());
                }
                if let Some(val) = find_in_toml_value(v, key) {
                    return Some(val);
                }
            }
            None
        }
        _ => None,
    }
}


#[cfg(test)]
mod tests {

    #[test]
    fn test_foundry_detection_from_file() {
        let framework = super::detect_framework("./test_data/imports/framework/foundry/mytoken/src/MyToken.sol");
        assert!(framework.is_ok());
        println!("Framework : {framework:#?}");
    }

    #[test]
    fn test_foundry_detection_from_folder() {
        let framework = super::detect_framework("./test_data/imports/frameworks/foundry/mytoken/");
        assert!(framework.is_ok());
        println!("Framework : {framework:#?}");
    }
}
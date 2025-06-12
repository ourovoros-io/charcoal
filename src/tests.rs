use crate::{
    cli,
    project::{Framework, Project},
};
use colorize::AnsiColor;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    sync::{Arc, Mutex},
};
use walkdir::WalkDir;

#[test]
fn test_foundry_template() {
    Test::new("foundry-template", "./tests/foundry-template").run();
}

#[test]
fn test_custom_tests() {
    Test::new("custom_tests", "./tests/custom-tests").run();
}

#[test]
fn test_code_samples_x() {
    Test::new("code_samples_x", "./tests/code_samples_x").run();
}

#[test]
fn test_solidity_by_example() {
    Test::new(
        "solidity_by_example",
        "./tests/solidity-by-example/contracts",
    )
    .with_repo("https://github.com/solidity-by-example/solidity-by-example.github.io.git")
    .run();
}

#[test]
fn test_compound_protocol() {
    Test::new("compound_protocol", "./tests/compound-protocol")
        .with_repo("https://github.com/compound-finance/compound-protocol.git")
        .run();
}

#[test]
fn test_openzeppelin_contracts() {
    Test::new("openzeppelin_contracts", "./tests/openzeppelin-contracts")
        .with_repo("https://github.com/OpenZeppelin/openzeppelin-contracts.git")
        .run();
}

#[test]
fn test_uniswap_v2_core() {
    Test::new("uniswap_v2_core", "./tests/uniswap/v2-core")
        .with_repo("https://github.com/Uniswap/v2-core.git")
        .run();
}

#[test]
fn test_uniswap_v2_periphery() {
    Test::new("uniswap_v2_periphery", "./tests/uniswap/v2-periphery")
        .with_repo("https://github.com/Uniswap/v2-periphery.git")
        .run();
}

#[test]
fn test_uniswap_v3_core() {
    Test::new("uniswap_v3_core", "./tests/uniswap/v3-core")
        .with_repo("https://github.com/Uniswap/v3-core.git")
        .run();
}

#[test]
fn test_uniswap_v3_periphery() {
    Test::new("uniswap_v3_periphery", "./tests/uniswap/v3-periphery")
        .with_repo("https://github.com/Uniswap/v3-periphery.git")
        .run();
}

#[test]
fn test_uniswap_v4_core() {
    Test::new("uniswap_v4_core", "./tests/uniswap/v4-core")
        .with_repo("https://github.com/Uniswap/v4-core.git")
        .run();
}

#[test]
fn test_uniswap_v4_periphery() {
    Test::new("uniswap_v4_periphery", "./tests/uniswap/v4-periphery")
        .with_repo("https://github.com/Uniswap/v4-periphery.git")
        .run();
}

#[derive(Debug)]
pub struct Test {
    pub options: cli::Args,
    pub repo: Option<String>,
}

impl Test {
    const LINE_LENGTH: usize = 100;

    pub fn new<S: AsRef<str>, P: AsRef<Path>>(name: S, path: P) -> Self {
        Self {
            options: cli::Args {
                input: path.as_ref().into(),
                output_directory: Some(format!("./output/{}", name.as_ref()).into()),
                name: Some(name.as_ref().into()),
            }
            .canonicalize()
            .unwrap(),
            repo: None,
        }
    }

    pub fn with_repo(mut self, url: &str) -> Self {
        self.repo = Some(url.into());
        self
    }

    pub fn run(&self) {
        if !self.clone_repo() {
            panic!()
        }
        let framework = Framework::from_path(&self.options.input)
            .expect("Failed to detect a Solidity project framework");

        if !self.install_dependencies(&framework) {
            panic!()
        }

        let project = Arc::new(Mutex::new(
            Project::new(self.options.clone(), framework.clone())
                .expect("Failed to create Charcoal project"),
        ));

        if !self.translate(project.clone()) {
            panic!()
        }

        if !self.build() {
            panic!()
        }
    }

    fn clone_repo(&self) -> bool {
        let Some(repo) = self.repo.as_ref() else {
            return true;
        };

        if self.options.input.exists() {
            return true;
        }

        print!("Cloning repository: {} ...", repo);

        let output = Command::new("git")
            .args(&[
                "clone",
                "--recursive",
                repo,
                self.options
                    .input
                    .to_str()
                    .expect("Failed to convert path to string"),
            ])
            .output()
            .expect("Failed to execute git clone command");

        if !output.status.success() {
            println!("{}", " FAILED".red());
            return false;
        }

        println!("{}", " OK".green());
        true
    }

    fn install_dependencies(&self, framework: &Framework) -> bool {
        print!("Installing dependencies ...");

        let mut result = true;

        match framework {
            Framework::Unknown => {}

            Framework::Foundry { .. } => {
                // Run `forge install`
                let output = Command::new("forge")
                    .current_dir(self.options.input.clone())
                    .arg("install")
                    .output()
                    .expect("Failed to execute command");

                result = output.status.success();
            }

            Framework::Hardhat => {
                //
                // TODO: We might need to redo this...
                //

                let package_json_path = PathBuf::from(format!(
                    "{}/package.json",
                    self.options.input.to_string_lossy()
                ));

                let node_modules_folder = PathBuf::from(format!(
                    "{}/node_modules",
                    self.options.input.to_string_lossy()
                ));

                if !node_modules_folder.exists() {
                    let yarn_paths: Vec<_> = WalkDir::new(&self.options.input)
                        .into_iter()
                        .filter_map(Result::ok)
                        .filter(|e| e.file_type().is_file())
                        .filter(|e| {
                            e.path()
                                .file_name()
                                .map(|f| f == "yarn.lock")
                                .unwrap_or(false)
                        })
                        .collect();

                    if !yarn_paths.is_empty() {
                        let output = Command::new("yarn")
                            .current_dir(&self.options.input)
                            .output()
                            .expect("Failed to execute yarn command");

                        result = output.status.success();
                    } else if package_json_path.exists() {
                        let output = Command::new("npm")
                            .arg("install")
                            .current_dir(&self.options.input)
                            .output()
                            .expect("Failed to execute npm command");

                        result = output.status.success();
                    }
                }
            }
        }

        println!(
            "{}",
            if result {
                " OK".green()
            } else {
                " FAILED".red()
            }
        );

        result
    }

    fn translate(&self, project: Arc<Mutex<Project>>) -> bool {
        print!("Translating ...");

        // Temporarily suppress unwinding panics
        let panic_hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(|panic_info| {
            if let Some(s) = panic_info.payload().downcast_ref::<&str>() {
                print!("{}", format!(" FAILED: {s}").red());
            } else if let Some(s) = panic_info.payload().downcast_ref::<String>() {
                print!("{}", format!(" FAILED: {s}").red());
            } else {
                print!(" FAILED");
            }
        }));
        let result = std::panic::catch_unwind(|| project.lock().unwrap().translate()).is_ok();
        std::panic::set_hook(panic_hook);

        println!("{}", if result { " OK".green() } else { "".into() });

        result
    }

    fn build(&self) -> bool {
        // Get all the directory paths in the output directory
        let output_directory = self.options.output_directory.as_ref().unwrap();

        let output_paths: Vec<String> = WalkDir::new(output_directory)
            .into_iter()
            .filter_map(Result::ok)
            .filter(|e| e.file_type().is_dir())
            .filter(|e| !e.path().to_string_lossy().contains("src"))
            .filter(|e| e.path() != output_directory)
            .filter(|e| {
                let path_str = e.path().to_string_lossy();
                !path_str.ends_with("out") && !path_str.ends_with("out/debug")
            })
            .map(|e| e.path().to_string_lossy().into_owned())
            .collect();

        // Create a hashmap to store the results of the charcoal analysis
        let results: Mutex<HashMap<String, bool>> = Mutex::new(HashMap::new());

        // Run in every folder in the output folder the command `forc build` in parallel
        output_paths.par_iter().for_each(|output_path| {
            println!("Building: {}", output_path);

            let output = Command::new("forc")
                .arg("build")
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .current_dir(output_path.clone())
                .output()
                .expect("Failed to execute command");

            let mut results = results.lock().unwrap();

            if output.status.success() {
                results.insert(output_path.clone(), true);
            } else {
                results.insert(output_path.clone(), false);
            }
        });

        let results = results.lock().unwrap();

        // Print all the successful paths
        for (path, result) in results.iter() {
            if *result {
                println!("{}", format!("Success : {}", path).green());
            }
        }

        // Print all the failed paths
        for (path, result) in results.iter() {
            if !*result {
                println!("{}", format!("Failed  : {}", path).red());
            }
        }

        // Print the coverage percentage of the charcoal analysis
        let total = results.len();
        let passed = results.values().filter(|&&v| v).count();
        let failed = total - passed;
        let coverage = (passed as f32 / total as f32) * 100.0;

        if total > 0 {
            println!("{}", "-".repeat(Self::LINE_LENGTH));
            println!("[Forc Build Results]");
            println!("{}", "-".repeat(Self::LINE_LENGTH));
            if passed > 0 {
                println!(
                    "{}",
                    format!("Total Passed: {} of {}", passed, total).green()
                );
            }
            if failed > 0 {
                println!("{}", format!("Total Failed: {} of {}", failed, total).red());
            }
            println!("{}", "-".repeat(Self::LINE_LENGTH).magenta());
            println!("{}", format!("[Coverage: {:.2}%]", coverage).magenta());
            println!("{}", "-".repeat(Self::LINE_LENGTH).magenta());
        }

        total == 0 || (passed != 0 && failed == 0)
    }
}

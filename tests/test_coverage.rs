use colorize::AnsiColor;
use rayon::iter::IntoParallelRefIterator;
use rayon::iter::ParallelIterator;

const LINE_LENGTH: usize = 100;


#[test]
fn test_ds_token() {
    let path = std::path::Path::new("./tests/ds-token");
    let repo = "https://github.com/dapphub/ds-token.git";
    run_test(path, repo);
}

#[test]
fn test_compound_protocol() {
    let path = std::path::Path::new("./tests/compound-protocol");
    let repo = "https://github.com/compound-finance/compound-protocol.git";
    run_test(path, repo);
}

#[test]
fn test_solidity_by_example() {
    let path = std::path::Path::new("./tests/solidity-by-example");
    let repo = "https://github.com/solidity-by-example/solidity-by-example.github.io.git";
    run_test(path, repo);
}

#[test]
fn test_openzeppelin_contracts() {
    let path = std::path::Path::new("./tests/openzeppelin-contracts");
    let repo = "https://github.com/OpenZeppelin/openzeppelin-contracts.git";
    run_test(path, repo);
}

#[test]
fn test_custom_tests() {
    let path = std::path::Path::new("./tests/custom-tests");
    run_test(path, "");
}

#[test]
fn test_uniswap_v2_core() {
    let path = std::path::Path::new("./tests/uniswap/v2-core");
    let repo = "https://github.com/Uniswap/v2-core.git";
    run_test(path, repo);
}

#[test]
fn test_uniswap_v2_periphery() {
    let path = std::path::Path::new("./tests/uniswap/v2-periphery");
    let repo = "https://github.com/Uniswap/v2-periphery.git";
    run_test(path, repo);
}

#[test]
fn test_uniswap_v3_core() {
    let path = std::path::Path::new("./tests/uniswap/v3-core");
    let repo = "https://github.com/Uniswap/v3-core.git";
    run_test(path, repo);
}

#[test]
fn test_uniswap_v3_periphery() {
    let path = std::path::Path::new("./tests/uniswap/v3-periphery");
    let repo = "https://github.com/Uniswap/v3-periphery.git";
    run_test(path, repo);
}


#[test]
fn test_uniswap_v4_core() {
    let path = std::path::Path::new("./tests/uniswap/v4-core");
    let repo = "https://github.com/Uniswap/v4-core.git";
    run_test(path, repo);
}

#[test]
fn test_uniswap_v4_periphery() {
    let path = std::path::Path::new("./tests/uniswap/v4-periphery");
    let repo = "https://github.com/Uniswap/v4-periphery.git";
    run_test(path, repo);
}


fn run_test(path: &std::path::Path, target_repo: &str) {
    clone_repo(path, target_repo);
    translate(path);
    build(path.components().last().unwrap().as_os_str().to_str().unwrap());
}


fn clone_repo(path: &std::path::Path, target_repo: &str) {
    if !path.exists() {
        let _ = std::process::Command::new("git")
            .args(&[
                "clone",
                "--recursive",
                target_repo,
                path.to_str().expect("Failed to convert path to string"),
            ])
            .output()
            .expect("Failed to execute git clone command");
    } else {
        println!(
            "{}",
            format!(
                "The directory {:?} already exists, skipping the cloning process",
                path
            )
            .yellow()
        );
    }

    // Check if the path contains the ds-token directory
    if path.display().to_string().contains("ds-token") {
        // Install dependencies with forge install inside the ds-token directory
        // Change to the ds-token directory
        std::env::set_current_dir("./tests/ds-token").expect("Failed to change directory");

        // Run `forge install`
        let output = std::process::Command::new("forge")
            .arg("install")
            .output()
            .expect("Failed to execute command");

        // Check the output of the command
        assert!(
            output.status.success(),
            "Forge install command did not run successfully"
        );

        // Change back to the root directory
        std::env::set_current_dir("../../").expect("Failed to change directory");
    }
}

fn translate(path: &std::path::Path)  {
    println!("{}", "-".repeat(LINE_LENGTH).cyan());
    println!(
        "{}",
        format!("[Running charcoal analysis on: {:?}]", path).cyan()
    );
    println!("{}", "-".repeat(LINE_LENGTH).cyan());

    let name = path.components().last().unwrap().as_os_str().to_str().unwrap();

    let output_folder = &format!("./output/{name}");
    
    // Get all the .sol file paths from the repo and store them in a vector but do not include the .t.sol files
    let paths: Vec<String> = walkdir::WalkDir::new(path)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_file())
        .filter(|e| e.path().extension().and_then(std::ffi::OsStr::to_str) == Some("sol"))
        .filter(|e| !e.path().to_string_lossy().ends_with(".t.sol"))
        .filter(|e| !e.path().to_string_lossy().contains("ds-test"))
        .filter(|e| !e.path().to_string_lossy().contains("contracts/test"))
        .filter(|e| !e.path().to_string_lossy().contains("audits"))
        .map(|e| e.path().to_string_lossy().into_owned())
        .collect();

    // Ensure the output folder exists
    if !std::fs::exists("./output/").expect("Failed to query \"./output/\" directory") {
        std::fs::create_dir("./output/").expect("Failed to create \"./output/\" directory");
    }

    // Create a hashmap to store the results of the charcoal analysis
    let results: std::sync::Mutex<std::collections::HashMap<String, bool>> = std::sync::Mutex::new(std::collections::HashMap::new());

    // Run charcoal for each .sol file in the vector in parallel
    paths.par_iter().for_each(|path| {
        let output = std::process::Command::new("cargo")
            .args(&["run", "--", "--target", &path, "-o", output_folder])
            .output()
            .expect("Failed to execute command");
        
        let mut results = results.lock().unwrap();
        if output.status.success() {
            results.insert(path.clone(), true);
        } else {
            results.insert(path.clone(), false);
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
    let coverage = (passed as f32 / total as f32) * 100.0;
    println!("{}", "-".repeat(LINE_LENGTH).magenta());
    println!("{}", "[Charcoal Analysis Results]".magenta());
    println!("{}", "-".repeat(LINE_LENGTH).magenta());
    println!("{}", format!("[Total    : {}]", total).blue());
    println!("{}", format!("[Passed   : {}]", passed).green());
    println!("{}", format!("[Failed   : {}]", total - passed).red());
    println!("{}", "-".repeat(LINE_LENGTH).magenta());
    println!("{}", format!("[Coverage : {:.2}%]", coverage).magenta());
    println!("{}", "-".repeat(LINE_LENGTH).magenta());
}

fn build(name: &str) {
    let out_folder = &format!("./output/{name}");

    let out_folder = std::path::Path::new(out_folder);

    // Get all the folders paths in the output folder 
    let output_paths: Vec<String> = walkdir::WalkDir::new(out_folder)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_dir())
        .filter(|e| !e.path().to_string_lossy().contains("src"))
        .filter(|e| e.path() != out_folder)
        .filter(|e| {
            let path_str = e.path().to_string_lossy();
            !path_str.ends_with("out") && !path_str.ends_with("out/debug")
        })
        .map(|e| e.path().to_string_lossy().into_owned())
        .collect();

    // Create a hashmap to store the results of the charcoal analysis
    let results: std::sync::Mutex<std::collections::HashMap<String, bool>> = std::sync::Mutex::new(std::collections::HashMap::new());

    // Run in every folder in the output folder the command `forc build` in parallel
    output_paths.par_iter().for_each(|output_path| {
        let output = std::process::Command::new("forc")
            .arg("build")
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
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
    let coverage = (passed as f32 / total as f32) * 100.0;
    println!("{}", "-".repeat(LINE_LENGTH).magenta());
    println!("{}", "[Forc Build Results]".magenta());
    println!("{}", "-".repeat(LINE_LENGTH).magenta());
    println!("{}", format!("[Total    : {}]", total).blue());
    println!("{}", format!("[Passed   : {}]", passed).green());
    println!("{}", format!("[Failed   : {}]", total - passed).red());
    println!("{}", "-".repeat(LINE_LENGTH).magenta());
    println!("{}", format!("[Coverage : {:.2}%]", coverage).magenta());
    println!("{}", "-".repeat(LINE_LENGTH).magenta());
}
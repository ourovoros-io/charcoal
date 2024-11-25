use colorize::AnsiColor;
use rayon::iter::IntoParallelRefIterator;
use rayon::iter::ParallelIterator;

#[test]
fn test_coverage() {
    let mut targets = std::collections::HashMap::new();
    targets.insert(
        "./tests/ds-token",
        "https://github.com/dapphub/ds-token.git",
    );
    targets.insert(
        "./tests/compound-protocol",
        "https://github.com/compound-finance/compound-protocol.git",
    );
    targets.insert(
        "./tests/solidity-by-example",
        "https://github.com/solidity-by-example/solidity-by-example.github.io.git",
    );
    targets.insert(
        "./tests/openzeppelin-contracts",
        "https://github.com/OpenZeppelin/openzeppelin-contracts.git",
    );

    targets.insert("./tests/custom-tests", "");

    targets.insert("./tests/uniswap/v2-core", "https://github.com/Uniswap/v2-core.git");
    targets.insert("./tests/uniswap/v3-core", "https://github.com/Uniswap/v3-core.git");
    targets.insert("./tests/uniswap/v4-core", "https://github.com/Uniswap/v4-core.git");

    targets.insert("./tests/uniswap/v2-periphery", "https://github.com/Uniswap/v2-periphery.git");
    targets.insert("./tests/uniswap/v3-periphery", "https://github.com/Uniswap/v3-periphery.git");
    targets.insert("./tests/uniswap/v4-periphery", "https://github.com/Uniswap/v4-periphery.git");

    targets.par_iter().for_each(|(path, target_repo)| {
        clone_target_repo(std::path::Path::new(path), target_repo);
        process_path(std::path::Path::new(path));
    });
}

fn clone_target_repo(path: &std::path::Path, target_repo: &str) {
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

fn process_path(path: &std::path::Path) {
    let line_length = 100;
    println!("{}", "-".repeat(line_length).cyan());
    println!(
        "{}",
        format!("[Running charcoal analysis on: {:?}]", path).cyan()
    );
    println!("{}", "-".repeat(line_length).cyan());

    // Get all the .sol file paths from the repo and store them in a vector but do not include the .t.sol files
    let paths: Vec<String> = walkdir::WalkDir::new(path)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_file())
        .filter(|e| e.path().extension().and_then(std::ffi::OsStr::to_str) == Some("sol"))
        .filter(|e| !e.path().to_string_lossy().ends_with(".t.sol")) // Exclude the .t.sol files
        .filter(|e| !e.path().to_string_lossy().contains("ds-test")) // Exclude the ds-test directory
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
            .args(&["run", "--", "--target", &path, "-o", "./output"])
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
    println!("{}", "-".repeat(line_length).magenta());
    println!("{}", "[Charcoal Analysis Results]".magenta());
    println!("{}", "-".repeat(line_length).magenta());
    println!("{}", format!("[Total    : {}]", total).blue());
    println!("{}", format!("[Passed   : {}]", passed).green());
    println!("{}", format!("[Failed   : {}]", total - passed).red());
    println!("{}", "-".repeat(line_length).magenta());
    println!("{}", format!("[Coverage : {:.2}%]", coverage).magenta());
    println!("{}", "-".repeat(line_length).magenta());

    let out_folder = std::path::Path::new("./output");

    // Get all the folders paths in the output folder 
    let output_paths: Vec<String> = walkdir::WalkDir::new(out_folder)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_dir())
        .filter(|e| !e.path().to_string_lossy().contains("src"))
        .filter(|e| e.path() != out_folder)
        .map(|e| e.path().to_string_lossy().into_owned())
        .collect();

    let results: std::sync::Mutex<(usize, usize)> = std::sync::Mutex::new((0, 0));

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
            println!("{}", format!("Success : {}", output_path.clone()).green());
            results.0 += 1;
        } else {
            println!("{}", format!("Failed  : {}", output_path).red());
            results.1 += 1;
        }
    });

    let (successful, failed) = *results.lock().unwrap();

    println!("{}", "-".repeat(line_length).magenta());
    println!("{}", "[Forc Build Results]".magenta());
    println!("{}", "-".repeat(line_length).magenta());
    println!("{}", format!("[Total    : {}]", successful + failed).blue());
    println!("{}", format!("[Passed   : {}]", successful).green());
    println!("{}", format!("[Failed   : {}]", failed).red());
    println!("{}", "-".repeat(line_length).magenta());
    println!("{}", format!("[Coverage : {:.2}%]", (successful as f32 / (successful + failed) as f32) * 100.0).magenta());
    println!("{}", "-".repeat(line_length).magenta());
}
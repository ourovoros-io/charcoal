use colorize::AnsiColor;

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

    for (path, target_repo) in targets.iter() {
        clone_target_repo(std::path::Path::new(path), target_repo);
        process_path(std::path::Path::new(path));
    }
}

fn clone_target_repo(path: &std::path::Path, target_repo: &str) {
    if !path.exists() {
        let _ = std::process::Command::new("git")
            .args(&[
                "clone",
                target_repo,
                path.to_str().expect("Failed to convert path to string"),
            ])
            .output()
            .expect("Failed to execute git clone command");
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

    // Create a hashmap to store the results of the charcoal analysis
    let mut results = std::collections::HashMap::new();

    // Run charcoal for each .sol file in the vector
    for path in paths {
        if path.len() > 0 {
            let output = std::process::Command::new("cargo")
                .args(&["run", "--", "--target", &path])
                .output()
                .expect("Filed to execute command");
            if output.status.success() {
                results.insert(path, true);
            } else {
                results.insert(path, false);
            }
        }
    }

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
}

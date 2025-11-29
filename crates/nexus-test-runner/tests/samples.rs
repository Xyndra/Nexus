//! Integration tests for sample Nexus programs.
//!
//! These tests automatically discover and run all valid samples in the `samples/` directory.
//! A valid sample must have:
//! - `nexus.json5` - project configuration
//! - `main.nx` - main source file
//! - `expected.out` - expected output
//!
//! Test functions (fn test_*) are run via `nexus test <path>`.

use std::fs;
use std::path::PathBuf;
use std::process::{Command, Stdio};

/// Get the path to the samples directory
fn samples_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .map(|p| p.join("samples"))
        .unwrap_or_else(|| PathBuf::from("samples"))
}

/// Get the workspace root directory
fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .map(|p| p.to_path_buf())
        .unwrap_or_else(|| PathBuf::from("."))
}

/// Normalize output for comparison (handle line endings)
fn normalize_output(s: &str) -> String {
    s.replace("\r\n", "\n").trim_end().to_string()
}

/// Run a sample program and return its output
fn run_sample(sample_dir: &PathBuf) -> Result<String, String> {
    let output = Command::new("cargo")
        .current_dir(workspace_root())
        .args(["run", "-q", "-p", "nexus-cli", "--", "run"])
        .arg(sample_dir)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(|e| format!("Failed to run cargo: {}", e))?;

    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        Err(format!(
            "Program exited with error:\nstderr: {}\nstdout: {}",
            stderr, stdout
        ))
    }
}

/// Run `nexus test` on a sample directory
fn run_nexus_test(sample_dir: &PathBuf) -> Result<String, String> {
    let output = Command::new("cargo")
        .current_dir(workspace_root())
        .args(["run", "-q", "-p", "nexus-cli", "--", "test"])
        .arg(sample_dir)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(|e| format!("Failed to run nexus test: {}", e))?;

    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();

    if output.status.success() {
        Ok(stdout)
    } else {
        Err(format!(
            "nexus test failed:\nstderr: {}\nstdout: {}",
            stderr, stdout
        ))
    }
}

/// Discovered sample with all required paths
struct Sample {
    name: String,
    dir: PathBuf,
    expected_path: PathBuf,
}

/// Discover all valid sample directories
fn discover_samples() -> Vec<Sample> {
    let mut samples = Vec::new();
    let dir = samples_dir();

    if !dir.exists() {
        return samples;
    }

    if let Ok(entries) = fs::read_dir(&dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                let name = path
                    .file_name()
                    .and_then(|s| s.to_str())
                    .map(String::from)
                    .unwrap_or_default();

                let config_path = path.join("nexus.json5");
                let expected_path = path.join("expected.out");

                // Only include samples with config and expected output
                if config_path.exists() && expected_path.exists() {
                    samples.push(Sample {
                        name,
                        dir: path,
                        expected_path,
                    });
                }
            }
        }
    }

    samples.sort_by(|a, b| a.name.cmp(&b.name));
    samples
}

/// Run a single sample: compare output and run test functions
fn run_single_sample(sample: &Sample) -> Result<(), String> {
    // Run the main program and compare output
    let expected = fs::read_to_string(&sample.expected_path)
        .map_err(|e| format!("{}: Failed to read expected output: {}", sample.name, e))?;

    let actual = run_sample(&sample.dir).map_err(|e| format!("{}: {}", sample.name, e))?;

    let actual_normalized = normalize_output(&actual);
    let expected_normalized = normalize_output(&expected);

    if actual_normalized != expected_normalized {
        return Err(format!(
            "{}: Output mismatch\n  Expected:\n{}\n  Actual:\n{}",
            sample.name,
            expected_normalized
                .lines()
                .map(|l| format!("    {}", l))
                .collect::<Vec<_>>()
                .join("\n"),
            actual_normalized
                .lines()
                .map(|l| format!("    {}", l))
                .collect::<Vec<_>>()
                .join("\n")
        ));
    }

    // Run nexus test to execute any test_* functions
    match run_nexus_test(&sample.dir) {
        Ok(output) => {
            // Check if any tests failed (look for failure indicators)
            if output.contains("failed") && output.contains("✗") {
                return Err(format!(
                    "{}: test functions failed:\n{}",
                    sample.name, output
                ));
            }
            Ok(())
        }
        Err(e) => {
            // If "No test functions found", that's fine
            if e.contains("No test functions found") {
                Ok(())
            } else {
                Err(format!("{}: {}", sample.name, e))
            }
        }
    }
}

#[test]
fn test_all_samples() {
    let samples = discover_samples();

    if samples.is_empty() {
        println!("No samples found in {:?}", samples_dir());
        return;
    }

    let mut passed = 0;
    let mut failed = 0;
    let mut failures = Vec::new();

    for sample in &samples {
        match run_single_sample(sample) {
            Ok(()) => {
                passed += 1;
                println!("✓ {}", sample.name);
            }
            Err(e) => {
                failed += 1;
                failures.push(e);
                println!("✗ {}", sample.name);
            }
        }
    }

    println!();
    println!("═══════════════════════════════════════");
    println!("Sample Tests: {} passed, {} failed", passed, failed);
    println!("═══════════════════════════════════════");

    if !failures.is_empty() {
        println!();
        println!("Failures:");
        for failure in &failures {
            println!();
            println!("{}", failure);
        }
        panic!("{} sample test(s) failed", failed);
    }
}

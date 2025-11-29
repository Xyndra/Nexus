//! Test runner for Nexus sample programs.
//!
//! This crate provides functionality to run Nexus programs from a samples directory
//! and compare their output against expected `.out` files.
//!
//! The test runner executes programs via the CLI binary to ensure reliable output capture.

use std::fs;
use std::path::PathBuf;
use std::process::{Command, Stdio};

/// Result of running a single sample test
#[derive(Debug, Clone)]
pub struct TestResult {
    /// Name of the sample (without extension)
    pub name: String,
    /// Path to the .nx file
    pub source_path: PathBuf,
    /// Path to the .out file
    pub expected_path: PathBuf,
    /// Whether the test passed
    pub passed: bool,
    /// Actual output from running the program
    pub actual_output: String,
    /// Expected output from the .out file
    pub expected_output: String,
    /// Error message if the test failed to run
    pub error: Option<String>,
    /// Diff between expected and actual (if different)
    pub diff: Option<String>,
}

impl TestResult {
    /// Create a passed test result
    pub fn passed(
        name: String,
        source_path: PathBuf,
        expected_path: PathBuf,
        output: String,
    ) -> Self {
        Self {
            name,
            source_path,
            expected_path,
            passed: true,
            actual_output: output.clone(),
            expected_output: output,
            error: None,
            diff: None,
        }
    }

    /// Create a failed test result due to output mismatch
    pub fn mismatch(
        name: String,
        source_path: PathBuf,
        expected_path: PathBuf,
        actual: String,
        expected: String,
    ) -> Self {
        let diff = compute_diff(&expected, &actual);
        Self {
            name,
            source_path,
            expected_path,
            passed: false,
            actual_output: actual,
            expected_output: expected,
            error: None,
            diff: Some(diff),
        }
    }

    /// Create a failed test result due to an error
    pub fn error(
        name: String,
        source_path: PathBuf,
        expected_path: PathBuf,
        error: String,
    ) -> Self {
        Self {
            name,
            source_path,
            expected_path,
            passed: false,
            actual_output: String::new(),
            expected_output: String::new(),
            error: Some(error),
            diff: None,
        }
    }
}

/// Summary of all test results
#[derive(Debug, Clone)]
pub struct TestSummary {
    /// All test results
    pub results: Vec<TestResult>,
    /// Number of passed tests
    pub passed: usize,
    /// Number of failed tests
    pub failed: usize,
    /// Number of skipped tests (missing .out file)
    pub skipped: usize,
}

impl TestSummary {
    /// Create a new empty summary
    pub fn new() -> Self {
        Self {
            results: Vec::new(),
            passed: 0,
            failed: 0,
            skipped: 0,
        }
    }

    /// Add a test result
    pub fn add(&mut self, result: TestResult) {
        if result.passed {
            self.passed += 1;
        } else {
            self.failed += 1;
        }
        self.results.push(result);
    }

    /// Record a skipped test
    pub fn skip(&mut self) {
        self.skipped += 1;
    }

    /// Check if all tests passed
    pub fn all_passed(&self) -> bool {
        self.failed == 0
    }

    /// Get total number of tests run
    pub fn total(&self) -> usize {
        self.passed + self.failed
    }
}

impl Default for TestSummary {
    fn default() -> Self {
        Self::new()
    }
}

/// Configuration for the test runner
#[derive(Debug, Clone)]
pub struct TestRunnerConfig {
    /// Directory containing sample .nx files
    pub samples_dir: PathBuf,
    /// Path to the nexus CLI binary (if None, uses cargo run)
    pub cli_binary: Option<PathBuf>,
    /// Whether to update .out files with actual output on mismatch
    pub update_snapshots: bool,
    /// Filter to only run tests matching this pattern
    pub filter: Option<String>,
    /// Whether to show verbose output
    pub verbose: bool,
}

impl TestRunnerConfig {
    /// Create a new config with the given samples directory
    pub fn new(samples_dir: impl Into<PathBuf>) -> Self {
        Self {
            samples_dir: samples_dir.into(),
            cli_binary: None,
            update_snapshots: false,
            filter: None,
            verbose: false,
        }
    }

    /// Set the CLI binary path
    pub fn cli_binary(mut self, path: impl Into<PathBuf>) -> Self {
        self.cli_binary = Some(path.into());
        self
    }

    /// Set whether to update snapshots
    pub fn update_snapshots(mut self, update: bool) -> Self {
        self.update_snapshots = update;
        self
    }

    /// Set the filter pattern
    pub fn filter(mut self, pattern: impl Into<String>) -> Self {
        self.filter = Some(pattern.into());
        self
    }

    /// Set verbose mode
    pub fn verbose(mut self, verbose: bool) -> Self {
        self.verbose = verbose;
        self
    }
}

/// The test runner
pub struct TestRunner {
    config: TestRunnerConfig,
}

impl TestRunner {
    /// Create a new test runner with the given config
    pub fn new(config: TestRunnerConfig) -> Self {
        Self { config }
    }

    /// Create a test runner for the default samples directory
    pub fn default_samples() -> Self {
        // Try to find the samples directory relative to the crate
        let samples_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .and_then(|p| p.parent())
            .map(|p| p.join("samples"))
            .unwrap_or_else(|| PathBuf::from("samples"));

        Self::new(TestRunnerConfig::new(samples_dir))
    }

    /// Run all sample tests
    pub fn run_all(&self) -> TestSummary {
        let mut summary = TestSummary::new();

        let samples = match self.discover_samples() {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Failed to discover samples: {}", e);
                return summary;
            }
        };

        for sample in samples {
            if let Some(ref filter) = self.config.filter {
                if !sample.name.contains(filter) {
                    continue;
                }
            }

            let result = self.run_sample(&sample);

            if self.config.verbose {
                self.print_result(&result);
            }

            // Update snapshot if configured
            if self.config.update_snapshots && !result.passed && result.error.is_none() {
                if let Err(e) = fs::write(&sample.expected_path, &result.actual_output) {
                    eprintln!(
                        "Failed to update snapshot {}: {}",
                        sample.expected_path.display(),
                        e
                    );
                } else if self.config.verbose {
                    println!("  Updated snapshot: {}", sample.expected_path.display());
                }
            }

            summary.add(result);
        }

        summary
    }

    /// Run a single sample by name
    pub fn run_one(&self, name: &str) -> Option<TestResult> {
        let samples = self.discover_samples().ok()?;
        let sample = samples.into_iter().find(|s| s.name == name)?;
        Some(self.run_sample(&sample))
    }

    /// Discover all sample files in the samples directory
    ///
    /// Samples are organized as subdirectories containing:
    /// - `main.nx` - the source file
    /// - `expected.out` - the expected output
    fn discover_samples(&self) -> Result<Vec<Sample>, String> {
        let mut samples = Vec::new();

        if !self.config.samples_dir.exists() {
            return Err(format!(
                "Samples directory does not exist: {}",
                self.config.samples_dir.display()
            ));
        }

        let entries = fs::read_dir(&self.config.samples_dir)
            .map_err(|e| format!("Failed to read samples directory: {}", e))?;

        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                let name = path
                    .file_name()
                    .and_then(|s| s.to_str())
                    .map(String::from)
                    .unwrap_or_default();

                let source_path = path.join("main.nx");
                let expected_path = path.join("expected.out");

                if source_path.exists() && expected_path.exists() {
                    samples.push(Sample {
                        name,
                        source_path,
                        expected_path,
                    });
                } else if self.config.verbose {
                    if !source_path.exists() {
                        println!("Skipping {} (no main.nx)", path.display());
                    } else {
                        println!("Skipping {} (no expected.out)", path.display());
                    }
                }
            }
        }

        samples.sort_by(|a, b| a.name.cmp(&b.name));
        Ok(samples)
    }

    /// Run a single sample and return the result
    fn run_sample(&self, sample: &Sample) -> TestResult {
        // Read expected output
        let expected = match fs::read_to_string(&sample.expected_path) {
            Ok(s) => s,
            Err(e) => {
                return TestResult::error(
                    sample.name.clone(),
                    sample.source_path.clone(),
                    sample.expected_path.clone(),
                    format!("Failed to read expected output file: {}", e),
                );
            }
        };

        // Run the program via CLI
        let actual = match self.run_via_cli(&sample.source_path) {
            Ok(output) => output,
            Err(e) => {
                return TestResult::error(
                    sample.name.clone(),
                    sample.source_path.clone(),
                    sample.expected_path.clone(),
                    e,
                );
            }
        };

        // Normalize line endings and compare
        let actual_normalized = normalize_output(&actual);
        let expected_normalized = normalize_output(&expected);

        if actual_normalized == expected_normalized {
            TestResult::passed(
                sample.name.clone(),
                sample.source_path.clone(),
                sample.expected_path.clone(),
                actual,
            )
        } else {
            TestResult::mismatch(
                sample.name.clone(),
                sample.source_path.clone(),
                sample.expected_path.clone(),
                actual,
                expected,
            )
        }
    }

    /// Run a Nexus program via the CLI binary
    fn run_via_cli(&self, source_path: &PathBuf) -> Result<String, String> {
        let output = if let Some(ref binary) = self.config.cli_binary {
            // Use the specified binary
            Command::new(binary)
                .arg(source_path)
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .output()
                .map_err(|e| format!("Failed to run CLI binary: {}", e))?
        } else {
            // Use cargo run
            let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
            let workspace_root = manifest_dir
                .parent()
                .and_then(|p| p.parent())
                .ok_or("Failed to find workspace root")?;

            Command::new("cargo")
                .current_dir(workspace_root)
                .args(["run", "-q", "-p", "nexus-cli", "--"])
                .arg(source_path)
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .output()
                .map_err(|e| format!("Failed to run cargo: {}", e))?
        };

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

    /// Print a test result
    fn print_result(&self, result: &TestResult) {
        if result.passed {
            println!("✓ {}", result.name);
        } else if let Some(ref error) = result.error {
            println!("✗ {} - ERROR: {}", result.name, error);
        } else {
            println!("✗ {} - OUTPUT MISMATCH", result.name);
            if let Some(ref diff) = result.diff {
                for line in diff.lines().take(20) {
                    println!("  {}", line);
                }
                let line_count = diff.lines().count();
                if line_count > 20 {
                    println!("  ... ({} more lines)", line_count - 20);
                }
            }
        }
    }

    /// Print the summary
    pub fn print_summary(&self, summary: &TestSummary) {
        println!();
        println!("═══════════════════════════════════════");
        println!("Test Results:");
        println!("  Passed:  {}", summary.passed);
        println!("  Failed:  {}", summary.failed);
        println!("  Skipped: {}", summary.skipped);
        println!("  Total:   {}", summary.total());
        println!("═══════════════════════════════════════");

        if !summary.all_passed() {
            println!();
            println!("Failed tests:");
            for result in &summary.results {
                if !result.passed {
                    println!("  ✗ {}", result.name);
                    if let Some(ref error) = result.error {
                        println!("    Error: {}", error);
                    }
                    if let Some(ref diff) = result.diff {
                        for line in diff.lines().take(10) {
                            println!("    {}", line);
                        }
                    }
                }
            }
        }
    }
}

/// A discovered sample file
#[derive(Debug, Clone)]
struct Sample {
    name: String,
    source_path: PathBuf,
    expected_path: PathBuf,
}

/// Normalize output for comparison (handle line endings)
fn normalize_output(s: &str) -> String {
    s.replace("\r\n", "\n").trim_end().to_string()
}

/// Compute a simple diff between expected and actual output
fn compute_diff(expected: &str, actual: &str) -> String {
    let mut diff = String::new();
    let expected_lines: Vec<&str> = expected.lines().collect();
    let actual_lines: Vec<&str> = actual.lines().collect();

    diff.push_str("--- expected\n");
    diff.push_str("+++ actual\n");

    let max_lines = expected_lines.len().max(actual_lines.len());

    for i in 0..max_lines {
        let exp = expected_lines.get(i).copied();
        let act = actual_lines.get(i).copied();

        match (exp, act) {
            (Some(e), Some(a)) if e == a => {
                diff.push_str(&format!("  {}\n", e));
            }
            (Some(e), Some(a)) => {
                diff.push_str(&format!("- {}\n", e));
                diff.push_str(&format!("+ {}\n", a));
            }
            (Some(e), None) => {
                diff.push_str(&format!("- {}\n", e));
            }
            (None, Some(a)) => {
                diff.push_str(&format!("+ {}\n", a));
            }
            (None, None) => {}
        }
    }

    diff
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normalize_output() {
        assert_eq!(normalize_output("hello\r\nworld\r\n"), "hello\nworld");
        assert_eq!(normalize_output("hello\nworld\n"), "hello\nworld");
        assert_eq!(normalize_output("hello\n"), "hello");
    }

    #[test]
    fn test_compute_diff() {
        let expected = "line1\nline2\nline3";
        let actual = "line1\nchanged\nline3";
        let diff = compute_diff(expected, actual);
        assert!(diff.contains("- line2"));
        assert!(diff.contains("+ changed"));
    }

    #[test]
    fn test_compute_diff_same() {
        let text = "line1\nline2\nline3";
        let diff = compute_diff(text, text);
        // The diff contains "--- expected" header, so check for actual diff lines
        assert!(!diff.contains("\n- "), "Should not have removed lines");
        assert!(!diff.contains("\n+ "), "Should not have added lines");
    }

    #[test]
    fn test_compute_diff_extra_lines() {
        let expected = "line1\nline2";
        let actual = "line1\nline2\nline3";
        let diff = compute_diff(expected, actual);
        assert!(diff.contains("+ line3"));
    }

    #[test]
    fn test_compute_diff_missing_lines() {
        let expected = "line1\nline2\nline3";
        let actual = "line1\nline2";
        let diff = compute_diff(expected, actual);
        assert!(diff.contains("- line3"));
    }

    #[test]
    fn test_test_summary() {
        let mut summary = TestSummary::new();
        assert!(summary.all_passed());
        assert_eq!(summary.total(), 0);

        summary.add(TestResult::passed(
            "test1".to_string(),
            PathBuf::from("test1.nx"),
            PathBuf::from("test1.out"),
            "output".to_string(),
        ));
        assert!(summary.all_passed());
        assert_eq!(summary.passed, 1);
        assert_eq!(summary.total(), 1);

        summary.add(TestResult::mismatch(
            "test2".to_string(),
            PathBuf::from("test2.nx"),
            PathBuf::from("test2.out"),
            "actual".to_string(),
            "expected".to_string(),
        ));
        assert!(!summary.all_passed());
        assert_eq!(summary.failed, 1);
        assert_eq!(summary.total(), 2);
    }

    #[test]
    fn test_test_result_passed() {
        let result = TestResult::passed(
            "test".to_string(),
            PathBuf::from("test.nx"),
            PathBuf::from("test.out"),
            "output".to_string(),
        );
        assert!(result.passed);
        assert!(result.error.is_none());
        assert!(result.diff.is_none());
    }

    #[test]
    fn test_test_result_error() {
        let result = TestResult::error(
            "test".to_string(),
            PathBuf::from("test.nx"),
            PathBuf::from("test.out"),
            "some error".to_string(),
        );
        assert!(!result.passed);
        assert!(result.error.is_some());
        assert!(result.diff.is_none());
    }

    #[test]
    fn test_test_result_mismatch() {
        let result = TestResult::mismatch(
            "test".to_string(),
            PathBuf::from("test.nx"),
            PathBuf::from("test.out"),
            "actual".to_string(),
            "expected".to_string(),
        );
        assert!(!result.passed);
        assert!(result.error.is_none());
        assert!(result.diff.is_some());
    }

    #[test]
    fn test_config_builder() {
        let config = TestRunnerConfig::new("samples")
            .verbose(true)
            .filter("hello")
            .update_snapshots(true)
            .cli_binary("/usr/bin/nexus");

        assert_eq!(config.samples_dir, PathBuf::from("samples"));
        assert!(config.verbose);
        assert_eq!(config.filter, Some("hello".to_string()));
        assert!(config.update_snapshots);
        assert_eq!(config.cli_binary, Some(PathBuf::from("/usr/bin/nexus")));
    }
}

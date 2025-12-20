//! Test runner for Nexus sample programs.
//!
//! This crate provides functionality to run Nexus programs from a samples directory
//! and compare their output against expected `.out` files.
//!
//! The test runner executes programs via the CLI binary to ensure reliable output capture.

use nexus_transpiler_c::{CTranspiler, TranspilerConfig};
use std::fs;
use std::path::{Path, PathBuf};
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
    /// C transpiler test result (if enabled)
    pub c_result: Option<CTestResult>,
}

/// Result of C transpiler test
#[derive(Debug, Clone)]
pub struct CTestResult {
    /// Whether C transpiler test passed
    pub passed: bool,
    /// Output from C-compiled binary
    pub c_output: String,
    /// Compiler used (tcc, gcc, clang, or error message)
    pub compiler: String,
    /// Error message if C test failed
    pub error: Option<String>,
    /// Diff between interpreter and C output (if different)
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
            c_result: None,
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
            c_result: None,
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
            c_result: None,
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
    /// Whether to test C transpiler as well
    pub test_c_transpiler: bool,
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
            test_c_transpiler: false,
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

    /// Set whether to test C transpiler
    pub fn test_c_transpiler(mut self, test: bool) -> Self {
        self.test_c_transpiler = test;
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
                let config_path = path.join("nexus.json5");
                let resources_path = path.join("resources");

                // Check for resources directory with test cases
                if source_path.exists() && resources_path.exists() && resources_path.is_dir() {
                    // Find test cases in resources directory
                    let test_cases = self.discover_test_cases(&resources_path);
                    if !test_cases.is_empty() {
                        // Create a sample for each test case
                        for tc in test_cases {
                            samples.push(Sample {
                                name: format!("{}::{}", name, tc.name),
                                source_path: source_path.clone(),
                                expected_path: tc.expected_path,
                                is_project: config_path.exists(),
                                input_path: Some(tc.input_path),
                            });
                        }
                        continue;
                    }
                }

                if source_path.exists() && expected_path.exists() {
                    samples.push(Sample {
                        name,
                        source_path,
                        expected_path,
                        is_project: config_path.exists(),
                        input_path: None,
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

    /// Discover test cases in a resources directory
    /// Looks for pairs of files like: name.txt (input) and name.out (expected output)
    fn discover_test_cases(&self, resources_path: &PathBuf) -> Vec<TestCase> {
        let mut test_cases = Vec::new();

        if let Ok(entries) = fs::read_dir(resources_path) {
            // Collect all .txt files (inputs)
            let mut input_files: Vec<PathBuf> = entries
                .flatten()
                .map(|e| e.path())
                .filter(|p| p.extension().is_some_and(|ext| ext == "txt"))
                .collect();

            input_files.sort();

            for input_path in input_files {
                let stem = input_path
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or("");
                let expected_path = resources_path.join(format!("{}.out", stem));

                if expected_path.exists() {
                    test_cases.push(TestCase {
                        name: stem.to_string(),
                        input_path,
                        expected_path,
                    });
                }
            }
        }

        test_cases
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
        // For projects with nexus.json5, pass the directory; otherwise pass the file
        let run_path = if sample.is_project {
            sample
                .source_path
                .parent()
                .unwrap_or(&sample.source_path)
                .to_path_buf()
        } else {
            sample.source_path.clone()
        };
        let actual = match self.run_via_cli_with_input(&run_path, sample.input_path.as_ref()) {
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

        let mut result = if actual_normalized == expected_normalized {
            TestResult::passed(
                sample.name.clone(),
                sample.source_path.clone(),
                sample.expected_path.clone(),
                actual.clone(),
            )
        } else {
            TestResult::mismatch(
                sample.name.clone(),
                sample.source_path.clone(),
                sample.expected_path.clone(),
                actual.clone(),
                expected,
            )
        };

        // Run C transpiler test if enabled
        if self.config.test_c_transpiler {
            result.c_result = self.run_c_transpiler_test(sample, &actual);

            // If C transpiler test failed, mark the overall test as failed
            if let Some(ref c_result) = result.c_result {
                if !c_result.passed {
                    result.passed = false;
                }
            }
        }

        result
    }

    /// Test the C transpiler by transpiling, compiling, and running
    fn run_c_transpiler_test(
        &self,
        sample: &Sample,
        interpreter_output: &str,
    ) -> Option<CTestResult> {
        // Get the project directory (parent of main.nx)
        let project_dir = sample.source_path.parent()?;

        // Create a temporary directory for C output
        let temp_dir = project_dir.join("c_transpile");
        if temp_dir.exists() {
            // Try to clean up, but don't fail if we can't (Windows file locking issues)
            let _ = fs::remove_dir_all(&temp_dir);
        }
        if let Err(e) = fs::create_dir_all(&temp_dir) {
            return Some(CTestResult {
                passed: false,
                c_output: String::new(),
                compiler: "setup".to_string(),
                error: Some(format!("Failed to create temp directory: {}", e)),
                diff: None,
            });
        }

        // Transpile to C
        let config = TranspilerConfig {
            output_dir: temp_dir.clone(),
            ..Default::default()
        };
        let mut transpiler = CTranspiler::with_config(config);
        let transpile_result = match transpiler.transpile_project(project_dir) {
            Ok(r) => r,
            Err(e) => {
                return Some(CTestResult {
                    passed: false,
                    c_output: String::new(),
                    compiler: "transpiler".to_string(),
                    error: Some(format!("Transpilation failed: {}", e)),
                    diff: None,
                });
            }
        };

        // Write the C files
        for (path, content) in &transpile_result.files {
            if let Err(e) = fs::write(path, content) {
                return Some(CTestResult {
                    passed: false,
                    c_output: String::new(),
                    compiler: "write".to_string(),
                    error: Some(format!("Failed to write C file {}: {}", path.display(), e)),
                    diff: None,
                });
            }
        }

        // Write runtime files
        let runtime_h_path = temp_dir.join("nexus_runtime.h");
        let runtime_c_path = temp_dir.join("nexus_runtime.c");
        if let Err(e) = fs::write(&runtime_h_path, nexus_transpiler_c::RUNTIME_HEADER) {
            return Some(CTestResult {
                passed: false,
                c_output: String::new(),
                compiler: "write".to_string(),
                error: Some(format!("Failed to write runtime header: {}", e)),
                diff: None,
            });
        }
        if let Err(e) = fs::write(&runtime_c_path, &transpile_result.runtime) {
            return Some(CTestResult {
                passed: false,
                c_output: String::new(),
                compiler: "write".to_string(),
                error: Some(format!("Failed to write runtime: {}", e)),
                diff: None,
            });
        }

        // Try to compile and run with available C compiler
        match self.try_compile_and_run(&temp_dir, &transpile_result, interpreter_output) {
            Ok(result) => Some(result),
            Err(e) => Some(CTestResult {
                passed: false,
                c_output: String::new(),
                compiler: "none".to_string(),
                error: Some(e),
                diff: None,
            }),
        }
    }

    /// Try to compile and run the C code with available compilers
    fn try_compile_and_run(
        &self,
        temp_dir: &Path,
        transpile_result: &nexus_transpiler_c::TranspileResult,
        interpreter_output: &str,
    ) -> Result<CTestResult, String> {
        // Collect all C source files
        let mut c_files: Vec<PathBuf> = transpile_result.files.keys().cloned().collect();
        c_files.push(temp_dir.join("nexus_runtime.c"));

        // Output executable
        let exe_name = if cfg!(windows) {
            "program.exe"
        } else {
            "program"
        };
        let exe_path = temp_dir.join(exe_name);

        // Try compilers in order: clang, gcc, tcc
        let compilers = ["clang", "gcc", "tcc"];

        for compiler in &compilers {
            // Check if compiler exists
            let check = Command::new(compiler)
                .arg("--version")
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status();

            if check.is_err() {
                println!("Compiler {} not found", compiler);
                continue; // Compiler not available
            }

            if self.config.verbose {
                println!("  Trying to compile with {}...", compiler);
            }

            // Compile
            let mut cmd = Command::new(compiler);
            cmd.current_dir(temp_dir);

            for c_file in &c_files {
                cmd.arg(c_file.file_name().unwrap());
            }

            cmd.arg("-o").arg(&exe_path);
            cmd.arg("-lm"); // Link math library

            let compile_output = cmd
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .output()
                .map_err(|e| format!("Failed to run {}: {}", compiler, e))?;

            if !compile_output.status.success() {
                let stderr = String::from_utf8_lossy(&compile_output.stderr);
                if self.config.verbose {
                    println!("  {} compilation failed: {}", compiler, stderr);
                }
                continue; // Try next compiler
            }

            // Run the compiled program
            let run_output = Command::new(&exe_path)
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .output()
                .map_err(|e| format!("Failed to run compiled program: {}", e))?;

            if !run_output.status.success() {
                let stderr = String::from_utf8_lossy(&run_output.stderr);
                return Ok(CTestResult {
                    passed: false,
                    c_output: String::new(),
                    compiler: compiler.to_string(),
                    error: Some(format!("Program execution failed: {}", stderr)),
                    diff: None,
                });
            }

            let c_output = String::from_utf8_lossy(&run_output.stdout).to_string();

            // Compare outputs
            let c_normalized = normalize_output(&c_output);
            let interpreter_normalized = normalize_output(interpreter_output);

            if c_normalized == interpreter_normalized {
                return Ok(CTestResult {
                    passed: true,
                    c_output,
                    compiler: compiler.to_string(),
                    error: None,
                    diff: None,
                });
            } else {
                let diff = compute_diff(&interpreter_normalized, &c_normalized);
                return Ok(CTestResult {
                    passed: false,
                    c_output,
                    compiler: compiler.to_string(),
                    error: Some("Output mismatch between interpreter and C".to_string()),
                    diff: Some(diff),
                });
            }
        }

        Err(
            "No C compiler found (checked: tcc, gcc, clang). Install one to test C transpiler."
                .to_string(),
        )
    }

    /// Run a Nexus program via the CLI binary with optional stdin input
    fn run_via_cli_with_input(
        &self,
        source_path: &PathBuf,
        input_path: Option<&PathBuf>,
    ) -> Result<String, String> {
        let stdin_data = if let Some(input) = input_path {
            Some(
                fs::read_to_string(input)
                    .map_err(|e| format!("Failed to read input file: {}", e))?,
            )
        } else {
            None
        };

        let output = if let Some(ref binary) = self.config.cli_binary {
            // Use the specified binary
            let mut cmd = Command::new(binary);
            cmd.arg("run")
                .arg(source_path)
                .stdout(Stdio::piped())
                .stderr(Stdio::piped());

            if stdin_data.is_some() {
                cmd.stdin(Stdio::piped());
            }

            let mut child = cmd
                .spawn()
                .map_err(|e| format!("Failed to run CLI binary: {}", e))?;

            if let Some(ref data) = stdin_data {
                use std::io::Write;
                if let Some(mut stdin) = child.stdin.take() {
                    stdin
                        .write_all(data.as_bytes())
                        .map_err(|e| format!("Failed to write to stdin: {}", e))?;
                }
            }

            child
                .wait_with_output()
                .map_err(|e| format!("Failed to wait for CLI binary: {}", e))?
        } else {
            // Use cargo run
            let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
            let workspace_root = manifest_dir
                .parent()
                .and_then(|p| p.parent())
                .ok_or("Failed to find workspace root")?;

            let mut cmd = Command::new("cargo");
            cmd.current_dir(workspace_root)
                .args(["run", "-q", "-p", "nexus-cli", "--", "run"])
                .arg(source_path)
                .stdout(Stdio::piped())
                .stderr(Stdio::piped());

            if stdin_data.is_some() {
                cmd.stdin(Stdio::piped());
            }

            let mut child = cmd
                .spawn()
                .map_err(|e| format!("Failed to run cargo: {}", e))?;

            if let Some(ref data) = stdin_data {
                use std::io::Write;
                if let Some(mut stdin) = child.stdin.take() {
                    stdin
                        .write_all(data.as_bytes())
                        .map_err(|e| format!("Failed to write to stdin: {}", e))?;
                }
            }

            child
                .wait_with_output()
                .map_err(|e| format!("Failed to wait for cargo: {}", e))?
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
            print!("✓ {}", result.name);
        } else if let Some(ref error) = result.error {
            print!("✗ {} - ERROR: {}", result.name, error);
        } else {
            print!("✗ {} - OUTPUT MISMATCH", result.name);
            if let Some(ref diff) = result.diff {
                println!();
                for line in diff.lines().take(20) {
                    println!("  {}", line);
                }
                let line_count = diff.lines().count();
                if line_count > 20 {
                    println!("  ... ({} more lines)", line_count - 20);
                }
            }
        }

        // Print C transpiler result
        if let Some(ref c_result) = result.c_result {
            if c_result.passed {
                println!(" [C: ✓ with {}]", c_result.compiler);
            } else if let Some(ref error) = c_result.error {
                println!(" [C: ⚠ {}]", error);
                if self.config.verbose && error.contains("mismatch") {
                    if let Some(ref diff) = c_result.diff {
                        println!("  C Transpiler Diff (Interpreter vs C):");
                        for line in diff.lines().take(15) {
                            println!("    {}", line);
                        }
                    }
                }
            }
        } else if !result.passed {
            println!();
        }

        if result.passed && result.c_result.is_none() {
            println!();
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
                    // Show C transpiler errors
                    if let Some(ref c_result) = result.c_result {
                        if !c_result.passed {
                            if let Some(ref error) = c_result.error {
                                println!("    C Transpiler: {}", error);
                            }
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
    is_project: bool,
    /// Optional input file to pipe to stdin
    input_path: Option<PathBuf>,
}

/// A test case within a sample (for samples with resources directory)
#[derive(Debug, Clone)]
struct TestCase {
    name: String,
    input_path: PathBuf,
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
            .cli_binary("/usr/bin/nexus")
            .test_c_transpiler(true);

        assert_eq!(config.samples_dir, PathBuf::from("samples"));
        assert!(config.verbose);
        assert_eq!(config.filter, Some("hello".to_string()));
        assert!(config.update_snapshots);
        assert_eq!(config.cli_binary, Some(PathBuf::from("/usr/bin/nexus")));
        assert!(config.test_c_transpiler);
    }
}

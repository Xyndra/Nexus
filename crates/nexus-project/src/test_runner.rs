//! Test discovery and execution for Nexus projects.
//!
//! This module provides functionality to:
//! - Discover test functions (functions prefixed with `test_`)
//! - Execute tests and collect results
//! - Report test outcomes

use crate::resolver::ResolvedProject;
use nexus_core::NexusResult;
use nexus_interpreter::{Interpreter, InterpreterConfig};
use nexus_parser::{FunctionDef, Item, Program, parse};
use std::time::{Duration, Instant};

/// A discovered test function
#[derive(Debug, Clone)]
pub struct TestFunction {
    /// Name of the test function
    pub name: String,

    /// The function definition
    pub function: FunctionDef,
}

/// Result of running a single test
#[derive(Debug, Clone)]
pub struct TestResult {
    /// Name of the test
    pub name: String,

    /// Whether the test passed
    pub passed: bool,

    /// Duration of the test
    pub duration: Duration,

    /// Error message if the test failed
    pub error: Option<String>,

    /// Output captured during the test
    pub output: String,
}

/// Summary of all test results
#[derive(Debug, Clone, Default)]
pub struct TestSummary {
    /// Individual test results
    pub results: Vec<TestResult>,

    /// Number of tests that passed
    pub passed: usize,

    /// Number of tests that failed
    pub failed: usize,

    /// Total duration of all tests
    pub total_duration: Duration,
}

impl TestSummary {
    /// Create a new empty summary
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a test result to the summary
    pub fn add_result(&mut self, result: TestResult) {
        if result.passed {
            self.passed += 1;
        } else {
            self.failed += 1;
        }
        self.total_duration += result.duration;
        self.results.push(result);
    }

    /// Check if all tests passed
    pub fn all_passed(&self) -> bool {
        self.failed == 0
    }

    /// Get total number of tests
    pub fn total(&self) -> usize {
        self.passed + self.failed
    }
}

/// Test discovery for finding test functions in source code
pub struct TestDiscovery;

impl TestDiscovery {
    /// Discover all test functions in a parsed program
    ///
    /// Test functions are identified by the `test_` prefix in their name.
    pub fn discover(program: &Program) -> Vec<TestFunction> {
        let mut tests = Vec::new();

        for item in &program.items {
            if let Item::Function(func) = item
                && func.name.starts_with("test_")
            {
                tests.push(TestFunction {
                    name: func.name.clone(),
                    function: func.clone(),
                });
            }
        }

        // Sort tests by name for consistent ordering
        tests.sort_by(|a, b| a.name.cmp(&b.name));
        tests
    }

    /// Discover tests from source code string
    pub fn discover_from_source(source: &str) -> NexusResult<Vec<TestFunction>> {
        let program = parse(source)?;
        Ok(Self::discover(&program))
    }

    /// Get the names of all discovered tests
    pub fn test_names(program: &Program) -> Vec<String> {
        Self::discover(program)
            .into_iter()
            .map(|t| t.name)
            .collect()
    }
}

/// Test runner for executing discovered tests
pub struct TestRunner {
    /// Interpreter configuration
    config: InterpreterConfig,

    /// Whether to print verbose output
    verbose: bool,
}

impl TestRunner {
    /// Create a new test runner with default configuration
    pub fn new() -> Self {
        Self {
            config: InterpreterConfig::default(),
            verbose: false,
        }
    }

    /// Create a test runner with custom interpreter config
    pub fn with_config(config: InterpreterConfig) -> Self {
        Self {
            config,
            verbose: false,
        }
    }

    /// Set verbose output mode
    pub fn verbose(mut self, verbose: bool) -> Self {
        self.verbose = verbose;
        self
    }

    /// Run all tests in a resolved project
    pub fn run_project_tests(&self, project: &ResolvedProject) -> NexusResult<TestSummary> {
        let combined_source = project.combined_source();
        let source = combined_source.as_str();

        // Parse the source
        let program = parse(source)?;

        // Discover tests
        let tests = TestDiscovery::discover(&program);

        if tests.is_empty() {
            return Ok(TestSummary::new());
        }

        // Run each test
        let mut summary = TestSummary::new();

        for test in &tests {
            let result = self.run_single_test(source, &program, test)?;

            if self.verbose {
                let status = if result.passed { "PASS" } else { "FAIL" };
                println!("[{}] {} ({:?})", status, result.name, result.duration);
                if let Some(ref error) = result.error {
                    println!("  Error: {}", error);
                }
            }

            summary.add_result(result);
        }

        Ok(summary)
    }

    /// Run a single test function
    fn run_single_test(
        &self,
        source: &str,
        _program: &Program,
        test: &TestFunction,
    ) -> NexusResult<TestResult> {
        let start = Instant::now();

        // Create a fresh interpreter for each test
        let mut interpreter = Interpreter::with_config(self.config.clone());

        // Parse and load the program
        let program = parse(source)?;
        interpreter.load_program(&program)?;

        // Execute the test function
        let result = interpreter.run_function(&test.name, vec![]);

        let duration = start.elapsed();

        match result {
            Ok(_) => Ok(TestResult {
                name: test.name.clone(),
                passed: true,
                duration,
                error: None,
                output: String::new(),
            }),
            Err(e) => Ok(TestResult {
                name: test.name.clone(),
                passed: false,
                duration,
                error: Some(e.to_string()),
                output: String::new(),
            }),
        }
    }
}

impl Default for TestRunner {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_discover_test_functions() {
        let source = r#"
            std test_addition(): void {
                // test code
            }

            std helper_function(): void {
                // not a test
            }

            std test_subtraction(): void {
                // another test
            }

            std main(): void {
                // main function
            }
        "#;

        let tests = TestDiscovery::discover_from_source(source).unwrap();

        assert_eq!(tests.len(), 2);
        assert_eq!(tests[0].name, "test_addition");
        assert_eq!(tests[1].name, "test_subtraction");
    }

    #[test]
    fn test_discover_no_tests() {
        let source = r#"
            std main(): void {
                // no tests here
            }
        "#;

        let tests = TestDiscovery::discover_from_source(source).unwrap();
        assert!(tests.is_empty());
    }

    #[test]
    fn test_summary_tracking() {
        let mut summary = TestSummary::new();

        summary.add_result(TestResult {
            name: "test_one".to_string(),
            passed: true,
            duration: Duration::from_millis(10),
            error: None,
            output: String::new(),
        });

        summary.add_result(TestResult {
            name: "test_two".to_string(),
            passed: false,
            duration: Duration::from_millis(20),
            error: Some("assertion failed".to_string()),
            output: String::new(),
        });

        summary.add_result(TestResult {
            name: "test_three".to_string(),
            passed: true,
            duration: Duration::from_millis(15),
            error: None,
            output: String::new(),
        });

        assert_eq!(summary.total(), 3);
        assert_eq!(summary.passed, 2);
        assert_eq!(summary.failed, 1);
        assert!(!summary.all_passed());
        assert_eq!(summary.total_duration, Duration::from_millis(45));
    }

    #[test]
    fn test_summary_all_passed() {
        let mut summary = TestSummary::new();

        summary.add_result(TestResult {
            name: "test_one".to_string(),
            passed: true,
            duration: Duration::from_millis(10),
            error: None,
            output: String::new(),
        });

        assert!(summary.all_passed());
    }

    #[test]
    fn test_test_names() {
        let source = r#"
            std test_foo(): void {}
            std test_bar(): void {}
            std not_a_test(): void {}
        "#;

        let program = parse(source).unwrap();
        let names = TestDiscovery::test_names(&program);

        assert_eq!(names.len(), 2);
        assert!(names.contains(&"test_foo".to_string()));
        assert!(names.contains(&"test_bar".to_string()));
    }
}

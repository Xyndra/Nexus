//! Integration tests for sample Nexus programs.
//!
//! These tests automatically discover and run all valid samples in the `samples/` directory.
//! A valid sample must have:
//! - `nexus.json5` - project configuration
//! - `main.nx` - main source file
//! - `expected.out` - expected output (for simple tests)
//!   OR
//! - `resources/` - directory with test cases (name.txt = input, name.out = expected output)
//!
//! Test functions (fn test_*) are run via `nexus test <path>`.
//!
//! Also tests C transpiler by transpiling, compiling, and comparing output.

use nexus_transpiler_c::{CTranspiler, TranspilerConfig};
use rayon::prelude::*;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;
use std::time::{Duration, Instant};

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

/// Timing information for a test run
#[derive(Default, Clone)]
struct TimingInfo {
    interpreter_time: Duration,
    transpiler_time: Duration,
    compiler_time: Duration,
    c_run_time: Duration,
}

impl TimingInfo {
    fn total(&self) -> Duration {
        self.interpreter_time + self.transpiler_time + self.compiler_time + self.c_run_time
    }
}

fn format_duration(d: Duration) -> String {
    let millis = d.as_millis();
    if millis < 1000 {
        format!("{}ms", millis)
    } else {
        format!("{:.2}s", d.as_secs_f64())
    }
}

/// Run a sample program with optional input file path (passed as argument, not stdin)
fn run_sample_with_input(
    sample_dir: &PathBuf,
    input_path: Option<&PathBuf>,
) -> Result<(String, Duration), String> {
    let start = Instant::now();

    let mut cmd = Command::new("cargo");
    cmd.current_dir(workspace_root())
        .args(["run", "-q", "-p", "nexus-cli", "--", "run"])
        .arg(sample_dir);

    // Pass input file path as argument to main() instead of piping to stdin
    if let Some(input) = input_path {
        cmd.arg("--").arg(input);
    }

    cmd.stdout(Stdio::piped()).stderr(Stdio::piped());

    let output = cmd
        .output()
        .map_err(|e| format!("Failed to run cargo: {}", e))?;

    let elapsed = start.elapsed();

    if output.status.success() {
        Ok((String::from_utf8_lossy(&output.stdout).to_string(), elapsed))
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
fn run_nexus_test(sample_dir: &PathBuf) -> Result<(String, Duration), String> {
    let start = Instant::now();

    let output = Command::new("cargo")
        .current_dir(workspace_root())
        .args(["run", "-q", "-p", "nexus-cli", "--", "test"])
        .arg(sample_dir)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(|e| format!("Failed to run nexus test: {}", e))?;

    let elapsed = start.elapsed();
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();

    if output.status.success() {
        Ok((stdout, elapsed))
    } else {
        Err(format!(
            "nexus test failed:\nstderr: {}\nstdout: {}",
            stderr, stdout
        ))
    }
}

/// A test case within a sample
#[derive(Debug, Clone)]
struct TestCase {
    name: String,
    input_path: Option<PathBuf>,
    expected_path: PathBuf,
}

/// Discovered sample with all required paths
struct Sample {
    name: String,
    dir: PathBuf,
    test_cases: Vec<TestCase>,
}

/// Discover test cases in a resources directory
fn discover_test_cases(resources_path: &Path) -> Vec<TestCase> {
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
                    input_path: Some(input_path),
                    expected_path,
                });
            }
        }
    }

    test_cases
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
                let resources_path = path.join("resources");

                // Check for resources directory with test cases
                if config_path.exists() && resources_path.exists() && resources_path.is_dir() {
                    let test_cases = discover_test_cases(&resources_path);
                    if !test_cases.is_empty() {
                        samples.push(Sample {
                            name,
                            dir: path,
                            test_cases,
                        });
                        continue;
                    }
                }

                // Only include samples with config and expected output
                if config_path.exists() && expected_path.exists() {
                    samples.push(Sample {
                        name,
                        dir: path,
                        test_cases: vec![TestCase {
                            name: "main".to_string(),
                            input_path: None,
                            expected_path,
                        }],
                    });
                }
            }
        }
    }

    samples.sort_by(|a, b| a.name.cmp(&b.name));
    samples
}

/// Run a single test case for a sample
fn run_single_test_case(
    sample: &Sample,
    test_case: &TestCase,
    timing: &mut TimingInfo,
) -> Result<(), String> {
    let expected = fs::read_to_string(&test_case.expected_path).map_err(|e| {
        format!(
            "{}::{}: Failed to read expected output: {}",
            sample.name, test_case.name, e
        )
    })?;

    let (actual, interpreter_time) =
        run_sample_with_input(&sample.dir, test_case.input_path.as_ref())
            .map_err(|e| format!("{}::{}: {}", sample.name, test_case.name, e))?;

    timing.interpreter_time += interpreter_time;

    let actual_normalized = normalize_output(&actual);
    let expected_normalized = normalize_output(&expected);

    if actual_normalized != expected_normalized {
        return Err(format!(
            "{}::{}: Output mismatch\n  Expected:\n{}\n  Actual:\n{}",
            sample.name,
            test_case.name,
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

    // Test C transpiler for all test cases
    test_c_transpiler(sample, test_case, &actual_normalized, timing)?;

    Ok(())
}

/// Run a single sample: compare output and run test functions
fn run_single_sample(sample: &Sample) -> Result<TimingInfo, String> {
    let mut timing = TimingInfo::default();

    // Run all test cases
    for test_case in &sample.test_cases {
        run_single_test_case(sample, test_case, &mut timing)?;
    }

    // Run nexus test to execute any test_* functions (only for simple samples)
    if sample.test_cases.len() == 1 && sample.test_cases[0].input_path.is_none() {
        match run_nexus_test(&sample.dir) {
            Ok((output, _test_time)) => {
                // Check if any tests failed (look for failure indicators)
                if output.contains("failed") && output.contains("✗") {
                    return Err(format!(
                        "{}: test functions failed:\n{}",
                        sample.name, output
                    ));
                }
            }
            Err(e) => {
                // If "No test functions found", that's fine
                if !e.contains("No test functions found") {
                    return Err(format!("{}: {}", sample.name, e));
                }
            }
        }
    }

    Ok(timing)
}

/// Test C transpiler for a sample
fn test_c_transpiler(
    sample: &Sample,
    test_case: &TestCase,
    interpreter_output: &str,
    timing: &mut TimingInfo,
) -> Result<(), String> {
    let input_path = test_case.input_path.as_ref();
    // Create temp directory
    let temp_dir = sample.dir.join("c_transpile");
    let _ = fs::remove_dir_all(&temp_dir);
    fs::create_dir_all(&temp_dir).map_err(|e| {
        format!(
            "{}::{}: Failed to create C temp dir: {}",
            sample.name, test_case.name, e
        )
    })?;

    // Transpile
    let transpile_start = Instant::now();
    let config = TranspilerConfig {
        output_dir: temp_dir.clone(),
        ..Default::default()
    };
    let mut transpiler = CTranspiler::with_config(config);
    let result = transpiler.transpile_project(&sample.dir).map_err(|e| {
        format!(
            "{}::{}: C transpilation failed: {}",
            sample.name, test_case.name, e
        )
    })?;
    timing.transpiler_time += transpile_start.elapsed();

    // Write source files (one per top-level module)
    for (file_path, content) in &result.files {
        let output_path = temp_dir.join(file_path.file_name().unwrap());
        fs::write(&output_path, content).map_err(|e| {
            format!(
                "{}::{}: Failed to write C source file: {}",
                sample.name, test_case.name, e
            )
        })?;
    }

    // Write header file
    let header_path = temp_dir.join(result.header_path.file_name().unwrap());
    fs::write(&header_path, &result.header).map_err(|e| {
        format!(
            "{}::{}: Failed to write C header file: {}",
            sample.name, test_case.name, e
        )
    })?;
    fs::write(
        temp_dir.join("nexus_core.h"),
        nexus_transpiler_c::RUNTIME_HEADER,
    )
    .map_err(|e| {
        format!(
            "{}::{}: Failed to write runtime header: {}",
            sample.name, test_case.name, e
        )
    })?;
    fs::write(temp_dir.join("nexus_core.c"), &result.runtime).map_err(|e| {
        format!(
            "{}::{}: Failed to write runtime: {}",
            sample.name, test_case.name, e
        )
    })?;

    // Try to compile and run
    if let Some((c_output, compile_time, run_time)) = try_compile_and_run(
        &temp_dir,
        &result,
        &sample.name,
        &test_case.name,
        input_path,
    )? {
        timing.compiler_time += compile_time;
        timing.c_run_time += run_time;

        let c_normalized = normalize_output(&c_output);
        let interpreter_normalized = normalize_output(interpreter_output);

        if c_normalized != interpreter_normalized {
            return Err(format!(
                "{}::{}: C output differs from interpreter\n  Interpreter:\n{}\n  C:\n{}",
                sample.name,
                test_case.name,
                interpreter_normalized
                    .lines()
                    .map(|l| format!("    {}", l))
                    .collect::<Vec<_>>()
                    .join("\n"),
                c_normalized
                    .lines()
                    .map(|l| format!("    {}", l))
                    .collect::<Vec<_>>()
                    .join("\n")
            ));
        }
    }

    Ok(())
}

/// Try to compile and run C code, return output and timings if successful
fn try_compile_and_run(
    temp_dir: &Path,
    result: &nexus_transpiler_c::TranspileResult,
    sample_name: &str,
    test_case_name: &str,
    input_path: Option<&PathBuf>,
) -> Result<Option<(String, Duration, Duration)>, String> {
    let mut c_files: Vec<PathBuf> = result
        .files
        .keys()
        .map(|p| temp_dir.join(p.file_name().unwrap()))
        .collect();
    c_files.push(temp_dir.join("nexus_core.c"));

    let exe_name = if cfg!(windows) {
        "program.exe"
    } else {
        "program"
    };
    let exe_path = temp_dir.join(exe_name);

    // Try compilers
    for compiler in &["clang", "gcc", "tcc"] {
        if Command::new(compiler)
            .arg("--version")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .is_err()
        {
            continue;
        }

        let compile_start = Instant::now();

        let mut cmd = Command::new(compiler);
        cmd.current_dir(temp_dir);
        cmd.arg("-I").arg(".");
        for c_file in &c_files {
            cmd.arg(c_file.file_name().unwrap());
        }
        cmd.arg("-o").arg(&exe_path);

        // Add math library on Unix-like systems
        if !cfg!(windows) {
            cmd.arg("-lm");
        }

        let compile_output = cmd
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .map_err(|e| {
                format!(
                    "{}::{}: Failed to run {}: {}",
                    sample_name, test_case_name, compiler, e
                )
            })?;

        let compile_time = compile_start.elapsed();

        if !compile_output.status.success() {
            let stderr = String::from_utf8_lossy(&compile_output.stderr);
            return Err(format!(
                "{}::{}: C compilation failed with {}:\n{}",
                sample_name, test_case_name, compiler, stderr
            ));
        }

        let run_start = Instant::now();

        let mut run_cmd = Command::new(&exe_path);
        run_cmd.stdout(Stdio::piped()).stderr(Stdio::piped());

        // Pass input file path as argument if provided
        if let Some(input) = input_path {
            run_cmd.arg(input);
        }

        let run_output = run_cmd.output().map_err(|e| {
            format!(
                "{}::{}: Failed to run compiled program: {}",
                sample_name, test_case_name, e
            )
        })?;

        let run_time = run_start.elapsed();

        if !run_output.status.success() {
            let stderr = String::from_utf8_lossy(&run_output.stderr);
            return Err(format!(
                "{}::{}: Compiled program failed:\n{}",
                sample_name, test_case_name, stderr
            ));
        }

        return Ok(Some((
            String::from_utf8_lossy(&run_output.stdout).to_string(),
            compile_time,
            run_time,
        )));
    }

    // No compiler available - skip C test
    Ok(None)
}

#[test]
fn test_all_samples() {
    let samples = discover_samples();

    if samples.is_empty() {
        println!("No samples found in {:?}", samples_dir());
        return;
    }

    let passed = AtomicUsize::new(0);
    let failed = AtomicUsize::new(0);
    let failures: Mutex<Vec<String>> = Mutex::new(Vec::new());

    samples.par_iter().for_each(|sample| {
        let test_count = sample.test_cases.len();

        match run_single_sample(sample) {
            Ok(timing) => {
                passed.fetch_add(test_count, Ordering::Relaxed);

                let timing_str = format!(
                    "[total: {}, interp: {}, transpile: {}, compile: {}, c_run: {}]",
                    format_duration(timing.total()),
                    format_duration(timing.interpreter_time),
                    format_duration(timing.transpiler_time),
                    format_duration(timing.compiler_time),
                    format_duration(timing.c_run_time),
                );

                if test_count == 1 {
                    println!("✓ {} {}", sample.name, timing_str);
                } else {
                    println!(
                        "✓ {} ({} test cases) {}",
                        sample.name, test_count, timing_str
                    );
                }
            }
            Err(e) => {
                failed.fetch_add(1, Ordering::Relaxed);
                failures.lock().unwrap().push(e);
                println!("✗ {}", sample.name);
            }
        }
    });

    let passed_count = passed.load(Ordering::Relaxed);
    let failed_count = failed.load(Ordering::Relaxed);
    let failures = failures.into_inner().unwrap();

    println!();
    println!("═══════════════════════════════════════");
    println!(
        "Sample Tests: {} passed, {} failed",
        passed_count, failed_count
    );
    println!("═══════════════════════════════════════");

    if !failures.is_empty() {
        println!();
        println!("Failures:");
        for failure in &failures {
            println!();
            println!("{}", failure);
        }
        panic!("{} sample test(s) failed", failed_count);
    }
}

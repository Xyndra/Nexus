//! CLI for the Nexus programming language.
//!
//! Supports running from files, buffers, and interactive mode.

use nexus_core::NexusError;
use nexus_interpreter::{Interpreter, InterpreterConfig, Value};
use nexus_parser::parse;
use nexus_permissions::{CompatPermission, Permission, PermissionSet, PlatPermission};
use nexus_project::{ProjectConfig, ResolvedProject, load_and_resolve_project};
use nexus_sandbox::Sandbox;
use std::env;
use std::fs;
use std::io::{self, BufRead, Write};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

/// CLI configuration parsed from arguments
#[derive(Default)]
struct CliConfig {
    /// Subcommand (run, test, or none for default behavior)
    subcommand: Option<Subcommand>,
    /// Source file or directory to run
    file: Option<PathBuf>,
    /// Code to execute directly
    eval: Option<String>,
    /// Output AST instead of running
    ast: bool,
    /// Lint mode
    lint: bool,
    /// Disable bounds checking
    no_bounds_check: bool,
    /// Run in sandboxed mode
    sandbox: bool,
    /// Maximum execution steps (for sandbox)
    max_steps: Option<usize>,
    /// Show help
    help: bool,
    /// Show version
    version: bool,
    /// REPL mode
    repl: bool,
}

#[derive(Clone, Debug)]
enum Subcommand {
    Run,
    Test,
}

fn parse_args() -> CliConfig {
    let args: Vec<String> = env::args().collect();
    let mut config = CliConfig::default();
    let mut i = 1;

    // Check for subcommand first
    if i < args.len() {
        match args[i].as_str() {
            "run" => {
                config.subcommand = Some(Subcommand::Run);
                i += 1;
            }
            "test" => {
                config.subcommand = Some(Subcommand::Test);
                i += 1;
            }
            _ => {}
        }
    }

    while i < args.len() {
        match args[i].as_str() {
            "-h" | "--help" => config.help = true,
            "-v" | "--version" => config.version = true,
            "-e" | "--eval" => {
                i += 1;
                if i < args.len() {
                    config.eval = Some(args[i].clone());
                }
            }
            "--ast" => config.ast = true,
            "--lint" => config.lint = true,
            "--no-bounds-check" => config.no_bounds_check = true,
            "--sandbox" => config.sandbox = true,
            "--max-steps" => {
                i += 1;
                if i < args.len() {
                    config.max_steps = args[i].parse().ok();
                }
            }
            "--repl" | "-i" => config.repl = true,
            arg if !arg.starts_with('-') => {
                config.file = Some(PathBuf::from(arg));
            }
            _ => {
                eprintln!("Unknown argument: {}", args[i]);
            }
        }
        i += 1;
    }

    // Default to REPL if no file or eval specified
    if config.file.is_none()
        && config.eval.is_none()
        && !config.help
        && !config.version
        && config.subcommand.is_none()
    {
        config.repl = true;
    }

    config
}

fn print_help() {
    println!(
        r#"Nexus Programming Language

USAGE:
    nexus [OPTIONS]
    nexus run <FILE_OR_DIR>
    nexus test <FILE_OR_DIR>

SUBCOMMANDS:
    run <path>              Run a file or directory (for dirs, looks for main.nx)
    test <path>             Run tests for a file or directory

OPTIONS:
    -h, --help              Show this help message
    -v, --version           Show version information
    -e, --eval <CODE>       Execute code directly
    -i, --repl              Start interactive REPL
    --ast                   Output AST instead of running
    --lint                  Lint the code without running
    --no-bounds-check       Disable array bounds checking
    --sandbox               Run in sandboxed mode
    --max-steps <N>         Maximum execution steps (for sandbox)

EXAMPLES:
    nexus run hello.nx          Run a Nexus file
    nexus run hello             Run hello.nx (adds .nx extension)
    nexus run samples/hello     Run samples/hello/main.nx
    nexus test samples/arrays   Run tests for samples/arrays
    nexus -e "println(42)"      Execute code directly
    nexus run --ast hello.nx    Print the AST of a file
    nexus                       Start the REPL
"#
    );
}

fn print_version() {
    println!("Nexus Programming Language v{}", env!("CARGO_PKG_VERSION"));
}

/// Resolve a path to a source file, handling directories
fn resolve_source_path(path: &Path) -> Result<PathBuf, NexusError> {
    if path.is_file() {
        return Ok(path.to_path_buf());
    }

    if path.is_dir() {
        // Look for main.nx in the directory
        let main_path = path.join("main.nx");
        if main_path.exists() {
            return Ok(main_path);
        }

        // Also check for a file with the same name as the directory
        if let Some(name) = path.file_name() {
            let same_name = path.join(format!("{}.nx", name.to_string_lossy()));
            if same_name.exists() {
                return Ok(same_name);
            }
        }

        return Err(NexusError::IoError {
            message: format!("Directory '{}' does not contain main.nx", path.display()),
        });
    }

    // Path doesn't exist, try adding .nx extension
    let with_ext = path.with_extension("nx");
    if with_ext.exists() {
        return Ok(with_ext);
    }

    Err(NexusError::IoError {
        message: format!("File '{}' not found", path.display()),
    })
}

fn run_source(source: &str, config: &CliConfig) -> Result<(), NexusError> {
    // Parse the source
    let program = parse(source)?;

    // AST mode: just print and exit
    if config.ast {
        println!("{:#?}", program);
        return Ok(());
    }

    // Lint mode: check for errors without running
    if config.lint {
        // For now, parsing is our only validation
        println!("Lint passed: no errors found");
        return Ok(());
    }

    // Create interpreter config
    let interp_config = InterpreterConfig {
        bounds_checking: !config.no_bounds_check,
        max_steps: config.max_steps,
        ..Default::default()
    };

    // Run in sandbox or regular mode
    if config.sandbox {
        let sandbox = Sandbox::with_config(interp_config);
        sandbox.run_source(source)?;
    } else {
        let mut interpreter = Interpreter::with_config(interp_config);
        interpreter.load_program(&program)?;
        interpreter.run()?;
    }

    Ok(())
}

fn run_file(path: &PathBuf, config: &CliConfig) -> Result<(), NexusError> {
    let source = fs::read_to_string(path).map_err(|e| NexusError::IoError {
        message: format!("Failed to read file '{}': {}", path.display(), e),
    })?;

    run_source(&source, config)
}

/// Build a PermissionSet for a module based on the project config
fn build_module_permissions(project_config: &ProjectConfig, module_name: &str) -> PermissionSet {
    let mut perms = PermissionSet::new();

    // std is always allowed
    perms.allow(Permission::Std);

    // Check each permission type
    if project_config.has_compat_io_permission(module_name) {
        perms.allow(Permission::Compat(CompatPermission::Io));
    }
    if project_config.has_compat_fs_permission(module_name) {
        perms.allow(Permission::Compat(CompatPermission::Fs));
    }
    if project_config.has_compat_net_permission(module_name) {
        perms.allow(Permission::Compat(CompatPermission::Net));
    }
    if project_config.has_plat_permission(module_name) {
        perms.allow(Permission::Plat(PlatPermission::All));
    }

    perms
}

/// Run a resolved project with proper module permissions
fn run_resolved_project(
    resolved: &ResolvedProject,
    cli_config: &CliConfig,
) -> Result<(), NexusError> {
    // Create interpreter config
    let interp_config = InterpreterConfig {
        bounds_checking: !cli_config.no_bounds_check,
        max_steps: cli_config.max_steps,
        ..Default::default()
    };

    let mut interpreter = Interpreter::with_config(interp_config);

    // Set up permissions for each module based on the root project's config
    for source in &resolved.sources {
        let perms = build_module_permissions(&resolved.config, &source.name);
        interpreter
            .permissions_mut()
            .set_module_permissions(&source.name, perms);
    }

    // First pass: parse all modules and register their functions
    // This allows imports to work across modules
    let mut parsed_programs: Vec<(&str, nexus_parser::Program)> = Vec::new();

    for source in &resolved.sources {
        let program = parse(&source.content)?;

        // Register this module as known
        interpreter.register_module(&source.name)?;

        // Register all functions from this module
        for func in program.functions() {
            interpreter.register_module_function(&source.name, &func.name);
        }

        parsed_programs.push((&source.name, program));
    }

    // Second pass: load each module's source with the correct current module set
    for (module_name, program) in &parsed_programs {
        // Set the current module before loading
        interpreter.set_current_module(*module_name);

        // Load the program (this will check permissions on use statements)
        interpreter.load_program(program)?;
    }

    // Set current module to the main project module before running
    interpreter.set_current_module(&resolved.config.module_name);

    // Run the main function
    interpreter.run()?;

    Ok(())
}

/// Run a file or directory with the `run` subcommand
fn run_path(path: &Path, cli_config: &CliConfig) -> Result<(), NexusError> {
    // Check if this is a project directory with nexus.json5
    let project_config_path = if path.is_dir() {
        path.join("nexus.json5")
    } else {
        path.parent()
            .map(|p| p.join("nexus.json5"))
            .unwrap_or_else(|| PathBuf::from("nexus.json5"))
    };

    if project_config_path.exists() {
        // Use the project system for proper module handling
        let project_dir = project_config_path.parent().unwrap_or(path);
        let resolved = load_and_resolve_project(project_dir)?;

        // AST mode: just print and exit
        if cli_config.ast {
            for source in &resolved.sources {
                println!("=== Module: {} ===", source.name);
                let program = parse(&source.content)?;
                println!("{:#?}", program);
            }
            return Ok(());
        }

        // Lint mode: check for errors without running
        if cli_config.lint {
            for source in &resolved.sources {
                let _ = parse(&source.content)?;
            }
            println!("Lint passed: no errors found");
            return Ok(());
        }

        return run_resolved_project(&resolved, cli_config);
    }

    // Fall back to simple file execution (no project config)
    let source_path = resolve_source_path(path)?;
    run_file(&source_path, cli_config)
}

/// Recursively collect all .nx files in a directory
/// Stops at subdirectories that have their own nexus.json5
fn collect_nx_files(dir: &PathBuf) -> Result<Vec<PathBuf>, NexusError> {
    let mut files = Vec::new();

    if !dir.is_dir() {
        // Single file
        if dir.extension().is_some_and(|ext| ext == "nx") {
            files.push(dir.clone());
        }
        return Ok(files);
    }

    fn collect_recursive(dir: &PathBuf, files: &mut Vec<PathBuf>) -> Result<(), NexusError> {
        let entries = fs::read_dir(dir).map_err(|e| NexusError::IoError {
            message: format!("Failed to read directory '{}': {}", dir.display(), e),
        })?;

        for entry in entries {
            let entry = entry.map_err(|e| NexusError::IoError {
                message: format!("Failed to read directory entry: {}", e),
            })?;
            let path = entry.path();

            if path.is_dir() {
                // Check if this subdirectory has its own nexus.json5
                // If so, it's a separate module and we skip it
                let subdir_config = path.join("nexus.json5");
                if !subdir_config.exists() {
                    // No separate config, include files from this subdirectory
                    collect_recursive(&path, files)?;
                }
            } else if path.extension().is_some_and(|ext| ext == "nx") {
                files.push(path);
            }
        }
        Ok(())
    }

    collect_recursive(dir, &mut files)?;
    files.sort(); // Sort for consistent ordering
    Ok(files)
}

/// Combine multiple source files into a single source string
fn combine_sources(files: &[PathBuf]) -> Result<String, NexusError> {
    let mut combined = String::new();

    for file in files {
        let source = fs::read_to_string(file).map_err(|e| NexusError::IoError {
            message: format!("Failed to read file '{}': {}", file.display(), e),
        })?;
        combined.push_str(&source);
        combined.push('\n');
    }

    Ok(combined)
}

/// Run tests for a file or directory with the `test` subcommand
///
/// Discovers all `test_*` functions in the source and runs them.
/// Test functions must return either:
/// - `bool` (true = pass, false = fail)
/// - `unknown<Error, None>` (None = pass, Error = fail)
fn test_path(path: &Path, cli_config: &CliConfig) -> Result<(), NexusError> {
    let base_path = if path.is_dir() {
        path.to_path_buf()
    } else {
        path.parent()
            .map(PathBuf::from)
            .unwrap_or_else(|| path.to_path_buf())
    };

    // Check if this is a project directory with nexus.json5
    let project_config_path = base_path.join("nexus.json5");

    if project_config_path.exists() {
        // Use the project system for proper module handling
        let resolved = load_and_resolve_project(&base_path)?;
        return test_resolved_project(&resolved, cli_config);
    }

    // Fall back to simple file-based testing (no project config)
    let nx_files = collect_nx_files(&base_path)?;

    if nx_files.is_empty() {
        return Err(NexusError::IoError {
            message: format!("No .nx files found in '{}'", base_path.display()),
        });
    }

    // Combine all sources
    let source = combine_sources(&nx_files)?;

    // Parse the combined source
    let program = parse(&source)?;

    // Find all test functions (functions starting with "test_")
    // and validate their return types
    let mut test_functions: Vec<String> = Vec::new();
    for func in program.functions() {
        if func.name.starts_with("test_") {
            // Check return type - void is not allowed for test functions
            use nexus_parser::TypeExpr;
            let is_void = matches!(&func.return_type, TypeExpr::Void { .. })
                || matches!(&func.return_type, TypeExpr::Named { name, .. } if name == "void");
            if is_void {
                return Err(NexusError::RuntimeError {
                    message: format!(
                        "Test function '{}' has void return type. Test functions must return bool or unknown<Error, None>",
                        func.name
                    ),
                    span: Some(func.span),
                });
            }
            test_functions.push(func.name.clone());
        }
    }

    if test_functions.is_empty() {
        println!("No test functions found in {}", base_path.display());
        return Ok(());
    }

    // Create interpreter
    let interp_config = InterpreterConfig {
        bounds_checking: !cli_config.no_bounds_check,
        max_steps: cli_config.max_steps,
        ..Default::default()
    };

    let mut interpreter = Interpreter::with_config(interp_config);
    interpreter.load_program(&program)?;

    // Run main first if it exists (to set up any state)
    let _ = interpreter.run();

    run_test_functions(&mut interpreter, &test_functions)
}

/// Run tests for a resolved project with proper module handling
fn test_resolved_project(
    resolved: &ResolvedProject,
    cli_config: &CliConfig,
) -> Result<(), NexusError> {
    // Create interpreter config
    let interp_config = InterpreterConfig {
        bounds_checking: !cli_config.no_bounds_check,
        max_steps: cli_config.max_steps,
        ..Default::default()
    };

    let mut interpreter = Interpreter::with_config(interp_config);

    // Set up permissions for each module based on the root project's config
    for source in &resolved.sources {
        let perms = build_module_permissions(&resolved.config, &source.name);
        interpreter
            .permissions_mut()
            .set_module_permissions(&source.name, perms);
    }

    // First pass: parse all modules and register their functions
    let mut parsed_programs: Vec<(&str, nexus_parser::Program)> = Vec::new();
    let mut all_test_functions: Vec<String> = Vec::new();

    for source in &resolved.sources {
        let program = parse(&source.content)?;

        // Register this module as known
        interpreter.register_module(&source.name)?;

        // Register all functions from this module
        for func in program.functions() {
            interpreter.register_module_function(&source.name, &func.name);

            // Collect test functions
            if func.name.starts_with("test_") {
                use nexus_parser::TypeExpr;
                let is_void = matches!(&func.return_type, TypeExpr::Void { .. })
                    || matches!(&func.return_type, TypeExpr::Named { name, .. } if name == "void");
                if is_void {
                    return Err(NexusError::RuntimeError {
                        message: format!(
                            "Test function '{}' has void return type. Test functions must return bool or unknown<Error, None>",
                            func.name
                        ),
                        span: Some(func.span),
                    });
                }
                all_test_functions.push(func.name.clone());
            }
        }

        parsed_programs.push((&source.name, program));
    }

    // Second pass: load each module's source with the correct current module set
    for (module_name, program) in &parsed_programs {
        interpreter.set_current_module(*module_name);
        interpreter.load_program(program)?;
    }

    if all_test_functions.is_empty() {
        println!("No test functions found in {}", resolved.config.module_name);
        return Ok(());
    }

    // Set current module to the main project module before running
    interpreter.set_current_module(&resolved.config.module_name);

    // Run main first if it exists (to set up any state)
    let _ = interpreter.run();

    run_test_functions(&mut interpreter, &all_test_functions)
}

/// Run test functions and report results
fn run_test_functions(
    interpreter: &mut Interpreter,
    test_functions: &[String],
) -> Result<(), NexusError> {
    let mut passed = 0;
    let mut failed = 0;
    let mut failures: Vec<(String, String)> = Vec::new();

    for test_name in test_functions {
        match interpreter.run_function(test_name, vec![]) {
            Ok(value) => {
                // Check the return value
                let test_passed = match &value {
                    Value::Bool(b) => *b,
                    Value::None => true, // None from unknown<Error, None> means pass
                    Value::Void => true, // No return value means pass (didn't panic)
                    _ => {
                        // Any other value is considered a failure
                        failures.push((
                            test_name.clone(),
                            format!("Unexpected return value: {}", value),
                        ));
                        failed += 1;
                        println!("✗ {}", test_name);
                        continue;
                    }
                };

                if test_passed {
                    passed += 1;
                    println!("✓ {}", test_name);
                } else {
                    failed += 1;
                    failures.push((test_name.clone(), "Returned false".to_string()));
                    println!("✗ {}", test_name);
                }
            }
            Err(e) => {
                failed += 1;
                failures.push((test_name.clone(), format!("{}", e)));
                println!("✗ {}", test_name);
            }
        }
    }

    // Print summary
    println!();
    println!("═══════════════════════════════════════");
    println!("Tests: {} passed, {} failed", passed, failed);
    println!("═══════════════════════════════════════");

    if !failures.is_empty() {
        println!();
        println!("Failures:");
        for (name, reason) in &failures {
            println!("  {}: {}", name, reason);
        }
        return Err(NexusError::RuntimeError {
            message: format!("{} test(s) failed", failed),
            span: None,
        });
    }

    Ok(())
}

fn run_repl() -> Result<(), NexusError> {
    println!("Nexus REPL v{}", env!("CARGO_PKG_VERSION"));
    println!("Type 'exit' or Ctrl+C to quit");
    println!();

    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut interpreter = Interpreter::new();

    // Track accumulated source for multi-line input
    let mut accumulated = String::new();

    loop {
        // Print prompt
        let prompt = if accumulated.is_empty() {
            "nexus> "
        } else {
            "...... "
        };
        print!("{}", prompt);
        stdout.flush().unwrap();

        // Read line
        let mut line = String::new();
        match stdin.lock().read_line(&mut line) {
            Ok(0) => {
                // EOF
                println!();
                break;
            }
            Ok(_) => {}
            Err(e) => {
                eprintln!("Error reading input: {}", e);
                break;
            }
        }

        let line = line.trim();

        // Check for exit
        if line == "exit" || line == "quit" {
            break;
        }

        // Check for special commands
        if line.starts_with(':') {
            handle_repl_command(line, &interpreter);
            continue;
        }

        // Accumulate input
        accumulated.push_str(line);
        accumulated.push('\n');

        // Try to parse
        match parse(&accumulated) {
            Ok(program) => {
                // Successfully parsed, try to execute
                match interpreter.load_program(&program) {
                    Ok(_) => match interpreter.run() {
                        Ok(value) => {
                            if !matches!(value, nexus_interpreter::Value::Void) {
                                println!("=> {}", value);
                            }
                        }
                        Err(e) => {
                            eprintln!("Runtime error: {}", e);
                        }
                    },
                    Err(e) => {
                        eprintln!("Load error: {}", e);
                    }
                }
                accumulated.clear();
            }
            Err(NexusError::UnexpectedEof { .. }) => {
                // Incomplete input, wait for more
                continue;
            }
            Err(e) => {
                eprintln!("Parse error: {}", e);
                accumulated.clear();
            }
        }
    }

    Ok(())
}

fn handle_repl_command(cmd: &str, _interpreter: &Interpreter) {
    match cmd {
        ":help" | ":h" => {
            println!("REPL Commands:");
            println!("  :help, :h    Show this help");
            println!("  :clear, :c   Clear the screen");
            println!("  :vars        Show defined variables");
            println!("  :funcs       Show defined functions");
            println!("  exit, quit   Exit the REPL");
        }
        ":clear" | ":c" => {
            print!("\x1B[2J\x1B[1;1H");
        }
        ":vars" => {
            println!("Variable listing not yet implemented");
        }
        ":funcs" => {
            println!("Function listing not yet implemented");
        }
        _ => {
            println!("Unknown command: {}", cmd);
            println!("Type :help for available commands");
        }
    }
}

fn main() -> ExitCode {
    let config = parse_args();

    if config.help {
        print_help();
        return ExitCode::SUCCESS;
    }

    if config.version {
        print_version();
        return ExitCode::SUCCESS;
    }

    let result = match &config.subcommand {
        Some(Subcommand::Run) => {
            if let Some(path) = &config.file {
                run_path(path, &config)
            } else {
                eprintln!("Error: 'run' subcommand requires a file or directory path");
                return ExitCode::FAILURE;
            }
        }
        Some(Subcommand::Test) => {
            if let Some(path) = &config.file {
                test_path(path, &config)
            } else {
                eprintln!("Error: 'test' subcommand requires a file or directory path");
                return ExitCode::FAILURE;
            }
        }
        None => {
            if let Some(code) = &config.eval {
                run_source(code, &config)
            } else if let Some(file) = &config.file {
                eprintln!(
                    "Error: To run '{}', use 'nexus run {}', otherwise consider 'nexus -h'",
                    file.display(),
                    file.display()
                );
                return ExitCode::FAILURE;
            } else if config.repl {
                run_repl()
            } else {
                print_help();
                return ExitCode::SUCCESS;
            }
        }
    };

    match result {
        Ok(_) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("Error: {}", e);
            ExitCode::FAILURE
        }
    }
}

//! C transpiler for the Nexus programming language.
//!
//! This crate provides functionality to transpile Nexus code to standard C,
//! performing type checking and macro expansion during the process.

mod codegen;
pub mod runtime;
mod typechecker;

pub use codegen::CCodeGenerator;
pub use runtime::{RUNTIME_HEADER, generate_runtime};
pub use typechecker::TypeChecker;

use nexus_core::{NexusError, NexusResult};
use nexus_parser::Program;
use nexus_project::{ResolvedProject, load_and_resolve_project_with_stdlib};
use nexus_types::TypeRegistry;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Configuration for the C transpiler
#[derive(Debug, Clone)]
pub struct TranspilerConfig {
    /// Enable debug symbols in generated C code
    pub debug_symbols: bool,
    /// Enable bounds checking
    pub bounds_checking: bool,
    /// Output directory for generated C files
    pub output_dir: PathBuf,
}

impl Default for TranspilerConfig {
    fn default() -> Self {
        Self {
            debug_symbols: false,
            bounds_checking: true,
            output_dir: PathBuf::from("."),
        }
    }
}

/// Result of transpilation
#[derive(Debug)]
pub struct TranspileResult {
    /// Generated C files mapped by their output path
    pub files: HashMap<PathBuf, String>,
    /// Runtime support file
    pub runtime: String,
}

/// Main transpiler for converting Nexus to C
pub struct CTranspiler {
    config: TranspilerConfig,
    type_registry: TypeRegistry,
}

impl CTranspiler {
    /// Create a new C transpiler with default configuration
    pub fn new() -> Self {
        Self::with_config(TranspilerConfig::default())
    }

    /// Create a new C transpiler with custom configuration
    pub fn with_config(config: TranspilerConfig) -> Self {
        Self {
            config,
            type_registry: TypeRegistry::new(),
        }
    }

    /// Transpile a project directory to C
    pub fn transpile_project(&mut self, project_dir: &Path) -> NexusResult<TranspileResult> {
        // Load and resolve the project with all dependencies
        let resolved = load_and_resolve_project_with_stdlib(project_dir)?;

        // Type check all modules
        let typed_programs = Self::typecheck_all_static(&mut self.type_registry, &resolved)?;

        // Collect all programs for cross-module macro expansion
        let all_programs: Vec<&Program> = typed_programs.values().collect();

        // Generate C code for each module
        let mut files = HashMap::new();
        for (module_path, program) in typed_programs.iter() {
            let c_code = self.generate_c_code(program, module_path, &all_programs)?;
            let output_path = self.get_output_path(module_path);
            files.insert(output_path, c_code);
        }

        // Generate the runtime support file
        let runtime = generate_runtime(&self.config);

        Ok(TranspileResult { files, runtime })
    }

    /// Type check all modules in the resolved project
    fn typecheck_all_static(
        type_registry: &mut TypeRegistry,
        resolved: &ResolvedProject,
    ) -> NexusResult<HashMap<PathBuf, Program>> {
        let mut typechecker = TypeChecker::new(type_registry);
        let mut typed_programs = HashMap::new();

        // First, parse all modules
        for module_source in &resolved.sources {
            let source = &module_source.content;
            let program = nexus_parser::parse(source).map_err(|e| NexusError::InternalError {
                message: format!("Failed to parse module {}: {}", module_source.name, e),
            })?;

            // Register all types from this module first
            typechecker.register_types(&program)?;

            let module_path = module_source.path.clone();
            typed_programs.insert(module_path, program);
        }

        // Then type check all modules
        for (_module_path, program) in typed_programs.iter() {
            typechecker.check_program(program)?;
        }

        Ok(typed_programs)
    }

    /// Generate C code for a single program
    fn generate_c_code(
        &self,
        program: &Program,
        module_path: &Path,
        all_programs: &[&Program],
    ) -> NexusResult<String> {
        let mut generator = CCodeGenerator::new(&self.config, &self.type_registry);
        generator.generate(program, module_path, all_programs)
    }

    /// Get the output path for a module
    fn get_output_path(&self, module_path: &Path) -> PathBuf {
        let file_name = module_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("module");

        self.config.output_dir.join(format!("{}.nx.c", file_name))
    }
}

impl Default for CTranspiler {
    fn default() -> Self {
        Self::new()
    }
}

/// Transpile a Nexus project to C
pub fn transpile_project(project_dir: &Path, output_dir: &Path) -> NexusResult<TranspileResult> {
    let config = TranspilerConfig {
        output_dir: output_dir.to_path_buf(),
        ..Default::default()
    };

    let mut transpiler = CTranspiler::with_config(config);
    transpiler.transpile_project(project_dir)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transpiler_creation() {
        let transpiler = CTranspiler::new();
        assert_eq!(transpiler.config.bounds_checking, true);
    }

    #[test]
    fn test_transpile_simple_function() {
        let source = r#"
            std add(i32 a, i32 b): i32 {
                return addi32(a, b)
            }
        "#;

        let program = nexus_parser::parse(source).expect("Failed to parse");
        let config = TranspilerConfig::default();
        let type_registry = TypeRegistry::new();
        let mut generator = CCodeGenerator::new(&config, &type_registry);

        let result = generator.generate(&program, Path::new("test.nx"), &[]);
        assert!(result.is_ok());
    }
}

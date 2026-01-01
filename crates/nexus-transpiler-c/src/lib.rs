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
use nexus_lsp_server::macro_expansion::{MacroExpansionContext, MacroExpansionResult};
use nexus_parser::{Item, Program};
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
    /// Generated C source files mapped by their output path
    /// Each top-level module (e.g., "std", "myproject") gets its own file
    /// with all submodules combined into it
    pub files: HashMap<PathBuf, String>,
    /// Header file with all forward declarations and typedefs
    pub header: String,
    /// Runtime support file
    pub runtime: String,
    /// Output path for the header file
    pub header_path: PathBuf,
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

        // Collect module info for cross-module references (in load order)
        let all_module_info: Vec<(String, &Program)> = resolved
            .sources
            .iter()
            .filter_map(|source| {
                typed_programs
                    .get(&source.path)
                    .map(|prog| (source.name.clone(), prog))
            })
            .collect();

        // Group modules by their top-level module name
        // e.g., "std", "std.collections", "std.util.strings" all go into "std"
        // e.g., "myproject", "myproject.utils" all go into "myproject"
        let mut top_level_groups: HashMap<String, Vec<(String, &Program)>> = HashMap::new();
        for (module_name, program) in &all_module_info {
            let top_level = module_name
                .split('.')
                .next()
                .unwrap_or(module_name)
                .to_string();
            top_level_groups
                .entry(top_level)
                .or_default()
                .push((module_name.clone(), *program));
        }

        // Generate C code for each top-level module group
        let project_module_name = &resolved.config.module_name;
        let mut generator = CCodeGenerator::new(&self.config, &self.type_registry);

        // First pass: register all types and functions from all modules
        generator.register_all_modules(&all_module_info)?;

        // Second pass: generate code for each top-level module group
        let mut files = HashMap::new();
        for (top_level_name, modules) in &top_level_groups {
            let is_root = top_level_name == project_module_name;
            let source = generator.generate_module_group(top_level_name, modules, is_root)?;
            let output_path = self
                .config
                .output_dir
                .join(format!("{}.nx.c", top_level_name));
            files.insert(output_path, source);
        }

        // Generate header with all declarations
        let header = generator.generate_header();

        // Generate the runtime support file
        let runtime = generate_runtime(&self.config);

        // Output paths
        let header_path = self.config.output_dir.join("nexus_decl.h");

        Ok(TranspileResult {
            files,
            header,
            runtime,
            header_path,
        })
    }

    /// Type check all modules in the resolved project
    fn typecheck_all_static(
        type_registry: &mut TypeRegistry,
        resolved: &ResolvedProject,
    ) -> NexusResult<HashMap<PathBuf, Program>> {
        let mut typechecker = TypeChecker::new(type_registry);
        let mut typed_programs = HashMap::new();
        let mut module_names = HashMap::new();

        // First, parse all modules
        let mut all_programs: Vec<Program> = Vec::new();
        for module_source in &resolved.sources {
            let source = &module_source.content;
            let program = nexus_parser::parse(source).map_err(|e| NexusError::InternalError {
                message: format!("Failed to parse module {}: {}", module_source.name, e),
            })?;
            all_programs.push(program);
        }

        // Build macro expansion context from all parsed programs with module names
        // This allows proper import resolution during macro expansion
        let programs_with_modules: Vec<(&str, &Program)> = resolved
            .sources
            .iter()
            .zip(all_programs.iter())
            .map(|(source, prog)| (source.name.as_str(), prog))
            .collect();
        let macro_ctx = MacroExpansionContext::from_programs_with_modules(programs_with_modules);

        // Now register types, expanding top-level macros as needed
        // Also build expanded programs that include macro-generated items
        for (idx, module_source) in resolved.sources.iter().enumerate() {
            let program = &all_programs[idx];

            // Collect expanded items for this module
            let mut expanded_items: Vec<Item> = Vec::new();

            // Expand top-level macros and register types from expanded items
            for item in &program.items {
                Self::expand_item_recursive(
                    item,
                    &macro_ctx,
                    &mut typechecker,
                    &mut expanded_items,
                )?;
            }

            // Create expanded program with macro-generated items included
            let expanded_program = Program {
                items: expanded_items,
                span: program.span,
            };

            let module_path = module_source.path.clone();
            module_names.insert(module_path.clone(), module_source.name.clone());
            typed_programs.insert(module_path, expanded_program);
        }

        // Register all functions and methods from all modules (now includes macro-generated functions)
        for (module_path, program) in typed_programs.iter() {
            let module_name = module_names.get(module_path).unwrap();
            typechecker.register_functions(program, module_name)?;
        }

        // Then type check all modules
        for (_module_path, program) in typed_programs.iter() {
            typechecker.check_program(program)?;
        }

        Ok(typed_programs)
    }

    /// Recursively expand an item, handling nested top-level macro calls
    fn expand_item_recursive(
        item: &Item,
        macro_ctx: &MacroExpansionContext,
        typechecker: &mut TypeChecker,
        expanded_items: &mut Vec<Item>,
    ) -> NexusResult<()> {
        match item {
            Item::Struct(s) => {
                // Register struct directly
                Self::register_struct_from_ast(typechecker, s)?;
                expanded_items.push(item.clone());
            }
            Item::Interface(i) => {
                // Register interface directly
                Self::register_interface_from_ast(typechecker, i)?;
                expanded_items.push(item.clone());
            }
            Item::TopLevelMacroCall(macro_call) => {
                // Expand the macro and register generated types
                let expansion_result = macro_ctx.expand_macro(&macro_call.name, &macro_call.args);
                match expansion_result {
                    MacroExpansionResult::Success { code } => {
                        let macro_expanded_items =
                            nexus_parser::parse_items(&code).map_err(|e| {
                                NexusError::InternalError {
                                    message: format!(
                                        "Failed to parse macro expansion for '{}': {}",
                                        macro_call.name, e
                                    ),
                                }
                            })?;
                        // Recursively process expanded items (handles nested macro calls)
                        for expanded_item in macro_expanded_items {
                            Self::expand_item_recursive(
                                &expanded_item,
                                macro_ctx,
                                typechecker,
                                expanded_items,
                            )?;
                        }
                    }
                    MacroExpansionResult::NotFound => {
                        // Macro not found - keep original item, will be caught later
                        expanded_items.push(item.clone());
                    }
                    MacroExpansionResult::RuntimeOnly { .. } => {
                        // Runtime-only macro - keep original item
                        expanded_items.push(item.clone());
                    }
                    MacroExpansionResult::Error { message } => {
                        return Err(NexusError::InternalError {
                            message: format!(
                                "Macro expansion error for '{}': {}",
                                macro_call.name, message
                            ),
                        });
                    }
                }
            }
            _ => {
                expanded_items.push(item.clone());
            }
        }
        Ok(())
    }

    /// Register a struct from AST into the typechecker
    fn register_struct_from_ast(
        typechecker: &mut TypeChecker,
        s: &nexus_parser::StructDefAst,
    ) -> NexusResult<()> {
        use nexus_types::{StructDef, StructField};

        let mut struct_def = StructDef::new(s.name.clone(), s.span);

        for f in &s.fields {
            let ty = typechecker.resolve_type_expr(&f.ty);
            let field = StructField::new(f.name.clone(), ty, f.span);
            struct_def.add_field(field);
        }

        for impl_name in &s.implements {
            struct_def.add_impl(impl_name.clone());
        }

        typechecker.register_struct(struct_def);
        Ok(())
    }

    /// Register an interface from AST into the typechecker
    fn register_interface_from_ast(
        typechecker: &mut TypeChecker,
        i: &nexus_parser::InterfaceDefAst,
    ) -> NexusResult<()> {
        use nexus_types::{InterfaceDef, InterfaceMethod, MethodParam};

        let mut interface_def = InterfaceDef::new(i.name.clone(), i.span);

        for m in &i.methods {
            let mut method = InterfaceMethod::new(
                m.name.clone(),
                typechecker.resolve_type_expr(&m.return_type),
                m.span,
            )
            .with_color(m.color);

            if m.receiver_mutable {
                method = method.with_mutable_receiver();
            }

            for p in &m.params {
                let param =
                    MethodParam::new(p.name.clone(), typechecker.resolve_type_expr(&p.ty), p.span);
                method = method.with_param(param);
            }

            interface_def.add_method(method);
        }

        for ext in &i.extends {
            interface_def.extend(ext.clone());
        }

        typechecker.register_interface(interface_def);
        Ok(())
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
        assert!(transpiler.config.bounds_checking);
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

        let all_module_info = vec![("test".to_string(), &program)];

        generator.register_all_modules(&all_module_info).unwrap();
        let result = generator.generate_module_group("test", &all_module_info, true);
        assert!(result.is_ok());
    }
}

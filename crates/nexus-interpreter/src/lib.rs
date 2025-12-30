//! Interpreter for the Nexus programming language.
//!
//! This crate provides the runtime interpreter that executes Nexus AST.
//! Note: All operations are function calls - there are no binary or unary operators.

pub mod builtins;
mod scope;
mod value;

pub use builtins::*;
pub use scope::*;
pub use value::*;

use nexus_core::{FunctionColor, NexusError, NexusResult, Span, VarModifiers};
use nexus_parser::{
    ArrayExpr, Assignment, Block, CallExpr, ElseClause, Expression, FieldAccessExpr, FunctionDef,
    IfCondition, IfStmt, IndexExpr, Item, LambdaBody, LambdaExpr, Literal, LiteralKind,
    MacroCallExpr, MethodCallExpr, MethodDef, PatternBody, Program, ReturnStmt, Statement,
    StructDefAst, StructInitExpr, SubscopeStmt, TopLevelMacroCall, UseStatement, VarDecl,
};
use nexus_permissions::{
    CompatPermission, DesktopX64Permission, Permission, PermissionManager, PermissionSet,
    PlatPermission,
};
use nexus_types::{InterfaceDef, NexusType, StructDef, TypeRegistry};
use rustc_hash::{FxHashMap, FxHashSet};

/// Configuration for the interpreter
#[derive(Debug, Clone)]
pub struct InterpreterConfig {
    /// Whether bounds checking is enabled for array access
    pub bounds_checking: bool,
    /// Maximum recursion depth
    pub max_recursion_depth: usize,
    /// Maximum execution steps (for sandboxing)
    pub max_steps: Option<usize>,
}

impl Default for InterpreterConfig {
    fn default() -> Self {
        Self {
            bounds_checking: true,
            max_recursion_depth: 1000,
            max_steps: None,
        }
    }
}

/// The main interpreter for Nexus programs
pub struct Interpreter {
    /// Configuration
    config: InterpreterConfig,
    /// Global scope for global variables
    global_scope: Scope,
    /// Type registry
    type_registry: TypeRegistry,
    /// Function definitions
    functions: FxHashMap<String, FunctionDef>,
    /// Method definitions (keyed by "TypeName.methodName")
    methods: FxHashMap<String, MethodDef>,
    /// Macro definitions
    macros: FxHashMap<String, nexus_parser::MacroDef>,
    /// Struct AST definitions (for default value evaluation)
    struct_defs: FxHashMap<String, StructDefAst>,
    /// Permission manager
    permissions: PermissionManager,
    /// Builtin functions
    builtins: BuiltinRegistry,
    /// Imported symbols mapped to their source module (e.g., "println" -> "compat.io")
    imported_symbols: FxHashMap<String, String>,
    /// Imported modules for qualified access (e.g., "use mymodule" allows "mymodule.func()")
    imported_modules: FxHashSet<String>,
    /// Per-module imported symbols (module_name -> (symbol_name -> source_module))
    module_imported_symbols: FxHashMap<String, FxHashMap<String, String>>,
    /// Macro source modules (qualified_macro_name -> defining_module)
    macro_source_modules: FxHashMap<String, String>,
    /// Pending use statements to be processed after macro expansion (module_name -> use_statements)
    pending_use_statements: FxHashMap<String, Vec<nexus_parser::UseStatement>>,
    /// Known module names (from dependencies)
    known_modules: FxHashSet<String>,
    /// Functions defined in each module (module_name -> function_names)
    module_functions: FxHashMap<String, FxHashSet<String>>,
    /// Macros defined in each module (module_name -> macro_names)
    module_macros: FxHashMap<String, FxHashSet<String>>,
    /// Structs defined in each module (module_name -> struct_names)
    module_structs: FxHashMap<String, FxHashSet<String>>,
    /// Current recursion depth
    recursion_depth: usize,
    /// Step counter for sandboxing
    step_count: usize,
    /// Current function color context
    current_color: FunctionColor,
    /// Current module name
    current_module: String,
    /// Subscope depth counter (0 = not in a subscope)
    subscope_depth: usize,
    /// Program arguments (stored for compat.proc.getargs)
    program_args: Vec<Value>,
}

impl Interpreter {
    /// Create a new interpreter with default configuration
    pub fn new() -> Self {
        Self::with_config(InterpreterConfig::default())
    }

    /// Create a new interpreter with custom configuration
    pub fn with_config(config: InterpreterConfig) -> Self {
        let mut permissions = PermissionManager::new();
        // Default module gets all permissions
        permissions.set_module_permissions("main", PermissionSet::allow_all());

        Self {
            config,
            global_scope: Scope::new_global(),
            type_registry: TypeRegistry::new(),
            functions: FxHashMap::default(),
            methods: FxHashMap::default(),
            macros: FxHashMap::default(),
            struct_defs: FxHashMap::default(),
            permissions,
            builtins: BuiltinRegistry::new(),
            imported_symbols: FxHashMap::default(),
            imported_modules: FxHashSet::default(),
            module_imported_symbols: FxHashMap::default(),
            macro_source_modules: FxHashMap::default(),
            pending_use_statements: FxHashMap::default(),
            known_modules: FxHashSet::default(),
            module_functions: FxHashMap::default(),
            module_macros: FxHashMap::default(),
            module_structs: FxHashMap::default(),
            recursion_depth: 0,
            step_count: 0,
            current_color: FunctionColor::Std,
            current_module: "main".to_string(),
            subscope_depth: 0,
            program_args: Vec::new(),
        }
    }

    /// Load and register a program's definitions (two-phase loading)
    /// This calls register_definitions followed by expand_pending_macros
    pub fn load_program(&mut self, program: &Program) -> NexusResult<()> {
        self.register_definitions(program)?;
        self.expand_pending_macros(program)?;
        Ok(())
    }

    /// Phase 1: Register all definitions (functions, macros, structs, etc.) without expanding macros
    /// This allows all macros to be available before any expansion happens
    /// Macro imports are processed immediately, other imports are deferred to phase 3
    pub fn register_definitions(&mut self, program: &Program) -> NexusResult<()> {
        // Validate program before loading
        Self::validate_program(program)?;

        for item in &program.items {
            match item {
                Item::Use(use_stmt) => {
                    // Process use statement in two ways:
                    // 1. Try to process it now for macros (needed for $macro_call expansion)
                    // 2. Also store it for later processing of macro-generated symbols

                    // First, try to process the import for any already-registered symbols
                    // This handles imports of macros which are needed for phase 2
                    // For compat/plat modules, permission errors are propagated immediately
                    self.try_process_use_statement(use_stmt)?;

                    // Also defer full processing for after macro expansion
                    // This handles imports of symbols generated by macros
                    self.pending_use_statements
                        .entry(self.current_module.clone())
                        .or_default()
                        .push(use_stmt.clone());
                }
                Item::Function(func) => {
                    // Store function with module-qualified name
                    let qualified_name = format!("{}.{}", self.current_module, func.name);
                    if self.functions.contains_key(&qualified_name) {
                        return Err(NexusError::FunctionAlreadyDefined {
                            name: func.name.clone(),
                            span: func.span,
                        });
                    }
                    self.functions.insert(qualified_name, func.clone());
                }
                Item::Method(method) => {
                    let key = format!("{}.{}", method.receiver_type, method.name);
                    self.methods.insert(key, method.clone());
                }
                Item::Struct(struct_def) => {
                    self.register_struct(struct_def)?;
                }
                Item::Interface(interface_def) => {
                    self.register_interface(interface_def)?;
                }
                Item::Macro(macro_def) => {
                    // Store macro with module-qualified name
                    let qualified_name = format!("{}.{}", self.current_module, macro_def.name);
                    self.macros
                        .insert(qualified_name.clone(), macro_def.clone());
                    // Track which module this macro was defined in
                    self.macro_source_modules
                        .insert(qualified_name, self.current_module.clone());
                    // Also register in module_macros for import checking
                    let module_name = self.current_module.clone();
                    self.register_module_macro(&module_name, &macro_def.name);
                }
                Item::TopLevelMacroCall(_) => {
                    // Skip top-level macro calls in phase 1 - they'll be expanded in phase 2
                }
            }
        }
        Ok(())
    }

    /// Phase 2: Expand all pending top-level macro calls
    /// This runs after all modules have registered their definitions
    pub fn expand_pending_macros(&mut self, program: &Program) -> NexusResult<()> {
        for item in &program.items {
            if let Item::TopLevelMacroCall(macro_call) = item {
                self.expand_top_level_macro(macro_call)?;
            }
        }
        Ok(())
    }

    /// Phase 3: Process all pending use statements
    /// This runs after macro expansion so macro-generated symbols are available
    pub fn process_pending_imports(&mut self) -> NexusResult<()> {
        // Take ownership of pending use statements to avoid borrow issues
        let pending = std::mem::take(&mut self.pending_use_statements);

        for (module_name, use_statements) in pending {
            // Set current module context for processing
            let prev_module = self.current_module.clone();
            self.current_module = module_name;

            for use_stmt in use_statements {
                self.process_use_statement(&use_stmt)?;
            }

            self.current_module = prev_module;
        }

        Ok(())
    }

    /// Expand a top-level macro call and register the generated items
    fn expand_top_level_macro(&mut self, macro_call: &TopLevelMacroCall) -> NexusResult<()> {
        // Look up the macro definition
        let qualified_name = format!("{}.{}", self.current_module, macro_call.name);
        let macro_def = if let Some(m) = self.macros.get(&qualified_name).cloned() {
            Some(m)
        } else {
            // Try to find it in imported symbols
            let source_module = self.imported_symbols.get(&macro_call.name).cloned();
            if let Some(module) = source_module {
                let full_name = format!("{}.{}", module, macro_call.name);
                self.macros.get(&full_name).cloned()
            } else {
                None
            }
        }
        .ok_or_else(|| NexusError::RuntimeError {
            message: format!("Top-level macro '{}' not found", macro_call.name),
            span: Some(macro_call.span),
        })?;

        // Create a scope for macro execution
        let mut macro_scope = Scope::new();

        // Bind macro arguments to parameters
        for (param, arg) in macro_def.params.iter().zip(macro_call.args.iter()) {
            let value = self.evaluate_expression(arg, &mut self.global_scope.clone())?;
            macro_scope.define(Variable {
                name: param.name.clone(),
                value,
                modifiers: VarModifiers {
                    mutable: param.mutable,
                    ..VarModifiers::default()
                },
                span: param.span,
            })?;
        }

        // Execute macro body to get the code string
        let macro_result = self.execute_block(&macro_def.body, &mut macro_scope)?;

        // The macro must return a string containing item definitions
        let code_string = match macro_result {
            Value::String(s) => s.into_iter().collect::<String>(),
            _ => {
                return Err(NexusError::TypeError {
                    message: "Top-level macro must return a string containing item definitions"
                        .to_string(),
                    span: macro_def.span,
                });
            }
        };

        // Parse the generated code as a list of items
        let parsed_items =
            nexus_parser::parse_items(&code_string).map_err(|e| NexusError::RuntimeError {
                message: format!("Failed to parse top-level macro expansion: {}", e),
                span: Some(macro_call.span),
            })?;

        // Register all generated items
        for item in parsed_items {
            match item {
                Item::Function(func) => {
                    let qualified_name = format!("{}.{}", self.current_module, func.name);
                    self.functions.insert(qualified_name, func.clone());
                    self.register_module_function(&self.current_module.clone(), &func.name);
                }
                Item::Struct(struct_def) => {
                    self.register_struct(&struct_def)?;
                }
                Item::Macro(macro_def) => {
                    let qualified_name = format!("{}.{}", self.current_module, macro_def.name);
                    self.macros
                        .insert(qualified_name.clone(), macro_def.clone());
                    // Track which module this macro was defined in
                    self.macro_source_modules
                        .insert(qualified_name, self.current_module.clone());
                    self.register_module_macro(&self.current_module.clone(), &macro_def.name);
                }
                Item::Method(method) => {
                    let key = format!("{}.{}", method.receiver_type, method.name);
                    self.methods.insert(key, method.clone());
                }
                Item::Interface(interface_def) => {
                    self.register_interface(&interface_def)?;
                }
                Item::Use(_) => {
                    // Use statements in macro expansion are not supported
                    return Err(NexusError::RuntimeError {
                        message: "Use statements are not allowed in top-level macro expansions"
                            .to_string(),
                        span: Some(macro_call.span),
                    });
                }
                Item::TopLevelMacroCall(nested_macro_call) => {
                    // Recursively expand nested top-level macro calls
                    self.expand_top_level_macro(&nested_macro_call)?;
                }
            }
        }

        Ok(())
    }

    /// Validate a program before execution
    fn validate_program(program: &Program) -> NexusResult<()> {
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    Self::validate_block(&func.body, 0)?;
                }
                Item::Method(method) => {
                    Self::validate_block(&method.body, 0)?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Validate a block for semantic errors
    fn validate_block(block: &Block, subscope_depth: usize) -> NexusResult<()> {
        for stmt in &block.statements {
            Self::validate_statement(stmt, subscope_depth)?;
        }
        Ok(())
    }

    /// Validate a statement for semantic errors
    fn validate_statement(stmt: &Statement, subscope_depth: usize) -> NexusResult<()> {
        match stmt {
            Statement::Return(ret) => {
                if subscope_depth > 0 && ret.value.is_some() {
                    return Err(NexusError::RuntimeError {
                        message: "Return inside a subscope cannot have a value".to_string(),
                        span: Some(ret.span),
                    });
                }
            }
            Statement::If(if_stmt) => {
                Self::validate_block(&if_stmt.then_block, subscope_depth)?;
                if let Some(else_clause) = &if_stmt.else_block {
                    Self::validate_else_clause(else_clause, subscope_depth)?;
                }
            }
            Statement::Subscope(subscope) => {
                Self::validate_block(&subscope.body, subscope_depth + 1)?;
            }
            Statement::Block(block) => {
                Self::validate_block(block, subscope_depth)?;
            }
            _ => {}
        }
        Ok(())
    }

    /// Validate an else clause for semantic errors
    fn validate_else_clause(else_clause: &ElseClause, subscope_depth: usize) -> NexusResult<()> {
        match else_clause {
            ElseClause::Block(block) => {
                Self::validate_block(block, subscope_depth)?;
            }
            ElseClause::ElseIf(else_if) => {
                Self::validate_block(&else_if.then_block, subscope_depth)?;
                if let Some(inner_else) = &else_if.else_block {
                    Self::validate_else_clause(inner_else, subscope_depth)?;
                }
            }
        }
        Ok(())
    }

    /// Process a use statement and register imported symbols
    fn process_use_statement(&mut self, use_stmt: &UseStatement) -> NexusResult<()> {
        let module_path = use_stmt.module_path.join(".");

        // Check if this is a module-level import (empty symbols list)
        let is_module_import = use_stmt.symbols.is_empty();

        // Check if this is a compat module
        if module_path.starts_with("compat.") {
            return self.process_compat_import(use_stmt, &module_path, is_module_import);
        }

        // Check if this is a plat module
        if module_path.starts_with("plat.") {
            return self.process_plat_import(use_stmt, &module_path, is_module_import);
        }

        // Check if this is a known dependency module
        if self.known_modules.contains(&module_path) {
            return self.process_module_import(use_stmt, &module_path, is_module_import);
        }

        Err(NexusError::RuntimeError {
            message: format!("Unknown module: {}", module_path),
            span: Some(use_stmt.span),
        })
    }

    /// Try to process a use statement, silently skipping symbols that aren't available yet
    /// This is used in phase 1 to import macros before expansion
    fn try_process_use_statement(&mut self, use_stmt: &UseStatement) -> NexusResult<()> {
        let module_path = use_stmt.module_path.join(".");

        // Check if this is a module-level import (empty symbols list)
        let is_module_import = use_stmt.symbols.is_empty();

        // Check if this is a compat module
        if module_path.starts_with("compat.") {
            return self.process_compat_import(use_stmt, &module_path, is_module_import);
        }

        // Check if this is a plat module
        if module_path.starts_with("plat.") {
            return self.process_plat_import(use_stmt, &module_path, is_module_import);
        }

        // Check if this is a known dependency module
        if self.known_modules.contains(&module_path) {
            // Use lenient import that doesn't fail on missing symbols
            return self.try_process_module_import(use_stmt, &module_path, is_module_import);
        }

        // Module not known yet - that's OK, will be processed later
        Ok(())
    }

    /// Try to process module import, silently skipping symbols that don't exist yet
    fn try_process_module_import(
        &mut self,
        use_stmt: &UseStatement,
        module_path: &str,
        is_module_import: bool,
    ) -> NexusResult<()> {
        if is_module_import {
            self.imported_modules.insert(module_path.to_string());
        } else {
            let available_functions = self
                .module_functions
                .get(module_path)
                .cloned()
                .unwrap_or_default();
            let available_macros = self
                .module_macros
                .get(module_path)
                .cloned()
                .unwrap_or_default();
            let available_structs = self
                .module_structs
                .get(module_path)
                .cloned()
                .unwrap_or_default();

            for symbol in &use_stmt.symbols {
                let is_macro = available_macros.contains(symbol);
                let is_struct = available_structs.contains(symbol);
                let is_function = available_functions.contains(symbol);

                // Only import if the symbol exists now (macros, existing functions/structs)
                // Other symbols will be imported in phase 3 after macro expansion
                if is_macro || is_struct || is_function {
                    self.imported_symbols
                        .insert(symbol.clone(), module_path.to_string());
                    self.module_imported_symbols
                        .entry(self.current_module.clone())
                        .or_default()
                        .insert(symbol.clone(), module_path.to_string());
                }
            }
        }
        Ok(())
    }

    /// Process an import from a compat module (e.g., compat.io)
    fn process_compat_import(
        &mut self,
        use_stmt: &UseStatement,
        module_path: &str,
        is_module_import: bool,
    ) -> NexusResult<()> {
        // Check if the current module has permission to use this import
        let required_permission = match module_path {
            "compat.io" => Permission::Compat(CompatPermission::Io),
            "compat.fs" => Permission::Compat(CompatPermission::Fs),
            "compat.proc" => Permission::Compat(CompatPermission::Process),
            "compat.net" => Permission::Compat(CompatPermission::Net),
            _ => {
                return Err(NexusError::RuntimeError {
                    message: format!("Unknown compat module: {}", module_path),
                    span: Some(use_stmt.span),
                });
            }
        };

        self.permissions.check_permission(
            &self.current_module,
            &required_permission,
            use_stmt.span,
        )?;

        if is_module_import {
            // Module-level import: register the module for qualified access
            self.imported_modules.insert(module_path.to_string());
        } else {
            // Get available symbols for the module
            let available_symbols: &[&str] = match module_path {
                "compat.io" => &["print", "println"],
                "compat.fs" => &["read_file", "write_file", "file_exists"],
                "compat.proc" => &["getargs", "exit", "getenv"],
                _ => &[],
            };

            // Register the imported symbols
            for symbol in &use_stmt.symbols {
                if !available_symbols.contains(&symbol.as_str()) {
                    return Err(NexusError::RuntimeError {
                        message: format!(
                            "Symbol '{}' not found in module '{}'",
                            symbol, module_path
                        ),
                        span: Some(use_stmt.span),
                    });
                }
                self.imported_symbols
                    .insert(symbol.clone(), module_path.to_string());
                // Also store in per-module imports
                self.module_imported_symbols
                    .entry(self.current_module.clone())
                    .or_default()
                    .insert(symbol.clone(), module_path.to_string());
            }
        }

        Ok(())
    }

    /// Process an import from a plat module (e.g., plat.console)
    fn process_plat_import(
        &mut self,
        use_stmt: &UseStatement,
        module_path: &str,
        is_module_import: bool,
    ) -> NexusResult<()> {
        // Check if the current module has permission to use this import
        let required_permission = match module_path {
            "plat.console" => Permission::Compat(CompatPermission::Console),
            "plat.desktop" => {
                Permission::Plat(PlatPermission::DesktopX64(DesktopX64Permission::All))
            }
            _ => {
                return Err(NexusError::RuntimeError {
                    message: format!("Unknown plat module: {}", module_path),
                    span: Some(use_stmt.span),
                });
            }
        };

        self.permissions.check_permission(
            &self.current_module,
            &required_permission,
            use_stmt.span,
        )?;

        if is_module_import {
            // Module-level import: register the module for qualified access
            self.imported_modules.insert(module_path.to_string());
        } else {
            // Get available symbols for the module
            let available_symbols: &[&str] = match module_path {
                "plat.console" => &["readln"],
                _ => &[],
            };

            // Register the imported symbols
            for symbol in &use_stmt.symbols {
                if !available_symbols.contains(&symbol.as_str()) {
                    return Err(NexusError::RuntimeError {
                        message: format!(
                            "Symbol '{}' not found in module '{}'",
                            symbol, module_path
                        ),
                        span: Some(use_stmt.span),
                    });
                }
                self.imported_symbols
                    .insert(symbol.clone(), module_path.to_string());
                // Also store in per-module imports
                self.module_imported_symbols
                    .entry(self.current_module.clone())
                    .or_default()
                    .insert(symbol.clone(), module_path.to_string());
            }
        }

        Ok(())
    }

    /// Process an import from a dependency module
    fn process_module_import(
        &mut self,
        use_stmt: &UseStatement,
        module_path: &str,
        is_module_import: bool,
    ) -> NexusResult<()> {
        if is_module_import {
            // Module-level import: register the module for qualified access
            self.imported_modules.insert(module_path.to_string());
        } else {
            // Get the functions, macros, and structs available in this module
            let available_functions = self
                .module_functions
                .get(module_path)
                .cloned()
                .unwrap_or_default();
            let available_macros = self
                .module_macros
                .get(module_path)
                .cloned()
                .unwrap_or_default();
            let available_structs = self
                .module_structs
                .get(module_path)
                .cloned()
                .unwrap_or_default();

            // Register the imported symbols
            for symbol in &use_stmt.symbols {
                // Check if it's a macro (no $ prefix needed - macros are just named like functions)
                let is_macro = available_macros.contains(symbol);
                let is_struct = available_structs.contains(symbol);

                if !is_macro && !is_struct && !available_functions.contains(symbol) {
                    return Err(NexusError::RuntimeError {
                        message: format!(
                            "Symbol '{}' not found in module '{}'",
                            symbol, module_path
                        ),
                        span: Some(use_stmt.span),
                    });
                }
                self.imported_symbols
                    .insert(symbol.clone(), module_path.to_string());
                // Also store in per-module imports
                self.module_imported_symbols
                    .entry(self.current_module.clone())
                    .or_default()
                    .insert(symbol.clone(), module_path.to_string());
            }
        }

        Ok(())
    }

    /// Register a struct definition
    fn register_struct(&mut self, def: &StructDefAst) -> NexusResult<()> {
        // Store the AST definition for later default value evaluation
        self.struct_defs.insert(def.name.clone(), def.clone());

        let mut struct_def = StructDef::new(&def.name, def.span);
        for iface in &def.implements {
            struct_def.add_impl(iface);
        }

        // Add fields to the struct definition
        for field_def in &def.fields {
            let field_type = self.resolve_type_expr(&field_def.ty);
            let mut struct_field =
                nexus_types::StructField::new(&field_def.name, field_type, field_def.span);

            // Mark that field has a default (will be evaluated lazily)
            if field_def.default.is_some() {
                struct_field.default_value = Some("<has_default>".to_string());
            }

            struct_def.add_field(struct_field);
        }

        self.type_registry.register_struct(struct_def);

        // Register the struct in the current module for import checking
        let module_name = self.current_module.clone();
        self.register_module_struct(&module_name, &def.name);

        Ok(())
    }

    /// Resolve a type expression to a concrete type
    fn resolve_type_expr(&self, ty_expr: &nexus_parser::TypeExpr) -> NexusType {
        use nexus_parser::{ArraySizeExpr, TypeExpr};
        use nexus_types::{ArraySize, ArrayType, PrimitiveType};

        match ty_expr {
            TypeExpr::Named { name, .. } => {
                let ty = NexusType::Named(name.clone());
                self.type_registry.resolve_type(&ty)
            }
            TypeExpr::Array { element, size, .. } => {
                let element_type = Box::new(self.resolve_type_expr(element));
                let array_size = match size {
                    ArraySizeExpr::Fixed(n) => ArraySize::Fixed(*n),
                    ArraySizeExpr::Dynamic => ArraySize::Dynamic,
                };
                NexusType::Array(ArrayType {
                    element_type,
                    size: array_size,
                    prealloc: None,
                })
            }
            TypeExpr::Void { .. } => NexusType::Primitive(PrimitiveType::Void),
            TypeExpr::Unknown { variants, .. } => {
                let resolved_variants: Vec<NexusType> =
                    variants.iter().map(|v| self.resolve_type_expr(v)).collect();
                NexusType::Unknown(nexus_types::UnknownType {
                    variants: resolved_variants,
                })
            }
            TypeExpr::Function { .. } => {
                // For now, treat functions as generic function pointers
                NexusType::Function(nexus_types::FunctionType {
                    params: Vec::new(),
                    return_type: Box::new(NexusType::Primitive(PrimitiveType::Void)),
                    color: nexus_core::FunctionColor::Std,
                })
            }
        }
    }

    /// Register an interface definition
    fn register_interface(&mut self, def: &nexus_parser::InterfaceDefAst) -> NexusResult<()> {
        let interface_def = InterfaceDef::new(&def.name, def.span);
        // Methods are handled separately during type resolution
        self.type_registry.register_interface(interface_def);
        Ok(())
    }

    /// Run the program by calling the main function in the current module
    pub fn run(&mut self) -> NexusResult<Value> {
        self.run_with_args(Vec::new())
    }

    /// Run the program by calling the main function with arguments
    pub fn run_with_args(&mut self, args: Vec<Value>) -> NexusResult<Value> {
        // Look for main function in the current module only
        let qualified_name = format!("{}.main", self.current_module);
        let main_func = self.functions.get(&qualified_name).cloned();
        match main_func {
            Some(func) => self.call_function(&func, args, Span::dummy()),
            None => Err(NexusError::UndefinedFunction {
                name: format!("main (in module '{}')", self.current_module),
                span: Span::dummy(),
            }),
        }
    }

    /// Run a specific named function with the given arguments
    pub fn run_function(&mut self, name: &str, args: Vec<Value>) -> NexusResult<Value> {
        // Try direct lookup first (for already qualified names)
        // Returns (function, module_name)
        let result: Option<(FunctionDef, Option<String>)> = self
            .functions
            .get(name)
            .cloned()
            .map(|f| {
                // Extract module from qualified name
                let module = name.split('.').next().map(|s| s.to_string());
                (f, module)
            })
            .or_else(|| {
                // Try current module next
                let qualified = format!("{}.{}", self.current_module, name);
                self.functions
                    .get(&qualified)
                    .cloned()
                    .map(|f| (f, Some(self.current_module.clone())))
            })
            .or_else(|| {
                // Search for the function in any module (for tests across modules)
                self.functions
                    .iter()
                    .find(|(key, _)| key.ends_with(&format!(".{}", name)))
                    .map(|(key, func)| {
                        // Extract module name from the key
                        let module = key.split('.').next().map(|s| s.to_string());
                        (func.clone(), module)
                    })
            });
        match result {
            Some((func, module)) => {
                self.call_function_in_module(&func, args, Span::dummy(), module.as_deref())
            }
            None => Err(NexusError::UndefinedFunction {
                name: name.to_string(),
                span: Span::dummy(),
            }),
        }
    }

    /// Execute a block of statements, returning control flow
    fn execute_block_with_flow(
        &mut self,
        block: &Block,
        scope: &mut Scope,
    ) -> NexusResult<ControlFlow> {
        let mut result = Value::Void;
        let mut deferred: Vec<Block> = Vec::new();

        for stmt in &block.statements {
            // Check step limit for sandboxing
            if let Some(max) = self.config.max_steps {
                self.step_count += 1;
                if self.step_count > max {
                    return Err(NexusError::SandboxViolation {
                        message: "Maximum execution steps exceeded".to_string(),
                        span: Some(*stmt.span()),
                    });
                }
            }

            match self.execute_statement(stmt, scope, &mut deferred)? {
                ControlFlow::Continue(val) => result = val,
                ControlFlow::Return(val) => {
                    // Execute deferred blocks in reverse order
                    self.execute_deferred(&deferred, scope)?;
                    return Ok(ControlFlow::Return(val));
                }

                ControlFlow::Goto(label) => {
                    self.execute_deferred(&deferred, scope)?;
                    return Ok(ControlFlow::Goto(label));
                }
            }
        }

        // Execute deferred blocks in reverse order
        self.execute_deferred(&deferred, scope)?;

        Ok(ControlFlow::Continue(result))
    }

    /// Execute a block of statements (convenience wrapper that extracts value)
    pub fn execute_block(&mut self, block: &Block, scope: &mut Scope) -> NexusResult<Value> {
        match self.execute_block_with_flow(block, scope)? {
            ControlFlow::Continue(val) => Ok(val),
            ControlFlow::Return(val) => Ok(val),
            ControlFlow::Goto(label) => Err(NexusError::RuntimeError {
                message: format!("Goto to undefined label: {}", label),
                span: None,
            }),
        }
    }

    /// Execute deferred blocks
    fn execute_deferred(&mut self, deferred: &[Block], scope: &mut Scope) -> NexusResult<()> {
        for block in deferred.iter().rev() {
            // Create a snapshot scope for defer
            let mut defer_scope = scope.clone();
            self.execute_block(block, &mut defer_scope)?;
        }
        Ok(())
    }

    /// Execute a single statement
    fn execute_statement(
        &mut self,
        stmt: &Statement,
        scope: &mut Scope,
        deferred: &mut Vec<Block>,
    ) -> NexusResult<ControlFlow> {
        match stmt {
            Statement::VarDecl(decl) => {
                self.execute_var_decl(decl, scope)?;
                Ok(ControlFlow::Continue(Value::Void))
            }
            Statement::Assignment(assign) => {
                self.execute_assignment(assign, scope)?;
                Ok(ControlFlow::Continue(Value::Void))
            }
            Statement::Expression(expr) => {
                let val = self.evaluate_expression(expr, scope)?;
                Ok(ControlFlow::Continue(val))
            }
            Statement::Return(ret) => {
                let val = self.execute_return(ret, scope)?;
                Ok(ControlFlow::Return(val))
            }
            Statement::If(if_stmt) => self.execute_if(if_stmt, scope, deferred),
            Statement::Defer(defer) => {
                deferred.push(defer.body.clone());
                Ok(ControlFlow::Continue(Value::Void))
            }
            Statement::Subscope(subscope) => self.execute_subscope(subscope, scope),
            Statement::Goto(goto) => Ok(ControlFlow::Goto(goto.label.clone())),
            Statement::Block(block) => {
                let mut inner_scope = scope.child();
                self.execute_block_with_flow(block, &mut inner_scope)
            }
        }
    }

    /// Execute a variable declaration
    fn execute_var_decl(&mut self, decl: &VarDecl, scope: &mut Scope) -> NexusResult<()> {
        let value = self.evaluate_expression(&decl.init, scope)?;

        let var = Variable {
            name: decl.name.clone(),
            value,
            modifiers: decl.modifiers,
            span: decl.span,
        };

        // Global variables go to global scope
        if decl.modifiers.global {
            self.global_scope.define_unique(var)?;
        } else {
            scope.define_unique(var)?;
        }

        Ok(())
    }

    /// Execute an assignment
    fn execute_assignment(&mut self, assign: &Assignment, scope: &mut Scope) -> NexusResult<()> {
        let value = self.evaluate_expression(&assign.value, scope)?;

        match &assign.target {
            Expression::Variable(var_ref) => {
                // Check global scope first
                if self.global_scope.has(&var_ref.name) {
                    self.global_scope
                        .assign(&var_ref.name, value, var_ref.span)?;
                } else {
                    scope.assign(&var_ref.name, value, var_ref.span)?;
                }
            }
            Expression::FieldAccess(field) => {
                self.assign_field(field, value, scope)?;
            }
            Expression::Index(index) => {
                self.assign_index(index, value, scope)?;
            }
            _ => {
                return Err(NexusError::RuntimeError {
                    message: "Invalid assignment target".to_string(),
                    span: Some(assign.span),
                });
            }
        }

        Ok(())
    }

    /// Assign to a field
    fn assign_field(
        &mut self,
        field: &FieldAccessExpr,
        value: Value,
        scope: &mut Scope,
    ) -> NexusResult<()> {
        // Get the variable name from the object expression
        if let Expression::Variable(var_ref) = &*field.object {
            // Get the variable from scope
            let mut var =
                scope
                    .get(&var_ref.name)
                    .ok_or_else(|| NexusError::UndefinedVariable {
                        name: var_ref.name.clone(),
                        span: var_ref.span,
                    })?;

            // Check if it's a struct
            if let Value::Struct(instance) = &mut var.value {
                // Convert the value to FieldValue and set it
                let field_value = Self::value_to_field_value(&value);
                if !instance.set_field(&field.field, field_value) {
                    return Err(NexusError::RuntimeError {
                        message: format!("Unknown field: {}", field.field),
                        span: Some(field.span),
                    });
                }

                // Assign the modified struct back to the variable
                scope.assign(&var_ref.name, var.value, var_ref.span)?;
                return Ok(());
            } else {
                return Err(NexusError::TypeError {
                    message: "Cannot assign to field of non-struct value".to_string(),
                    span: field.span,
                });
            }
        }

        // For nested field access or more complex cases
        Err(NexusError::RuntimeError {
            message: "Nested field assignment not yet implemented".to_string(),
            span: Some(field.span),
        })
    }

    /// Assign to an array index
    fn assign_index(
        &mut self,
        index_expr: &IndexExpr,
        value: Value,
        scope: &mut Scope,
    ) -> NexusResult<()> {
        let index = self.evaluate_expression(&index_expr.index, scope)?;
        let idx = match index {
            Value::I64(i) => i,
            _ => {
                return Err(NexusError::TypeError {
                    message: "Array index must be an integer".to_string(),
                    span: index_expr.span,
                });
            }
        };

        // Get the array variable name
        if let Expression::Variable(var_ref) = &*index_expr.array {
            // Get the current array value
            let arr_value = scope
                .get_value(&var_ref.name)
                .or_else(|| self.global_scope.get_value(&var_ref.name));
            let arr_value = arr_value.ok_or_else(|| NexusError::UndefinedVariable {
                name: var_ref.name.clone(),
                span: var_ref.span,
            })?;

            if let Value::Array(mut arr) = arr_value {
                if !index_expr.unchecked
                    && self.config.bounds_checking
                    && (idx < 0 || idx as usize >= arr.len())
                {
                    return Err(NexusError::IndexOutOfBounds {
                        index: idx,
                        length: arr.len(),
                        span: index_expr.span,
                    });
                }
                arr[idx as usize] = value;

                // Assign the modified array back
                if self.global_scope.has(&var_ref.name) {
                    self.global_scope
                        .assign(&var_ref.name, Value::Array(arr), var_ref.span)?;
                } else {
                    scope.assign(&var_ref.name, Value::Array(arr), var_ref.span)?;
                }
                Ok(())
            } else {
                Err(NexusError::TypeError {
                    message: "Cannot index non-array value".to_string(),
                    span: index_expr.span,
                })
            }
        } else {
            Err(NexusError::RuntimeError {
                message: "Complex array assignment not yet implemented".to_string(),
                span: Some(index_expr.span),
            })
        }
    }

    /// Execute a return statement
    fn execute_return(&mut self, ret: &ReturnStmt, scope: &mut Scope) -> NexusResult<Value> {
        match &ret.value {
            Some(expr) => {
                // Returns inside subscopes cannot have values
                if self.subscope_depth > 0 {
                    return Err(NexusError::RuntimeError {
                        message: "Return inside a subscope cannot have a value".to_string(),
                        span: Some(ret.span),
                    });
                }
                self.evaluate_expression(expr, scope)
            }
            None => Ok(Value::Void),
        }
    }

    /// Execute an if statement
    fn execute_if(
        &mut self,
        if_stmt: &IfStmt,
        scope: &mut Scope,
        _deferred: &mut Vec<Block>,
    ) -> NexusResult<ControlFlow> {
        match &if_stmt.condition {
            IfCondition::Boolean(expr) => {
                let cond = self.evaluate_expression(expr, scope)?;
                if cond.is_truthy() {
                    let mut inner_scope = scope.child();
                    self.execute_block_with_flow(&if_stmt.then_block, &mut inner_scope)
                } else if let Some(else_clause) = &if_stmt.else_block {
                    match else_clause {
                        ElseClause::Block(block) => {
                            let mut inner_scope = scope.child();
                            self.execute_block_with_flow(block, &mut inner_scope)
                        }
                        ElseClause::ElseIf(else_if) => self.execute_if(else_if, scope, _deferred),
                    }
                } else {
                    Ok(ControlFlow::Continue(Value::Void))
                }
            }
            IfCondition::Pattern { matcher, cases } => {
                let match_value = self.evaluate_expression(matcher, scope)?;

                for case in cases {
                    if self.pattern_matches(&case.pattern, &match_value)? {
                        match &case.body {
                            PatternBody::Expression(expr) => {
                                let val = self.evaluate_expression(expr, scope)?;
                                return Ok(ControlFlow::Continue(val));
                            }
                            PatternBody::Block(block) => {
                                let mut inner_scope = scope.child();
                                return self.execute_block_with_flow(block, &mut inner_scope);
                            }
                        }
                    }
                }

                // No pattern matched, check else
                if let Some(else_clause) = &if_stmt.else_block {
                    match else_clause {
                        ElseClause::Block(block) => {
                            let mut inner_scope = scope.child();
                            self.execute_block_with_flow(block, &mut inner_scope)
                        }
                        ElseClause::ElseIf(else_if) => self.execute_if(else_if, scope, _deferred),
                    }
                } else {
                    Ok(ControlFlow::Continue(Value::Void))
                }
            }
        }
    }

    /// Check if a pattern matches a value
    fn pattern_matches(&self, pattern: &nexus_parser::Pattern, value: &Value) -> NexusResult<bool> {
        match pattern {
            nexus_parser::Pattern::Literal(lit) => {
                let lit_val = self.literal_to_value(lit)?;
                Ok(value.equals(&lit_val))
            }
            nexus_parser::Pattern::Wildcard(_) => Ok(true),
            nexus_parser::Pattern::Binding { .. } => {
                // Bindings always match and bind the value
                Ok(true)
            }
        }
    }

    /// Execute a subscope (labeled block that can be jumped to with goto)
    fn execute_subscope(
        &mut self,
        subscope: &SubscopeStmt,
        scope: &mut Scope,
    ) -> NexusResult<ControlFlow> {
        // Create a subscope-level scope that persists across goto jumps
        let mut body_scope = scope.child();
        body_scope.set_label(subscope.name.clone());

        // Track that we're in a subscope
        self.subscope_depth += 1;

        loop {
            let result = self.execute_block_with_flow(&subscope.body, &mut body_scope);
            match result {
                Err(e) => {
                    self.subscope_depth -= 1;
                    return Err(e);
                }
                Ok(ControlFlow::Continue(_)) => {
                    // Block completed normally without goto or return - this is an error
                    self.subscope_depth -= 1;
                    return Err(NexusError::RuntimeError {
                        message: format!(
                            "Subscope '{}' must end with 'goto ...' or 'return'",
                            subscope.name
                        ),
                        span: Some(subscope.span),
                    });
                }
                Ok(ControlFlow::Return(_)) => {
                    // Return in a subscope exits the subscope with void
                    self.subscope_depth -= 1;
                    return Ok(ControlFlow::Continue(Value::Void));
                }
                Ok(ControlFlow::Goto(label)) => {
                    // Check if this goto is for us
                    if label == subscope.name {
                        // Jump back to the beginning of this subscope
                        // Clear the scope so variables can be redefined in the new iteration
                        body_scope.clear();
                        continue;
                    }
                    // Goto for a different label, propagate up
                    self.subscope_depth -= 1;
                    return Ok(ControlFlow::Goto(label));
                }
            }
        }
    }

    /// Evaluate an expression
    pub fn evaluate_expression(
        &mut self,
        expr: &Expression,
        scope: &mut Scope,
    ) -> NexusResult<Value> {
        match expr {
            Expression::Literal(lit) => self.literal_to_value(lit),
            Expression::Variable(var_ref) => {
                // Check global scope first, then local
                if let Some(value) = self.global_scope.get_value(&var_ref.name) {
                    Ok(value)
                } else {
                    scope
                        .get_value(&var_ref.name)
                        .ok_or_else(|| NexusError::UndefinedVariable {
                            name: var_ref.name.clone(),
                            span: var_ref.span,
                        })
                }
            }
            Expression::Call(call) => self.evaluate_call(call, scope),
            Expression::MethodCall(call) => self.evaluate_method_call(call, scope),
            Expression::MacroCall(call) => self.evaluate_macro_call(call, scope),
            Expression::FieldAccess(field) => self.evaluate_field_access(field, scope),
            Expression::Index(index) => self.evaluate_index(index, scope),
            Expression::Array(arr) => self.evaluate_array(arr, scope),
            Expression::StructInit(init) => self.evaluate_struct_init(init, scope),
            Expression::Lambda(lambda) => self.evaluate_lambda(lambda, scope),
            Expression::Grouped(inner, _) => self.evaluate_expression(inner, scope),
        }
    }

    /// Convert a literal to a value
    fn literal_to_value(&self, lit: &Literal) -> NexusResult<Value> {
        Ok(match &lit.kind {
            LiteralKind::Int(n) => Value::I64(*n),
            LiteralKind::Float(n) => Value::F64(*n),
            LiteralKind::String(s) => Value::String(s.chars().collect()),
            LiteralKind::Char(c) => Value::Rune(*c),
            LiteralKind::Bool(b) => Value::Bool(*b),
            LiteralKind::None => Value::None,
        })
    }

    /// Evaluate a function call
    fn evaluate_call(&mut self, call: &CallExpr, scope: &mut Scope) -> NexusResult<Value> {
        // First check core builtins - get the function pointer before evaluating args
        if let Some(builtin) = self.builtins.get(&call.function) {
            let func = builtin.func;
            let mut args = Vec::new();
            for arg in &call.args {
                args.push(self.evaluate_expression(arg, scope)?);
            }
            return func(&args, call.span);
        }

        // Check compat.io builtins (require import)
        if let Some(builtin) = self.builtins.get_compat_io(&call.function) {
            // Check if the symbol was imported
            if !self.imported_symbols.contains_key(&call.function) {
                return Err(NexusError::RuntimeError {
                    message: format!(
                        "'{}' requires import: use {{ {} }} from compat.io",
                        call.function, call.function
                    ),
                    span: Some(call.span),
                });
            }
            // Check color permissions - compat.io builtins are compat color
            self.permissions.check_color_call(
                &self.current_module,
                self.current_color,
                FunctionColor::Compat,
                &call.function,
                call.span,
            )?;
            let func = builtin.func;
            let mut args = Vec::new();
            for arg in &call.args {
                args.push(self.evaluate_expression(arg, scope)?);
            }
            return func(&args, call.span);
        }

        // Check compat.fs builtins (require import)
        if let Some(builtin) = self.builtins.get_compat_fs(&call.function) {
            // Check if the symbol was imported
            if !self.imported_symbols.contains_key(&call.function) {
                return Err(NexusError::RuntimeError {
                    message: format!(
                        "'{}' requires import: use {{ {} }} from compat.fs",
                        call.function, call.function
                    ),
                    span: Some(call.span),
                });
            }
            // Check color permissions - compat.fs builtins are compat color
            self.permissions.check_color_call(
                &self.current_module,
                self.current_color,
                FunctionColor::Compat,
                &call.function,
                call.span,
            )?;
            let func = builtin.func;
            let mut args = Vec::new();
            for arg in &call.args {
                args.push(self.evaluate_expression(arg, scope)?);
            }
            return func(&args, call.span);
        }

        // Check compat.proc builtins (require import)
        if let Some(builtin) = self.builtins.get_compat_proc(&call.function) {
            // Check if the symbol was imported
            if !self.imported_symbols.contains_key(&call.function) {
                return Err(NexusError::RuntimeError {
                    message: format!(
                        "'{}' requires import: use {{ {} }} from compat.proc",
                        call.function, call.function
                    ),
                    span: Some(call.span),
                });
            }
            // Check color permissions - compat.proc builtins are compat color
            self.permissions.check_color_call(
                &self.current_module,
                self.current_color,
                FunctionColor::Compat,
                &call.function,
                call.span,
            )?;

            // Special handling for getargs - it needs access to interpreter state
            if call.function == "getargs" {
                // Return the stored program arguments as an array of rune arrays
                return Ok(Value::Array(self.program_args.clone()));
            }

            // For other compat.proc builtins, call the function normally
            let func = builtin.func;
            let mut args = Vec::new();
            for arg in &call.args {
                args.push(self.evaluate_expression(arg, scope)?);
            }
            return func(&args, call.span);
        }

        // Check plat.console builtins (require import)
        if let Some(builtin) = self.builtins.get_plat_console(&call.function) {
            // Check if the symbol was imported
            if !self.imported_symbols.contains_key(&call.function) {
                return Err(NexusError::RuntimeError {
                    message: format!(
                        "'{}' requires import: use {{ {} }} from plat.console",
                        call.function, call.function
                    ),
                    span: Some(call.span),
                });
            }
            // Check color permissions - plat.console builtins are plat color
            self.permissions.check_color_call(
                &self.current_module,
                self.current_color,
                FunctionColor::Plat,
                &call.function,
                call.span,
            )?;
            let func = builtin.func;
            let mut args = Vec::new();
            for arg in &call.args {
                args.push(self.evaluate_expression(arg, scope)?);
            }
            return func(&args, call.span);
        }

        // Determine the qualified function name to look up
        // First check if this is an imported symbol
        let qualified_name = if let Some(source_module) = self.imported_symbols.get(&call.function)
        {
            format!("{}.{}", source_module, call.function)
        } else {
            // Otherwise, look in the current module
            format!("{}.{}", self.current_module, call.function)
        };

        // Then check user-defined functions
        if let Some(func) = self.functions.get(&qualified_name).cloned() {
            // Check color permissions
            self.permissions.check_color_call(
                &self.current_module,
                self.current_color,
                func.color,
                &func.name,
                call.span,
            )?;

            // Track mutable parameter variable names for copy-out semantics
            let mut mutable_args: Vec<(String, String)> = Vec::new(); // (param_name, caller_var_name)
            let mut args = Vec::new();
            for (i, arg) in call.args.iter().enumerate() {
                let value = self.evaluate_expression(arg, scope)?;
                args.push(value);

                // Check if this parameter is mutable and the arg is a variable reference
                if i < func.params.len()
                    && func.params[i].mutable
                    && let Expression::Variable(var_ref) = arg
                {
                    mutable_args.push((func.params[i].name.clone(), var_ref.name.clone()));
                }
            }
            // Extract the module from the qualified name (everything except the last component)
            let module = qualified_name
                .rfind('.')
                .map(|last_dot| &qualified_name[..last_dot]);
            let (result, final_scope) =
                self.call_function_in_module_with_scope(&func, args, call.span, module)?;

            // Copy-out: write back mutable parameter values to caller's variables
            for (param_name, caller_var_name) in mutable_args {
                if let Some(value) = final_scope.get_value(&param_name) {
                    scope.assign(&caller_var_name, value, call.span)?;
                }
            }

            return Ok(result);
        }

        Err(NexusError::UndefinedFunction {
            name: call.function.clone(),
            span: call.span,
        })
    }

    /// Call a function with arguments, optionally in a specific module context
    /// Returns the result and the final function scope (for mutable parameter writeback)
    fn call_function_in_module_with_scope(
        &mut self,
        func: &FunctionDef,
        args: Vec<Value>,
        span: Span,
        module: Option<&str>,
    ) -> NexusResult<(Value, Scope)> {
        // Check recursion depth
        self.recursion_depth += 1;
        if self.recursion_depth > self.config.max_recursion_depth {
            self.recursion_depth -= 1;
            return Err(NexusError::RuntimeError {
                message: "Maximum recursion depth exceeded".to_string(),
                span: Some(span),
            });
        }

        // Save current state
        let prev_color = self.current_color;
        let prev_module = self.current_module.clone();
        let prev_subscope_depth = self.subscope_depth;

        // Set new state
        self.current_color = func.color;
        if let Some(mod_name) = module {
            self.current_module = mod_name.to_string();
        }
        self.subscope_depth = 0; // New function call starts fresh

        // Create function scope with parameters
        let mut func_scope = Scope::new();
        for (param, arg) in func.params.iter().zip(args.into_iter()) {
            func_scope.define(Variable {
                name: param.name.clone(),
                value: arg,
                modifiers: VarModifiers {
                    mutable: param.mutable,
                    ..VarModifiers::default()
                },
                span: param.span,
            })?;
        }

        // Execute function body
        let result = self.execute_block(&func.body, &mut func_scope);

        // Restore state
        self.current_color = prev_color;
        self.current_module = prev_module;
        self.subscope_depth = prev_subscope_depth;
        self.recursion_depth -= 1;

        result.map(|v| (v, func_scope))
    }

    /// Call a function with arguments, optionally in a specific module context
    fn call_function_in_module(
        &mut self,
        func: &FunctionDef,
        args: Vec<Value>,
        span: Span,
        module: Option<&str>,
    ) -> NexusResult<Value> {
        self.call_function_in_module_with_scope(func, args, span, module)
            .map(|(v, _)| v)
    }

    /// Call a function with arguments (uses current module context)
    fn call_function(
        &mut self,
        func: &FunctionDef,
        args: Vec<Value>,
        span: Span,
    ) -> NexusResult<Value> {
        self.call_function_in_module(func, args, span, None)
    }

    /// Evaluate a method call
    fn evaluate_method_call(
        &mut self,
        call: &MethodCallExpr,
        scope: &mut Scope,
    ) -> NexusResult<Value> {
        // Check if this is a module-qualified function call (e.g., mymodule.myfunc())
        if let Expression::Variable(var_ref) = call.receiver.as_ref() {
            let module_name = &var_ref.name;

            // Check if this is an imported module
            if self.imported_modules.contains(module_name) {
                // Verify the function exists in the module
                let function_exists = self
                    .module_functions
                    .get(module_name)
                    .map(|funcs| funcs.contains(&call.method))
                    .unwrap_or(false);

                if !function_exists {
                    return Err(NexusError::UndefinedFunction {
                        name: format!("{}.{}", module_name, call.method),
                        span: call.span,
                    });
                }

                // Look up the function by its module-qualified name
                let qualified_name = format!("{}.{}", module_name, call.method);
                if let Some(func) = self.functions.get(&qualified_name).cloned() {
                    // Check color permissions
                    self.permissions.check_color_call(
                        &self.current_module,
                        self.current_color,
                        func.color,
                        &func.name,
                        call.span,
                    )?;

                    let mut args = Vec::new();
                    for arg in &call.args {
                        args.push(self.evaluate_expression(arg, scope)?);
                    }
                    // Call the function in its module context
                    return self.call_function_in_module(&func, args, call.span, Some(module_name));
                }

                return Err(NexusError::UndefinedFunction {
                    name: format!("{}.{}", module_name, call.method),
                    span: call.span,
                });
            }
        }

        let receiver = self.evaluate_expression(&call.receiver, scope)?;

        // Get the receiver's type name
        let type_name = receiver.type_name();
        let method_key = format!("{}.{}", type_name, call.method);

        if let Some(method) = self.methods.get(&method_key).cloned() {
            let mut args = vec![receiver];
            for arg in &call.args {
                args.push(self.evaluate_expression(arg, scope)?);
            }
            return self.call_method(&method, args, call.span);
        }

        Err(NexusError::UndefinedFunction {
            name: format!("{}.{}", type_name, call.method),
            span: call.span,
        })
    }

    /// Call a method with receiver and arguments
    fn call_method(
        &mut self,
        method: &MethodDef,
        args: Vec<Value>,
        span: Span,
    ) -> NexusResult<Value> {
        // Similar to call_function but with receiver
        self.recursion_depth += 1;
        if self.recursion_depth > self.config.max_recursion_depth {
            self.recursion_depth -= 1;
            return Err(NexusError::RuntimeError {
                message: "Maximum recursion depth exceeded".to_string(),
                span: Some(span),
            });
        }

        let prev_color = self.current_color;
        let prev_subscope_depth = self.subscope_depth;
        self.current_color = method.color;
        self.subscope_depth = 0; // New method call starts fresh

        let mut method_scope = Scope::new();

        // First argument is the receiver
        if let Some(receiver) = args.first() {
            method_scope.define(Variable {
                name: method.receiver_name.clone(),
                value: receiver.clone(),
                modifiers: if method.receiver_mutable {
                    VarModifiers {
                        mutable: true,
                        ..Default::default()
                    }
                } else {
                    VarModifiers::default()
                },
                span: method.span,
            })?;
        }

        // Remaining arguments
        for (param, arg) in method.params.iter().zip(args.into_iter().skip(1)) {
            method_scope.define(Variable {
                name: param.name.clone(),
                value: arg,
                modifiers: VarModifiers {
                    mutable: param.mutable,
                    ..VarModifiers::default()
                },
                span: param.span,
            })?;
        }

        let result = self.execute_block(&method.body, &mut method_scope);

        self.current_color = prev_color;
        self.subscope_depth = prev_subscope_depth;
        self.recursion_depth -= 1;

        result
    }

    /// Evaluate a macro call
    fn evaluate_macro_call(
        &mut self,
        call: &MacroCallExpr,
        scope: &mut Scope,
    ) -> NexusResult<Value> {
        // Determine the qualified macro name to look up
        // First check if this is an imported symbol (no $ prefix in import)
        let qualified_name = if let Some(source_module) = self.imported_symbols.get(&call.name) {
            format!("{}.{}", source_module, call.name)
        } else {
            // Try current module
            format!("{}.{}", self.current_module, call.name)
        };

        // Look up the macro with qualified name
        if let Some(macro_def) = self.macros.get(&qualified_name).cloned() {
            // Get the source module for this macro to use its imports during expansion
            let macro_source_module = self
                .macro_source_modules
                .get(&qualified_name)
                .cloned()
                .unwrap_or_else(|| self.current_module.clone());
            return self.expand_and_evaluate_macro(
                &macro_def,
                &call.args,
                scope,
                call.span,
                &macro_source_module,
            );
        }

        // If not found with qualified name, check if it requires an import
        // by looking in module_macros to give a helpful error
        for (module, macros) in &self.module_macros {
            if macros.contains(&call.name) {
                return Err(NexusError::RuntimeError {
                    message: format!(
                        "'${}' requires import: use {{ {} }} from {}",
                        call.name, call.name, module
                    ),
                    span: Some(call.span),
                });
            }
        }

        Err(NexusError::UndefinedFunction {
            name: format!("${}", call.name),
            span: call.span,
        })
    }

    /// Expand a user-defined macro and evaluate the result
    fn expand_and_evaluate_macro(
        &mut self,
        macro_def: &nexus_parser::MacroDef,
        args: &[Expression],
        scope: &mut Scope,
        span: Span,
        macro_source_module: &str,
    ) -> NexusResult<Value> {
        // Temporarily switch to the macro's source module context for import resolution
        let prev_module = self.current_module.clone();
        let prev_imported_symbols = self.imported_symbols.clone();

        // Use the macro's defining module's imports
        self.current_module = macro_source_module.to_string();
        if let Some(module_imports) = self.module_imported_symbols.get(macro_source_module) {
            // Merge the macro's module imports with current imports
            for (symbol, source) in module_imports {
                self.imported_symbols.insert(symbol.clone(), source.clone());
            }
        }

        // Create macro scope with parameters
        let mut macro_scope = Scope::new();

        // Bind macro arguments to parameters
        for (param, arg) in macro_def.params.iter().zip(args.iter()) {
            let value = self.evaluate_expression(arg, scope)?;
            macro_scope.define(Variable {
                name: param.name.clone(),
                value,
                modifiers: VarModifiers {
                    mutable: param.mutable,
                    ..VarModifiers::default()
                },
                span: param.span,
            })?;
        }

        // Execute macro body to get the code string
        let macro_result = self.execute_block(&macro_def.body, &mut macro_scope)?;

        // The macro must return a string
        let code_string = match macro_result {
            Value::String(s) => s.into_iter().collect::<String>(),
            _ => {
                return Err(NexusError::TypeError {
                    message: "Macro must return a string (macro type)".to_string(),
                    span: macro_def.span,
                });
            }
        };

        // Parse the generated code as an expression
        let parsed_expr =
            nexus_parser::parse_expression(&code_string).map_err(|e| NexusError::RuntimeError {
                message: format!("Failed to parse macro expansion: {}", e),
                span: Some(span),
            })?;

        // Evaluate the parsed expression in the original scope
        let result = self.evaluate_expression(&parsed_expr, scope);

        // Restore previous module context
        self.current_module = prev_module;
        self.imported_symbols = prev_imported_symbols;

        result
    }

    /// Evaluate field access
    fn evaluate_field_access(
        &mut self,
        field: &FieldAccessExpr,
        scope: &mut Scope,
    ) -> NexusResult<Value> {
        let object = self.evaluate_expression(&field.object, scope)?;

        match object {
            Value::Struct(instance) => {
                if let Some(field_val) = instance.get_field(&field.field) {
                    Ok(Self::field_value_to_value(&field_val.value))
                } else {
                    Err(NexusError::RuntimeError {
                        message: format!("Unknown field: {}", field.field),
                        span: Some(field.span),
                    })
                }
            }
            _ => Err(NexusError::TypeError {
                message: "Cannot access field on non-struct value".to_string(),
                span: field.span,
            }),
        }
    }

    /// Evaluate index access
    fn evaluate_index(&mut self, index: &IndexExpr, scope: &mut Scope) -> NexusResult<Value> {
        let array = self.evaluate_expression(&index.array, scope)?;
        let idx_val = self.evaluate_expression(&index.index, scope)?;

        let idx = match idx_val {
            Value::I64(i) => i,
            _ => {
                return Err(NexusError::TypeError {
                    message: "Array index must be an integer".to_string(),
                    span: index.span,
                });
            }
        };

        match array {
            Value::Array(arr) => {
                if !index.unchecked
                    && self.config.bounds_checking
                    && (idx < 0 || idx as usize >= arr.len())
                {
                    return Err(NexusError::IndexOutOfBounds {
                        index: idx,
                        length: arr.len(),
                        span: index.span,
                    });
                }
                Ok(arr.get(idx as usize).cloned().unwrap_or(Value::None))
            }
            Value::String(s) => {
                if !index.unchecked
                    && self.config.bounds_checking
                    && (idx < 0 || idx as usize >= s.len())
                {
                    return Err(NexusError::IndexOutOfBounds {
                        index: idx,
                        length: s.len(),
                        span: index.span,
                    });
                }
                Ok(s.get(idx as usize)
                    .map(|&c| Value::Rune(c))
                    .unwrap_or(Value::None))
            }
            Value::Bytes(b) => {
                if !index.unchecked
                    && self.config.bounds_checking
                    && (idx < 0 || idx as usize >= b.len())
                {
                    return Err(NexusError::IndexOutOfBounds {
                        index: idx,
                        length: b.len(),
                        span: index.span,
                    });
                }
                Ok(b.get(idx as usize)
                    .map(|&v| Value::I64(v as i64))
                    .unwrap_or(Value::None))
            }
            _ => Err(NexusError::TypeError {
                message: "Cannot index non-array value".to_string(),
                span: index.span,
            }),
        }
    }

    /// Evaluate array literal
    fn evaluate_array(&mut self, arr: &ArrayExpr, scope: &mut Scope) -> NexusResult<Value> {
        let mut elements = Vec::new();
        for elem in &arr.elements {
            elements.push(self.evaluate_expression(elem, scope)?);
        }
        Ok(Value::Array(elements))
    }

    /// Evaluate struct initialization
    fn evaluate_struct_init(
        &mut self,
        init: &StructInitExpr,
        scope: &mut Scope,
    ) -> NexusResult<Value> {
        let struct_def =
            self.type_registry
                .get_struct(&init.name)
                .ok_or_else(|| NexusError::UnknownType {
                    name: init.name.clone(),
                    span: init.span,
                })?;

        let mut instance = nexus_types::StructInstance::new(struct_def.clone());

        // Set explicitly initialized fields
        for field_init in &init.fields {
            let value = self.evaluate_expression(&field_init.value, scope)?;
            let field_value = Self::value_to_field_value(&value);
            instance.set_field(&field_init.name, field_value);
        }

        // Evaluate default values for uninitialized fields
        if let Some(struct_ast) = self.struct_defs.get(&init.name).cloned() {
            for (i, field_ast) in struct_ast.fields.iter().enumerate() {
                if let Some(field_val) = instance.values.get(i)
                    && !field_val.is_set
                {
                    // Field was not explicitly set, try to use default
                    if let Some(default_expr) = &field_ast.default {
                        let default_value = self.evaluate_expression(default_expr, scope)?;
                        let field_value = Self::value_to_field_value(&default_value);
                        instance.set_field(&field_ast.name, field_value);
                    }
                }
            }
        }

        Ok(Value::Struct(Box::new(instance)))
    }

    /// Convert Value to FieldValue
    fn value_to_field_value(value: &Value) -> nexus_types::FieldValue {
        match value {
            Value::I64(n) => nexus_types::FieldValue::I64(*n),
            Value::F64(n) => nexus_types::FieldValue::F64(*n),
            Value::Bool(b) => nexus_types::FieldValue::Bool(*b),
            Value::Rune(c) => nexus_types::FieldValue::Rune(*c),
            Value::String(s) => nexus_types::FieldValue::String(s.clone()),
            Value::Bytes(b) => nexus_types::FieldValue::Bytes(b.clone()),
            Value::None => nexus_types::FieldValue::None,
            Value::Array(arr) => {
                nexus_types::FieldValue::Array(arr.iter().map(Self::value_to_field_value).collect())
            }
            Value::Struct(s) => nexus_types::FieldValue::Struct(s.clone()),
            _ => nexus_types::FieldValue::Uninitialized,
        }
    }

    /// Convert FieldValue to Value
    fn field_value_to_value(field_value: &nexus_types::FieldValue) -> Value {
        match field_value {
            nexus_types::FieldValue::I64(n) => Value::I64(*n),
            nexus_types::FieldValue::F64(n) => Value::F64(*n),
            nexus_types::FieldValue::Bool(b) => Value::Bool(*b),
            nexus_types::FieldValue::Rune(c) => Value::Rune(*c),
            nexus_types::FieldValue::String(s) => Value::String(s.clone()),
            nexus_types::FieldValue::Bytes(b) => Value::Bytes(b.clone()),
            nexus_types::FieldValue::None => Value::None,
            nexus_types::FieldValue::Uninitialized => Value::None,
            nexus_types::FieldValue::Array(arr) => {
                Value::Array(arr.iter().map(Self::field_value_to_value).collect())
            }
            nexus_types::FieldValue::Struct(s) => Value::Struct(s.clone()),
        }
    }

    /// Evaluate lambda expression
    fn evaluate_lambda(&mut self, lambda: &LambdaExpr, scope: &mut Scope) -> NexusResult<Value> {
        // Capture variables from current scope
        let mut captured = FxHashMap::default();
        for capture_name in &lambda.captures {
            if let Some(value) = scope.get_value(capture_name) {
                captured.insert(capture_name.clone(), value);
            }
        }

        Ok(Value::Lambda(Box::new(LambdaValue {
            params: lambda.params.clone(),
            body: lambda.body.clone(),
            captured,
        })))
    }

    /// Get the permission manager
    pub fn permissions(&self) -> &PermissionManager {
        &self.permissions
    }

    /// Get mutable permission manager
    pub fn permissions_mut(&mut self) -> &mut PermissionManager {
        &mut self.permissions
    }

    /// Set the current module
    pub fn set_current_module(&mut self, module: impl Into<String>) {
        self.current_module = module.into();
    }

    /// Set program arguments (for compat.proc.getargs)
    pub fn set_args(&mut self, args: Vec<Value>) {
        self.program_args = args;
    }

    /// Get program arguments (for compat.proc.getargs)
    pub fn get_args(&self) -> &[Value] {
        &self.program_args
    }

    /// Register a known module (dependency)
    /// Returns an error if the module is already registered
    pub fn register_module(&mut self, module_name: impl Into<String>) -> NexusResult<()> {
        let name = module_name.into();
        if self.known_modules.contains(&name) {
            return Err(NexusError::ModuleAlreadyDefined { name });
        }
        self.known_modules.insert(name);
        Ok(())
    }

    /// Register a function as belonging to a module
    pub fn register_module_function(&mut self, module_name: &str, function_name: &str) {
        self.module_functions
            .entry(module_name.to_string())
            .or_default()
            .insert(function_name.to_string());
    }

    /// Register a struct as belonging to a module
    pub fn register_module_struct(&mut self, module_name: &str, struct_name: &str) {
        self.module_structs
            .entry(module_name.to_string())
            .or_default()
            .insert(struct_name.to_string());
    }

    /// Register a macro as belonging to a module (for import checking)
    pub fn register_module_macro(&mut self, module_name: &str, macro_name: &str) {
        self.module_macros
            .entry(module_name.to_string())
            .or_default()
            .insert(macro_name.to_string());
    }

    /// Register a macro directly by name (for sandboxed macro expansion)
    /// This allows nested macro calls to be resolved during macro execution
    pub fn register_macro_direct(&mut self, name: &str, macro_def: nexus_parser::MacroDef) {
        self.macros.insert(name.to_string(), macro_def.clone());
        // Also add to imported_symbols so it can be found during macro expansion
        self.imported_symbols
            .insert(macro_def.name.clone(), String::new());
    }

    /// Reset step counter (for sandboxing)
    pub fn reset_steps(&mut self) {
        self.step_count = 0;
    }

    /// Get current step count
    pub fn step_count(&self) -> usize {
        self.step_count
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

/// Control flow result from statement execution
#[derive(Debug)]
enum ControlFlow {
    /// Continue normal execution with a value
    Continue(Value),
    /// Return from function with a value
    Return(Value),
    /// Goto a labeled subscope
    Goto(String),
}

/// A captured lambda value
#[derive(Debug, Clone)]
pub struct LambdaValue {
    pub params: Vec<nexus_parser::Parameter>,
    pub body: LambdaBody,
    pub captured: FxHashMap<String, Value>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_interpreter_creation() {
        let interp = Interpreter::new();
        assert!(interp.functions.is_empty());
    }

    #[test]
    fn test_config_defaults() {
        let config = InterpreterConfig::default();
        assert!(config.bounds_checking);
        assert_eq!(config.max_recursion_depth, 1000);
        assert!(config.max_steps.is_none());
    }

    #[test]
    fn test_permission_denied_for_compat_io_without_permission() {
        use nexus_parser::parse;

        let source = r#"
            use { println } from compat.io
            compat main(): void {
                println(42)
            }
        "#;

        let program = parse(source).unwrap();
        let mut interpreter = Interpreter::new();

        // Set up a module without compat.io permission
        interpreter.set_current_module("restricted_module");
        interpreter
            .permissions_mut()
            .set_module_permissions("restricted_module", PermissionSet::std_only());

        // Loading the program should fail because of the use statement
        let result = interpreter.load_program(&program);
        assert!(result.is_err());

        let err = result.unwrap_err();
        match err {
            NexusError::PermissionDenied {
                permission,
                context,
                ..
            } => {
                assert_eq!(permission, "compat.io");
                assert_eq!(context, "restricted_module");
            }
            _ => panic!("Expected PermissionDenied error, got: {:?}", err),
        }
    }

    #[test]
    fn test_permission_allowed_for_compat_io_with_permission() {
        use nexus_parser::parse;

        let source = r#"
            use { println } from compat.io
            compat main(): void {
                println(42)
            }
        "#;

        let program = parse(source).unwrap();
        let mut interpreter = Interpreter::new();

        // Set up a module with compat.io permission
        let mut perms = PermissionSet::new();
        perms.allow(Permission::Compat(CompatPermission::Io));
        interpreter.set_current_module("permitted_module");
        interpreter
            .permissions_mut()
            .set_module_permissions("permitted_module", perms);

        // Loading the program should succeed
        let result = interpreter.load_program(&program);
        assert!(result.is_ok());
    }
}

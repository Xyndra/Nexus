//! Macro expansion for LSP hover support.
//!
//! This module provides functionality to expand macros at compile-time
//! for displaying the generated code in hover tooltips.

use std::collections::HashMap;

use nexus_core::{NexusResult, VarModifiers};
use nexus_interpreter::{Interpreter, InterpreterConfig, Scope, Value, Variable};
use nexus_parser::{Expression, Item, Literal, LiteralKind, MacroDef, Program, UseStatement};

/// Result of attempting to expand a macro
#[derive(Debug, Clone)]
pub enum MacroExpansionResult {
    /// Successfully expanded the macro
    Success {
        /// The generated code string
        code: String,
    },
    /// Macro found but arguments cannot be evaluated at compile-time
    RuntimeOnly {
        /// Explanation of why expansion failed
        reason: String,
    },
    /// Macro definition not found
    NotFound,
    /// Error during expansion
    Error {
        /// Error message
        message: String,
    },
}

/// Context for macro expansion containing macro definitions
pub struct MacroExpansionContext {
    /// Macro definitions by short name (name -> MacroDef)
    macros: HashMap<String, MacroDef>,
    /// Macro definitions by qualified name (module.name -> MacroDef)
    qualified_macros: HashMap<String, MacroDef>,
    /// Module that each macro was defined in (short_name -> module_name)
    macro_modules: HashMap<String, String>,
    /// Per-module imports (module_name -> (symbol_name -> source_module))
    module_imports: HashMap<String, HashMap<String, String>>,
}

impl MacroExpansionContext {
    /// Create a new empty context
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
            qualified_macros: HashMap::new(),
            macro_modules: HashMap::new(),
            module_imports: HashMap::new(),
        }
    }

    /// Build a context from multiple programs (for cross-file support)
    /// This version assumes no module names - macros registered by short name only
    pub fn from_programs<'a>(programs: impl IntoIterator<Item = &'a Program>) -> Self {
        let mut ctx = Self::new();
        for program in programs {
            for item in &program.items {
                if let Item::Macro(macro_def) = item {
                    ctx.macros.insert(macro_def.name.clone(), macro_def.clone());
                }
            }
        }
        ctx
    }

    /// Build a context from multiple programs with module names
    /// This allows proper import resolution during macro expansion
    pub fn from_programs_with_modules<'a>(
        programs: impl IntoIterator<Item = (&'a str, &'a Program)>,
    ) -> Self {
        let mut ctx = Self::new();
        for (module_name, program) in programs {
            // Extract use statements from this module
            let mut module_imports: HashMap<String, String> = HashMap::new();
            for item in &program.items {
                if let Item::Use(use_stmt) = item {
                    Self::extract_imports_from_use(&mut module_imports, use_stmt);
                }
            }
            if !module_imports.is_empty() {
                ctx.module_imports
                    .insert(module_name.to_string(), module_imports);
            }

            // Register macros with module context
            for item in &program.items {
                if let Item::Macro(macro_def) = item {
                    let short_name = macro_def.name.clone();
                    let qualified_name = format!("{}.{}", module_name, short_name);

                    ctx.macros.insert(short_name.clone(), macro_def.clone());
                    ctx.qualified_macros
                        .insert(qualified_name, macro_def.clone());
                    ctx.macro_modules
                        .insert(short_name, module_name.to_string());
                }
            }
        }
        ctx
    }

    /// Extract imports from a use statement
    fn extract_imports_from_use(imports: &mut HashMap<String, String>, use_stmt: &UseStatement) {
        // Join the module path components with dots (e.g., ["std", "util", "strings"] -> "std.util.strings")
        let module_path = use_stmt.module_path.join(".");
        for symbol in &use_stmt.symbols {
            imports.insert(symbol.clone(), module_path.clone());
        }
    }

    /// Get a macro definition by name
    pub fn get_macro(&self, name: &str) -> Option<&MacroDef> {
        self.macros.get(name)
    }

    /// Attempt to expand a macro call with the given arguments
    pub fn expand_macro(&self, name: &str, args: &[Expression]) -> MacroExpansionResult {
        self.expand_macro_with_depth(name, args, 0)
    }

    /// Attempt to expand a macro call with the given arguments, tracking recursion depth
    fn expand_macro_with_depth(
        &self,
        name: &str,
        args: &[Expression],
        depth: usize,
    ) -> MacroExpansionResult {
        // Prevent infinite recursion
        const MAX_RECURSION_DEPTH: usize = 32;
        if depth > MAX_RECURSION_DEPTH {
            return MacroExpansionResult::Error {
                message: format!(
                    "Maximum macro recursion depth ({}) exceeded",
                    MAX_RECURSION_DEPTH
                ),
            };
        }

        // Try to find the macro - first by qualified name, then by short name
        let (macro_def, macro_source_module) = if let Some(def) = self.qualified_macros.get(name) {
            // Extract module from qualified name
            let module = name.rfind('.').map(|i| &name[..i]).unwrap_or("");
            (def.clone(), Some(module.to_string()))
        } else if let Some(def) = self.macros.get(name) {
            let module = self.macro_modules.get(name).cloned();
            (def.clone(), module)
        } else {
            return MacroExpansionResult::NotFound;
        };

        // Try to convert arguments to compile-time values
        let mut arg_values = Vec::new();
        for arg in args {
            match try_evaluate_compile_time(arg) {
                Some(value) => arg_values.push(value),
                None => {
                    return MacroExpansionResult::RuntimeOnly {
                        reason:
                            "Macro arguments contain expressions that require runtime evaluation"
                                .to_string(),
                    };
                }
            }
        }

        // Check argument count
        if arg_values.len() != macro_def.params.len() {
            return MacroExpansionResult::Error {
                message: format!(
                    "Macro '{}' expects {} arguments, but {} were provided",
                    name,
                    macro_def.params.len(),
                    arg_values.len()
                ),
            };
        }

        // Execute the macro in a sandboxed interpreter, passing all macros for nested calls
        match execute_macro(
            &macro_def,
            &arg_values,
            &self.macros,
            &self.qualified_macros,
            &self.macro_modules,
            &self.module_imports,
            macro_source_module.as_deref(),
        ) {
            Ok(code) => {
                // Check if the expanded code contains nested top-level macro calls
                self.expand_nested_macros(&code, depth)
            }
            Err(e) => MacroExpansionResult::Error {
                message: format!("Macro expansion error: {}", e),
            },
        }
    }

    /// Expand any nested top-level macro calls in the generated code
    fn expand_nested_macros(&self, code: &str, depth: usize) -> MacroExpansionResult {
        // Try to parse the generated code as items
        // If parsing fails, the result might be an expression (for inline macros), not items
        // In that case, just return the code as-is
        let items = match nexus_parser::parse_items(code) {
            Ok(items) => items,
            Err(_) => {
                // Not valid item syntax - likely an expression result from an inline macro
                // Return the code as-is without nested expansion
                return MacroExpansionResult::Success {
                    code: code.to_string(),
                };
            }
        };

        // Check if any items are top-level macro calls
        let mut has_nested_macros = false;
        for item in &items {
            if matches!(item, Item::TopLevelMacroCall(_)) {
                has_nested_macros = true;
                break;
            }
        }

        // If no nested macros, return the original code
        if !has_nested_macros {
            return MacroExpansionResult::Success {
                code: code.to_string(),
            };
        }

        // Expand nested macros
        let mut expanded_code = String::new();
        for item in items {
            match item {
                Item::TopLevelMacroCall(macro_call) => {
                    // Recursively expand this macro
                    match self.expand_macro_with_depth(
                        &macro_call.name,
                        &macro_call.args,
                        depth + 1,
                    ) {
                        MacroExpansionResult::Success { code: nested_code } => {
                            expanded_code.push_str(&nested_code);
                            expanded_code.push('\n');
                        }
                        MacroExpansionResult::NotFound => {
                            return MacroExpansionResult::Error {
                                message: format!("Nested macro '{}' not found", macro_call.name),
                            };
                        }
                        MacroExpansionResult::RuntimeOnly { reason } => {
                            return MacroExpansionResult::RuntimeOnly { reason };
                        }
                        MacroExpansionResult::Error { message } => {
                            return MacroExpansionResult::Error { message };
                        }
                    }
                }
                _ => {
                    // Re-serialize non-macro items back to code
                    // For simplicity, we'll format them manually based on type
                    if let Some(item_str) = self.item_to_string(&item) {
                        expanded_code.push_str(&item_str);
                        expanded_code.push('\n');
                    }
                }
            }
        }

        MacroExpansionResult::Success {
            code: expanded_code,
        }
    }

    /// Convert an AST item back to source code (simplified serialization)
    fn item_to_string(&self, _item: &Item) -> Option<String> {
        // For non-macro items, we need to re-extract them from the original code
        // Since we don't have the original source position, we'll return None
        // and let the caller handle it by keeping the original code section
        //
        // In practice, macro-generated code that contains nested macros
        // should be re-parsed and the non-macro items can be represented
        // by their span in the original generated code
        //
        // For now, we'll just note that this is a limitation -
        // nested macros should produce complete items without mixing
        // macro calls with regular items in the same expansion
        None
    }
}

impl Default for MacroExpansionContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Try to evaluate an expression at compile-time.
/// Returns None if the expression requires runtime evaluation.
fn try_evaluate_compile_time(expr: &Expression) -> Option<Value> {
    match expr {
        Expression::Literal(lit) => Some(literal_to_value(lit)),
        Expression::Array(arr) => {
            let mut values = Vec::new();
            for elem in &arr.elements {
                values.push(try_evaluate_compile_time(elem)?);
            }
            Some(Value::Array(values))
        }
        Expression::Grouped(inner, _) => try_evaluate_compile_time(inner),
        // Variables, function calls, etc. require runtime evaluation
        _ => None,
    }
}

/// Convert a literal AST node to a runtime Value
fn literal_to_value(lit: &Literal) -> Value {
    match &lit.kind {
        LiteralKind::Int(n) => Value::I64(*n),
        LiteralKind::Float(f) => Value::F64(*f),
        LiteralKind::String(s) => Value::String(s.chars().collect()),
        LiteralKind::Char(c) => Value::Rune(*c),
        LiteralKind::Bool(b) => Value::Bool(*b),
        LiteralKind::None => Value::None,
    }
}

/// Execute a macro definition with the given argument values
fn execute_macro(
    macro_def: &MacroDef,
    args: &[Value],
    all_macros: &HashMap<String, MacroDef>,
    qualified_macros: &HashMap<String, MacroDef>,
    macro_modules: &HashMap<String, String>,
    module_imports: &HashMap<String, HashMap<String, String>>,
    macro_source_module: Option<&str>,
) -> NexusResult<String> {
    // Create a minimal interpreter for macro execution
    let config = InterpreterConfig {
        bounds_checking: true,
        max_recursion_depth: 100,
        max_steps: Some(100000), // Increased limit for nested macro expansion
    };

    let mut interpreter = Interpreter::with_config(config);

    // Set the current module to the macro's source module if known
    if let Some(module) = macro_source_module {
        interpreter.set_current_module(module);
    }

    // Register all modules as known
    for module_name in module_imports.keys() {
        let _ = interpreter.register_module(module_name);
    }
    for module_name in macro_modules.values() {
        let _ = interpreter.register_module(module_name);
    }

    // Register all macros with their qualified names
    for (qualified_name, def) in qualified_macros {
        interpreter.register_macro_direct(qualified_name, def.clone());
    }

    // Also register macros by short name for backward compatibility
    for (name, def) in all_macros {
        // Register with empty prefix for fallback
        let qualified = format!(".{}", name);
        interpreter.register_macro_direct(&qualified, def.clone());
        interpreter.register_macro_direct(name, def.clone());
    }

    // Register module imports so nested macro calls can resolve their dependencies
    // We need to use the interpreter's internal method to set up module_imported_symbols
    // Since we can't directly access it, we'll process use statements through the interpreter
    for (module_name, imports) in module_imports {
        // Register each module's imports
        for (symbol, source_module) in imports {
            // Register the imported macro/function with the interpreter
            // The interpreter tracks this via module_imported_symbols
            interpreter.register_import(module_name, symbol, source_module);
        }
    }

    // Create a scope with the macro parameters bound to the argument values
    let mut scope = Scope::new();
    for (param, value) in macro_def.params.iter().zip(args.iter()) {
        scope.define(Variable {
            name: param.name.clone(),
            value: value.clone(),
            modifiers: VarModifiers::default(),
            span: param.span,
        })?;
    }

    // Execute the macro body using the interpreter's execute_block method
    let result = interpreter.execute_block(&macro_def.body, &mut scope)?;

    // The macro must return a string
    match result {
        Value::String(chars) => Ok(chars.into_iter().collect()),
        _ => Err(nexus_core::NexusError::TypeError {
            message: "Macro must return a string (macro type)".to_string(),
            span: macro_def.span,
        }),
    }
}

/// Format a macro definition for hover display
pub fn format_macro_signature(macro_def: &MacroDef) -> String {
    let params: Vec<String> = macro_def
        .params
        .iter()
        .map(|p| format!("{} {}", crate::utils::format_type_expr(&p.ty), p.name))
        .collect();

    format!("${}({}): macro", macro_def.name, params.join(", "))
}

/// Generate hover content for a macro call
pub fn generate_macro_hover(
    macro_def: &MacroDef,
    expansion_result: &MacroExpansionResult,
    doc_comment: &str,
) -> String {
    let mut content = String::new();

    // Signature
    content.push_str("```nexus\n");
    content.push_str(&format_macro_signature(macro_def));
    content.push_str("\n```\n");

    // Doc comment if present
    if !doc_comment.is_empty() {
        content.push_str("\n---\n\n");
        content.push_str(doc_comment);
        content.push('\n');
    }

    // Expansion result
    content.push_str("\n---\n\n");
    content.push_str("**Macro Expansion:**\n\n");

    match expansion_result {
        MacroExpansionResult::Success { code } => {
            content.push_str("```nexus\n");
            content.push_str(code);
            content.push_str("\n```\n");
        }
        MacroExpansionResult::RuntimeOnly { reason } => {
            content.push_str("*Cannot expand at compile-time:* ");
            content.push_str(reason);
            content.push('\n');
        }
        MacroExpansionResult::NotFound => {
            content.push_str("*Macro definition not found*\n");
        }
        MacroExpansionResult::Error { message } => {
            content.push_str("*Expansion error:* ");
            content.push_str(message);
            content.push('\n');
        }
    }

    content
}

#[cfg(test)]
mod tests {
    use super::*;
    use nexus_parser::parse;

    #[test]
    fn test_context_from_programs() {
        let code = r#"
            $test_macro([dyn]rune input): macro {
                return input
            }
        "#;
        let program = parse(code).unwrap();
        let ctx = MacroExpansionContext::from_programs([&program]);
        assert!(ctx.get_macro("test_macro").is_some());
    }

    #[test]
    fn test_literal_to_value() {
        let lit = Literal {
            kind: LiteralKind::Int(42),
            span: nexus_core::Span::new(0, 2, 1, 1),
        };
        let val = literal_to_value(&lit);
        assert!(matches!(val, Value::I64(42)));
    }

    #[test]
    fn test_compile_time_string() {
        let expr = Expression::Literal(Literal {
            kind: LiteralKind::String("hello".to_string()),
            span: nexus_core::Span::new(0, 7, 1, 1),
        });
        let val = try_evaluate_compile_time(&expr);
        assert!(val.is_some());
        if let Some(Value::String(chars)) = val {
            assert_eq!(chars.iter().collect::<String>(), "hello");
        }
    }
}

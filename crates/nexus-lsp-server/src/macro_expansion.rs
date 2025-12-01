//! Macro expansion for LSP hover support.
//!
//! This module provides functionality to expand macros at compile-time
//! for displaying the generated code in hover tooltips.

use std::collections::HashMap;

use nexus_core::{NexusResult, VarModifiers};
use nexus_interpreter::{Interpreter, InterpreterConfig, Scope, Value, Variable};
use nexus_parser::{Expression, Item, Literal, LiteralKind, MacroDef, Program};

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
    /// Macro definitions (name -> MacroDef)
    macros: HashMap<String, MacroDef>,
}

impl MacroExpansionContext {
    /// Create a new empty context
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
        }
    }

    /// Build a context from multiple programs (for cross-file support)
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

    /// Get a macro definition by name
    pub fn get_macro(&self, name: &str) -> Option<&MacroDef> {
        self.macros.get(name)
    }

    /// Attempt to expand a macro call with the given arguments
    pub fn expand_macro(&self, name: &str, args: &[Expression]) -> MacroExpansionResult {
        let Some(macro_def) = self.macros.get(name) else {
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

        // Execute the macro in a sandboxed interpreter
        match execute_macro(macro_def, &arg_values) {
            Ok(code) => MacroExpansionResult::Success { code },
            Err(e) => MacroExpansionResult::Error {
                message: format!("Macro expansion error: {}", e),
            },
        }
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
fn execute_macro(macro_def: &MacroDef, args: &[Value]) -> NexusResult<String> {
    // Create a minimal interpreter for macro execution
    let config = InterpreterConfig {
        bounds_checking: true,
        max_recursion_depth: 100,
        max_steps: Some(10000), // Limit steps for safety
    };

    let mut interpreter = Interpreter::with_config(config);

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

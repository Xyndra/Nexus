//! Diagnostics functionality for the LSP server.
//!
//! This module provides diagnostic checking for Nexus source code,
//! including syntax errors and semantic analysis.

use nexus_core::Diagnostics;
use nexus_parser::{Block, ElseClause, Item, Program, Statement, parse};

/// Configuration for diagnostic checks.
#[derive(Debug, Clone)]
pub struct DiagnosticsConfig {
    /// Whether diagnostics are enabled
    pub enabled: bool,
}

impl Default for DiagnosticsConfig {
    fn default() -> Self {
        Self { enabled: true }
    }
}

/// Compute diagnostics for a document.
pub fn compute_diagnostics(
    content: &str,
    ast: &Option<Program>,
    config: &DiagnosticsConfig,
) -> Diagnostics {
    let mut diagnostics = Diagnostics::new();

    if !config.enabled {
        return diagnostics;
    }

    // Try to parse and collect errors
    if ast.is_none()
        && let Err(e) = parse(content)
    {
        diagnostics.push(e.into());
    }

    // Semantic checks
    if let Some(program) = ast {
        // Check for return statements with values inside subscopes
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    check_returns_in_block(&func.body, 0, &mut diagnostics);
                }
                Item::Method(method) => {
                    check_returns_in_block(&method.body, 0, &mut diagnostics);
                }
                _ => {}
            }
        }
    }

    diagnostics
}

/// Check for return statements with values inside subscopes.
fn check_returns_in_block(block: &Block, subscope_depth: usize, diagnostics: &mut Diagnostics) {
    for stmt in &block.statements {
        check_returns_in_statement(stmt, subscope_depth, diagnostics);
    }
}

/// Check a statement for invalid returns inside subscopes.
fn check_returns_in_statement(
    stmt: &Statement,
    subscope_depth: usize,
    diagnostics: &mut Diagnostics,
) {
    match stmt {
        Statement::Return(ret) => {
            if subscope_depth > 0 && ret.value.is_some() {
                diagnostics.error("Return inside a subscope cannot have a value", ret.span);
            }
        }
        Statement::If(if_stmt) => {
            check_returns_in_block(&if_stmt.then_block, subscope_depth, diagnostics);
            if let Some(else_clause) = &if_stmt.else_block {
                check_else_clause(else_clause, subscope_depth, diagnostics);
            }
        }
        Statement::Subscope(subscope) => {
            check_returns_in_block(&subscope.body, subscope_depth + 1, diagnostics);
        }
        Statement::Block(block) => {
            check_returns_in_block(block, subscope_depth, diagnostics);
        }
        Statement::Defer(defer) => {
            check_returns_in_block(&defer.body, subscope_depth, diagnostics);
        }
        _ => {}
    }
}

/// Check an else clause for invalid returns inside subscopes.
fn check_else_clause(
    else_clause: &ElseClause,
    subscope_depth: usize,
    diagnostics: &mut Diagnostics,
) {
    match else_clause {
        ElseClause::Block(block) => {
            check_returns_in_block(block, subscope_depth, diagnostics);
        }
        ElseClause::ElseIf(else_if) => {
            check_returns_in_block(&else_if.then_block, subscope_depth, diagnostics);
            if let Some(inner_else) = &else_if.else_block {
                check_else_clause(inner_else, subscope_depth, diagnostics);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_content(content: &str) -> Option<Program> {
        parse(content).ok()
    }

    #[test]
    fn test_diagnostics_disabled() {
        let content = "invalid syntax here";
        let ast = parse_content(content);
        let config = DiagnosticsConfig { enabled: false };
        let diagnostics = compute_diagnostics(content, &ast, &config);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_diagnostics_parse_error() {
        let content = "std main( {}"; // Missing closing paren and return type
        let ast = parse_content(content);
        let config = DiagnosticsConfig::default();
        let diagnostics = compute_diagnostics(content, &ast, &config);
        assert!(!diagnostics.is_empty());
    }

    #[test]
    fn test_diagnostics_valid_code() {
        let content = "std main(): void { return }";
        let ast = parse_content(content);
        let config = DiagnosticsConfig::default();
        let diagnostics = compute_diagnostics(content, &ast, &config);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_return_with_value_in_subscope_error() {
        let content = r#"
            std main(): i64 {
                subscope loop {
                    return 42
                    goto loop
                }
            }
        "#;
        let ast = parse_content(content);
        let config = DiagnosticsConfig::default();
        let diagnostics = compute_diagnostics(content, &ast, &config);

        // Should have an error about return with value in subscope
        assert!(!diagnostics.is_empty());
    }

    #[test]
    fn test_return_without_value_in_subscope_ok() {
        let content = r#"
            std main(): void {
                subscope loop {
                    m x = 1
                    goto loop
                }
            }
        "#;
        let ast = parse_content(content);
        let config = DiagnosticsConfig::default();
        let diagnostics = compute_diagnostics(content, &ast, &config);

        // Subscope without return with value should be ok
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_return_with_value_outside_subscope_ok() {
        let content = r#"
            std main(): i64 {
                return 42
            }
        "#;
        let ast = parse_content(content);
        let config = DiagnosticsConfig::default();
        let diagnostics = compute_diagnostics(content, &ast, &config);

        // Return with value outside subscope should be ok
        assert!(diagnostics.is_empty());
    }
}

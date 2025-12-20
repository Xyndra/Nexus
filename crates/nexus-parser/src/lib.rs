//! Parser for the Nexus programming language.
//!
//! This crate provides the parser that transforms tokens into an Abstract Syntax Tree (AST).

mod ast;
mod parser;

pub use ast::*;
pub use parser::*;

use nexus_core::NexusError;

/// Parse source code into an AST
pub fn parse(source: &str) -> Result<Program, NexusError> {
    let lexer = nexus_lexer::Lexer::new(source);
    let tokens = lexer.tokenize()?;
    let mut parser = Parser::new(tokens);
    parser.parse_program()
}

/// Parse a single expression
pub fn parse_expression(source: &str) -> Result<Expression, NexusError> {
    let lexer = nexus_lexer::Lexer::new(source);
    let tokens = lexer.tokenize()?;
    let mut parser = Parser::new(tokens);
    parser.parse_expression()
}

/// Parse a single statement
pub fn parse_statement(source: &str) -> Result<Statement, NexusError> {
    let lexer = nexus_lexer::Lexer::new(source);
    let tokens = lexer.tokenize()?;
    let mut parser = Parser::new(tokens);
    parser.parse_statement()
}

/// Parse a list of top-level items (for macro expansion)
pub fn parse_items(source: &str) -> Result<Vec<Item>, NexusError> {
    let lexer = nexus_lexer::Lexer::new(source);
    let tokens = lexer.tokenize()?;
    let mut parser = Parser::new(tokens);
    parser.parse_items()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_function() {
        let source = r#"
            std main(): void {
                m x = 42
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    }

    #[test]
    fn test_parse_struct() {
        let source = r#"
            struct Player {
                i32 health = 100
                [dyn]rune name
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    }

    #[test]
    fn test_parse_variable_modifiers() {
        let source = r#"
            std test(): void {
                m x = 1
                mh arr = [1, 2, 3]
                g counter = 0
            }
        "#;
        let result = parse(source);
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    }
}

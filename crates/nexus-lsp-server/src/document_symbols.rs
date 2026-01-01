//! Document symbol functionality for the LSP server.
//!
//! This module provides document symbol information for Nexus source code,
//! including functions, structs, interfaces, and their children.

use nexus_parser::Program;

use crate::types::{DocumentSymbol, SymbolKind};
use crate::utils::span_to_range;

/// Get document symbols for the given content and AST.
pub fn get_document_symbols(content: &str, ast: &Program) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();

    // Add functions
    for func in ast.functions() {
        symbols.push(DocumentSymbol {
            name: func.name.clone(),
            kind: SymbolKind::Function,
            range: span_to_range(content, &func.span),
            selection_range: span_to_range(content, &func.name_span),
            children: Vec::new(),
        });
    }

    // Add methods
    for item in &ast.items {
        if let nexus_parser::Item::Method(method) = item {
            symbols.push(DocumentSymbol {
                name: format!("{}.{}", method.receiver_type, method.name),
                kind: SymbolKind::Method,
                range: span_to_range(content, &method.span),
                selection_range: span_to_range(content, &method.name_span),
                children: Vec::new(),
            });
        }
    }

    // Add structs with their fields
    for struct_def in ast.structs() {
        let mut children = Vec::new();

        for field in &struct_def.fields {
            children.push(DocumentSymbol {
                name: field.name.clone(),
                kind: SymbolKind::Field,
                range: span_to_range(content, &field.span),
                selection_range: span_to_range(content, &field.span),
                children: Vec::new(),
            });
        }

        symbols.push(DocumentSymbol {
            name: struct_def.name.clone(),
            kind: SymbolKind::Struct,
            range: span_to_range(content, &struct_def.span),
            selection_range: span_to_range(content, &struct_def.name_span),
            children,
        });
    }

    // Add interfaces with their methods
    for iface in ast.interfaces() {
        let mut children = Vec::new();

        for method in &iface.methods {
            children.push(DocumentSymbol {
                name: method.name.clone(),
                kind: SymbolKind::Method,
                range: span_to_range(content, &method.span),
                selection_range: span_to_range(content, &method.span),
                children: Vec::new(),
            });
        }

        symbols.push(DocumentSymbol {
            name: iface.name.clone(),
            kind: SymbolKind::Interface,
            range: span_to_range(content, &iface.span),
            selection_range: span_to_range(content, &iface.name_span),
            children,
        });
    }

    // Add macros
    for item in &ast.items {
        if let nexus_parser::Item::Macro(macro_def) = item {
            symbols.push(DocumentSymbol {
                name: macro_def.name.clone(),
                kind: SymbolKind::Macro,
                range: span_to_range(content, &macro_def.span),
                selection_range: span_to_range(content, &macro_def.span),
                children: Vec::new(),
            });
        }
    }

    symbols
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_content(content: &str) -> Option<Program> {
        nexus_parser::parse(content).ok()
    }

    #[test]
    fn test_document_symbols_functions() {
        // Use the same syntax that works in lib.rs tests
        let content = "std main(): void {} std helper(): i64 { return 42 }";
        let ast = parse_content(content);
        assert!(
            ast.is_some(),
            "Failed to parse content: {:?}",
            nexus_parser::parse(content).err()
        );
        let ast = ast.unwrap();
        let symbols = get_document_symbols(content, &ast);

        let function_names: Vec<&str> = symbols
            .iter()
            .filter(|s| s.kind == SymbolKind::Function)
            .map(|s| s.name.as_str())
            .collect();

        assert!(function_names.contains(&"main"));
        assert!(function_names.contains(&"helper"));
    }

    #[test]
    fn test_document_symbols_structs() {
        let content = r#"
            struct Point {
                i64 x = 0
                i64 y = 0
            }
        "#;
        let ast = parse_content(content).unwrap();
        let symbols = get_document_symbols(content, &ast);

        let struct_symbol = symbols
            .iter()
            .find(|s| s.kind == SymbolKind::Struct && s.name == "Point");

        assert!(struct_symbol.is_some());
        let struct_symbol = struct_symbol.unwrap();

        let field_names: Vec<&str> = struct_symbol
            .children
            .iter()
            .map(|c| c.name.as_str())
            .collect();

        assert!(field_names.contains(&"x"));
        assert!(field_names.contains(&"y"));
    }

    #[test]
    fn test_document_symbols_interfaces() {
        let content = "interface Drawable {\ndraw(): void\n}";
        let ast = parse_content(content);
        assert!(ast.is_some(), "Failed to parse interface content");
        let ast = ast.unwrap();
        let symbols = get_document_symbols(content, &ast);

        let iface_symbol = symbols
            .iter()
            .find(|s| s.kind == SymbolKind::Interface && s.name == "Drawable");

        assert!(iface_symbol.is_some());
        let iface_symbol = iface_symbol.unwrap();

        let method_names: Vec<&str> = iface_symbol
            .children
            .iter()
            .map(|c| c.name.as_str())
            .collect();

        assert!(method_names.contains(&"draw"));
    }
}

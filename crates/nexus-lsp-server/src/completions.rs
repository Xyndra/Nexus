//! Completion functionality for the LSP server.
//!
//! This module provides code completion suggestions for Nexus source code,
//! including keywords, builtins, and user-defined symbols.

use nexus_parser::Program;

use crate::builtins::BUILTINS;
use crate::types::{CompletionItem, CompletionKind};

/// Get completion items for a document at the given position.
pub fn get_completions(ast: Option<&Program>) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // Add keywords
    add_keywords(&mut items);

    // Add builtin functions
    add_builtins(&mut items);

    // Add variable modifiers
    add_modifiers(&mut items);

    // Add document-specific completions
    if let Some(ast) = ast {
        add_document_completions(ast, &mut items);
    }

    items
}

/// Add language keywords to the completion list.
fn add_keywords(items: &mut Vec<CompletionItem>) {
    let keywords = [
        "std",
        "compat",
        "plat",
        "struct",
        "interface",
        "impl",
        "if",
        "else",
        "return",
        "defer",
        "subscope",
        "goto",
        "unknown",
        "void",
        "bool",
        "i8",
        "i16",
        "i32",
        "i64",
        "u8",
        "u16",
        "u32",
        "u64",
        "f32",
        "f64",
        "rune",
        "dyn",
        "true",
        "false",
        "None",
        "Error",
        "macro",
        "use",
        "from",
        "string",
        "bytes",
    ];

    for kw in keywords {
        items.push(CompletionItem {
            label: kw.to_string(),
            kind: CompletionKind::Keyword,
            detail: Some("keyword".to_string()),
            documentation: None,
            insert_text: None,
        });
    }
}

/// Add builtin functions to the completion list.
fn add_builtins(items: &mut Vec<CompletionItem>) {
    for builtin in BUILTINS.values() {
        let params: Vec<String> = builtin
            .params
            .iter()
            .map(|(name, ty)| format!("{}: {}", name, ty))
            .collect();

        let signature = format!(
            "{}({}): {}",
            builtin.name,
            params.join(", "),
            builtin.return_type
        );

        let documentation = format!(
            "{}\n\nSignature: `{}`\n\n{}",
            builtin.description, signature, builtin.documentation
        );

        items.push(CompletionItem {
            label: builtin.name.to_string(),
            kind: CompletionKind::Builtin,
            detail: Some("builtin function".to_string()),
            documentation: Some(documentation),
            insert_text: Some(format!("{}($0)", builtin.name)),
        });
    }
}

/// Add variable modifiers to the completion list.
fn add_modifiers(items: &mut Vec<CompletionItem>) {
    let modifiers = [
        (
            "m",
            "mutable variable",
            "Declares a mutable variable that can be reassigned.",
        ),
        (
            "mh",
            "mutable heap variable",
            "Declares a mutable variable allocated on the heap.",
        ),
        (
            "g",
            "global variable",
            "Declares a global variable accessible throughout the program.",
        ),
        (
            "l",
            "locked variable",
            "Declares a locked variable for thread-safe access.",
        ),
        (
            "u",
            "undetermined type variable",
            "Declares a variable with an undetermined (inferred) type.",
        ),
    ];

    for (mod_str, detail, doc) in modifiers {
        items.push(CompletionItem {
            label: mod_str.to_string(),
            kind: CompletionKind::Keyword,
            detail: Some(detail.to_string()),
            documentation: Some(doc.to_string()),
            insert_text: Some(format!("{} ", mod_str)),
        });
    }
}

/// Add document-specific completions (functions, structs, interfaces).
fn add_document_completions(ast: &Program, items: &mut Vec<CompletionItem>) {
    // Add functions from this document
    for func in ast.functions() {
        let params: Vec<String> = func
            .params
            .iter()
            .map(|p| format!("{}: {}", p.name, format_type_expr(&p.ty)))
            .collect();

        items.push(CompletionItem {
            label: func.name.clone(),
            kind: CompletionKind::Function,
            detail: Some(format!("{} function", func.color)),
            documentation: Some(format!(
                "{}({}): {}",
                func.name,
                params.join(", "),
                format_type_expr(&func.return_type)
            )),
            insert_text: Some(format!("{}($0)", func.name)),
        });
    }

    // Add structs from this document
    for struct_def in ast.structs() {
        let fields: Vec<String> = struct_def
            .fields
            .iter()
            .map(|f| format!("{}: {}", f.name, format_type_expr(&f.ty)))
            .collect();

        items.push(CompletionItem {
            label: struct_def.name.clone(),
            kind: CompletionKind::Struct,
            detail: Some("struct".to_string()),
            documentation: Some(format!(
                "struct {} {{\n  {}\n}}",
                struct_def.name,
                fields.join(",\n  ")
            )),
            insert_text: Some(format!("{} {{ $0 }}", struct_def.name)),
        });
    }

    // Add interfaces from this document
    for iface in ast.interfaces() {
        items.push(CompletionItem {
            label: iface.name.clone(),
            kind: CompletionKind::Interface,
            detail: Some("interface".to_string()),
            documentation: Some(format!(
                "interface {} with {} method(s)",
                iface.name,
                iface.methods.len()
            )),
            insert_text: None,
        });
    }

    // Add macros from this document
    for item in &ast.items {
        if let nexus_parser::Item::Macro(macro_def) = item {
            items.push(CompletionItem {
                label: macro_def.name.clone(),
                kind: CompletionKind::Macro,
                detail: Some("macro".to_string()),
                documentation: Some(format!("macro {}!", macro_def.name)),
                insert_text: Some(format!("{}!($0)", macro_def.name)),
            });
        }
    }
}

/// Format a type expression for display.
fn format_type_expr(ty: &nexus_parser::TypeExpr) -> String {
    match ty {
        nexus_parser::TypeExpr::Named { name, .. } => name.clone(),
        nexus_parser::TypeExpr::Array { element, .. } => {
            format!("[{}]", format_type_expr(element))
        }
        nexus_parser::TypeExpr::Unknown { variants, .. } => {
            let variants_str: Vec<String> = variants.iter().map(format_type_expr).collect();
            format!("unknown<{}>", variants_str.join(", "))
        }
        nexus_parser::TypeExpr::Function {
            params,
            return_type,
            ..
        } => {
            let params_str: Vec<String> = params.iter().map(format_type_expr).collect();
            format!(
                "({}) -> {}",
                params_str.join(", "),
                format_type_expr(return_type)
            )
        }
        nexus_parser::TypeExpr::Void { .. } => "void".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_content(content: &str) -> Option<Program> {
        nexus_parser::parse(content).ok()
    }

    #[test]
    fn test_completions_include_keywords() {
        let completions = get_completions(None);
        let keywords: Vec<&str> = completions
            .iter()
            .filter(|c| c.kind == CompletionKind::Keyword)
            .map(|c| c.label.as_str())
            .collect();

        assert!(keywords.contains(&"std"));
        assert!(keywords.contains(&"if"));
        assert!(keywords.contains(&"return"));
        assert!(keywords.contains(&"struct"));
    }

    #[test]
    fn test_completions_include_builtins() {
        let completions = get_completions(None);
        let builtins: Vec<&str> = completions
            .iter()
            .filter(|c| c.kind == CompletionKind::Builtin)
            .map(|c| c.label.as_str())
            .collect();

        assert!(builtins.contains(&"len"));
        assert!(builtins.contains(&"push"));
        assert!(builtins.contains(&"println"));
    }

    #[test]
    fn test_completions_include_document_functions() {
        let content = "std helper(i64 x): i64 { return x } std main(): void { m x = 1 }";
        let ast = parse_content(content);
        assert!(
            ast.is_some(),
            "Failed to parse: {:?}",
            nexus_parser::parse(content).err()
        );
        let completions = get_completions(Some(&ast.unwrap()));

        let functions: Vec<&str> = completions
            .iter()
            .filter(|c| c.kind == CompletionKind::Function)
            .map(|c| c.label.as_str())
            .collect();

        assert!(functions.contains(&"helper"));
    }

    #[test]
    fn test_completions_include_document_structs() {
        let content = "struct Point { i64 x = 0\n i64 y = 0 }";
        let ast = parse_content(content).unwrap();
        let completions = get_completions(Some(&ast));

        let structs: Vec<&str> = completions
            .iter()
            .filter(|c| c.kind == CompletionKind::Struct)
            .map(|c| c.label.as_str())
            .collect();

        assert!(structs.contains(&"Point"));
    }

    #[test]
    fn test_builtin_completions_have_documentation() {
        let completions = get_completions(None);
        let len_completion = completions.iter().find(|c| c.label == "len");

        assert!(len_completion.is_some());
        let len_completion = len_completion.unwrap();
        assert!(len_completion.documentation.is_some());
        let doc = len_completion.documentation.as_ref().unwrap();
        assert!(doc.contains("collection"));
        assert!(doc.contains("i64"));
    }
}

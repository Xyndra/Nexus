//! Hover functionality for the LSP server.
//!
//! This module provides hover information for symbols in Nexus source code,
//! including function signatures, type information, and documentation.

use std::collections::HashMap;

use nexus_parser::{Expression, IfCondition, Item, Program, Statement};

use crate::builtins::get_builtin;
use crate::types::HoverInfo;
use crate::utils::{extract_doc_comment, format_parameter, format_type_expr, span_to_range};

/// Find hover information at the given offset in the document.
pub fn find_hover(
    content: &str,
    ast: &Program,
    offset: usize,
    documents: &HashMap<String, (String, Option<Program>)>,
) -> Option<HoverInfo> {
    // Look for function definitions - only show hover when on the name
    for func in ast.functions() {
        if func.name_span.contains(offset) {
            let params: Vec<String> = func.params.iter().map(format_parameter).collect();

            let doc_comment = extract_doc_comment(content, &func.span);

            let mut hover_content = format!(
                "```nexus\n{} {}({}): {}\n```",
                func.color,
                func.name,
                params.join(", "),
                format_type_expr(&func.return_type),
            );

            if !doc_comment.is_empty() {
                hover_content.push_str("\n\n---\n\n");
                hover_content.push_str(&doc_comment);
            }

            return Some(HoverInfo {
                contents: hover_content,
                range: Some(span_to_range(content, &func.name_span)),
            });
        }
    }

    // Look for structs - only show hover when on the name
    for struct_def in ast.structs() {
        if struct_def.name_span.contains(offset) {
            let doc_comment = extract_doc_comment(content, &struct_def.span);

            let mut hover_content = format!(
                "```nexus\nstruct {}\n```\n\nFields: {}",
                struct_def.name,
                struct_def.fields.len()
            );

            if !doc_comment.is_empty() {
                hover_content.push_str("\n\n---\n\n");
                hover_content.push_str(&doc_comment);
            }

            return Some(HoverInfo {
                contents: hover_content,
                range: Some(span_to_range(content, &struct_def.name_span)),
            });
        }
    }

    // Look for interfaces - only show hover when on the name
    for iface in ast.interfaces() {
        if iface.name_span.contains(offset) {
            let doc_comment = extract_doc_comment(content, &iface.span);

            let mut hover_content = format!(
                "```nexus\ninterface {}\n```\n\nMethods: {}",
                iface.name,
                iface.methods.len()
            );

            if !doc_comment.is_empty() {
                hover_content.push_str("\n\n---\n\n");
                hover_content.push_str(&doc_comment);
            }

            return Some(HoverInfo {
                contents: hover_content,
                range: Some(span_to_range(content, &iface.name_span)),
            });
        }
    }

    // Look for function calls - show hover for the called function
    let imports = collect_imports(ast);
    for item in &ast.items {
        if let Item::Function(func) = item
            && func.body.span.contains(offset)
            && let Some(hover) = find_hover_in_statements(
                content,
                &func.body.statements,
                offset,
                &imports,
                documents,
            )
        {
            return Some(hover);
        }
        if let Item::Method(method) = item
            && method.body.span.contains(offset)
            && let Some(hover) = find_hover_in_statements(
                content,
                &method.body.statements,
                offset,
                &imports,
                documents,
            )
        {
            return Some(hover);
        }
    }

    None
}

/// Collect import information from the AST.
/// Returns a map of symbol name -> (module path, is_module_import).
fn collect_imports(ast: &Program) -> HashMap<String, (Vec<String>, bool)> {
    let mut imports = HashMap::new();

    for item in &ast.items {
        if let Item::Use(use_stmt) = item {
            if use_stmt.symbols.is_empty() {
                // Module import: `use mathlib`
                if let Some(module_name) = use_stmt.module_path.last() {
                    imports.insert(module_name.clone(), (use_stmt.module_path.clone(), true));
                }
            } else {
                // Symbol imports: `use { abs, max } from mathlib`
                for symbol in &use_stmt.symbols {
                    imports.insert(symbol.clone(), (use_stmt.module_path.clone(), false));
                }
            }
        }
    }

    imports
}

fn find_hover_in_statements(
    content: &str,
    statements: &[Statement],
    offset: usize,
    imports: &HashMap<String, (Vec<String>, bool)>,
    documents: &HashMap<String, (String, Option<Program>)>,
) -> Option<HoverInfo> {
    for stmt in statements {
        if let Some(hover) = find_hover_in_statement(content, stmt, offset, imports, documents) {
            return Some(hover);
        }
    }
    None
}

fn find_hover_in_statement(
    content: &str,
    stmt: &Statement,
    offset: usize,
    imports: &HashMap<String, (Vec<String>, bool)>,
    documents: &HashMap<String, (String, Option<Program>)>,
) -> Option<HoverInfo> {
    match stmt {
        Statement::VarDecl(var_decl) => {
            find_hover_in_expression(content, &var_decl.init, offset, imports, documents)
        }
        Statement::Assignment(assign) => {
            if let Some(hover) =
                find_hover_in_expression(content, &assign.target, offset, imports, documents)
            {
                return Some(hover);
            }
            find_hover_in_expression(content, &assign.value, offset, imports, documents)
        }
        Statement::Expression(expr) => {
            find_hover_in_expression(content, expr, offset, imports, documents)
        }
        Statement::Return(ret) => {
            if let Some(expr) = &ret.value {
                return find_hover_in_expression(content, expr, offset, imports, documents);
            }
            None
        }
        Statement::If(if_stmt) => {
            // Handle condition based on IfCondition type
            if let IfCondition::Boolean(cond_expr) = &if_stmt.condition
                && let Some(hover) =
                    find_hover_in_expression(content, cond_expr, offset, imports, documents)
            {
                return Some(hover);
            }
            if let Some(hover) = find_hover_in_statements(
                content,
                &if_stmt.then_block.statements,
                offset,
                imports,
                documents,
            ) {
                return Some(hover);
            }
            if let Some(else_clause) = &if_stmt.else_block {
                return find_hover_in_else_clause(content, else_clause, offset, imports, documents);
            }
            None
        }
        Statement::Defer(defer) => {
            find_hover_in_statements(content, &defer.body.statements, offset, imports, documents)
        }
        Statement::Subscope(subscope) => find_hover_in_statements(
            content,
            &subscope.body.statements,
            offset,
            imports,
            documents,
        ),
        Statement::Block(block) => {
            find_hover_in_statements(content, &block.statements, offset, imports, documents)
        }
        _ => None,
    }
}

fn find_hover_in_else_clause(
    content: &str,
    else_clause: &nexus_parser::ElseClause,
    offset: usize,
    imports: &HashMap<String, (Vec<String>, bool)>,
    documents: &HashMap<String, (String, Option<Program>)>,
) -> Option<HoverInfo> {
    match else_clause {
        nexus_parser::ElseClause::ElseIf(else_if) => {
            if let IfCondition::Boolean(cond_expr) = &else_if.condition
                && let Some(hover) =
                    find_hover_in_expression(content, cond_expr, offset, imports, documents)
            {
                return Some(hover);
            }
            if let Some(hover) = find_hover_in_statements(
                content,
                &else_if.then_block.statements,
                offset,
                imports,
                documents,
            ) {
                return Some(hover);
            }
            if let Some(next_else) = &else_if.else_block {
                return find_hover_in_else_clause(content, next_else, offset, imports, documents);
            }
            None
        }
        nexus_parser::ElseClause::Block(else_block) => {
            find_hover_in_statements(content, &else_block.statements, offset, imports, documents)
        }
    }
}

fn find_hover_in_expression(
    content: &str,
    expr: &Expression,
    offset: usize,
    imports: &HashMap<String, (Vec<String>, bool)>,
    documents: &HashMap<String, (String, Option<Program>)>,
) -> Option<HoverInfo> {
    match expr {
        Expression::Call(call) => {
            // IMPORTANT: Check arguments FIRST before the outer function.
            // This ensures that in nested calls like `foo(bar(x))`, if the cursor
            // is on `bar`, we show `bar`'s hover info, not `foo`'s.
            for arg in &call.args {
                if arg.span().contains(offset)
                    && let Some(hover) =
                        find_hover_in_expression(content, arg, offset, imports, documents)
                {
                    return Some(hover);
                }
            }

            // Now check if cursor is on the function call itself
            if call.span.contains(offset) {
                // Try to find the function and get its hover info
                if let Some(hover) = get_function_hover(&call.function, imports, documents) {
                    return Some(hover);
                }
            }
            None
        }
        Expression::MethodCall(method_call) => {
            // Check arguments first (same principle as Call)
            for arg in &method_call.args {
                if arg.span().contains(offset)
                    && let Some(hover) =
                        find_hover_in_expression(content, arg, offset, imports, documents)
                {
                    return Some(hover);
                }
            }

            // Check receiver
            if method_call.receiver.span().contains(offset)
                && let Some(hover) = find_hover_in_expression(
                    content,
                    &method_call.receiver,
                    offset,
                    imports,
                    documents,
                )
            {
                return Some(hover);
            }

            // Check if this is a module-qualified call like `mathlib.abs()`
            if method_call.span.contains(offset)
                && let Expression::Variable(var_ref) = method_call.receiver.as_ref()
                && let Some((_, is_module)) = imports.get(&var_ref.name)
                && *is_module
            {
                // This is a module-qualified call
                if let Some(hover) = get_function_hover(&method_call.method, imports, documents) {
                    return Some(hover);
                }
            }
            None
        }
        Expression::Array(array) => {
            for elem in &array.elements {
                if let Some(hover) =
                    find_hover_in_expression(content, elem, offset, imports, documents)
                {
                    return Some(hover);
                }
            }
            None
        }
        Expression::Index(index) => {
            if let Some(hover) =
                find_hover_in_expression(content, &index.array, offset, imports, documents)
            {
                return Some(hover);
            }
            find_hover_in_expression(content, &index.index, offset, imports, documents)
        }
        Expression::FieldAccess(field_access) => {
            find_hover_in_expression(content, &field_access.object, offset, imports, documents)
        }
        Expression::Grouped(inner, _) => {
            find_hover_in_expression(content, inner, offset, imports, documents)
        }
        Expression::MacroCall(macro_call) => {
            for arg in &macro_call.args {
                if let Some(hover) =
                    find_hover_in_expression(content, arg, offset, imports, documents)
                {
                    return Some(hover);
                }
            }
            None
        }
        Expression::Lambda(lambda) => match &lambda.body {
            nexus_parser::LambdaBody::Expression(expr) => {
                find_hover_in_expression(content, expr, offset, imports, documents)
            }
            nexus_parser::LambdaBody::Block(block) => {
                find_hover_in_statements(content, &block.statements, offset, imports, documents)
            }
        },
        _ => None,
    }
}

/// Get hover information for a function by name.
/// Checks builtins first, then user-defined functions in documents.
fn get_function_hover(
    name: &str,
    _imports: &HashMap<String, (Vec<String>, bool)>,
    documents: &HashMap<String, (String, Option<Program>)>,
) -> Option<HoverInfo> {
    // First check if it's a builtin
    if let Some(builtin) = get_builtin(name) {
        return Some(HoverInfo {
            contents: builtin.hover_content(),
            range: None,
        });
    }

    // Then check user-defined functions in all documents
    for (doc_content, ast_opt) in documents.values() {
        if let Some(ast) = ast_opt {
            for func in ast.functions() {
                if func.name == name {
                    let params: Vec<String> = func.params.iter().map(format_parameter).collect();

                    let doc_comment = extract_doc_comment(doc_content, &func.span);

                    let mut hover_content = format!(
                        "```nexus\n{} {}({}): {}\n```",
                        func.color,
                        func.name,
                        params.join(", "),
                        format_type_expr(&func.return_type),
                    );

                    if !doc_comment.is_empty() {
                        hover_content.push_str("\n\n---\n\n");
                        hover_content.push_str(&doc_comment);
                    }

                    return Some(HoverInfo {
                        contents: hover_content,
                        range: None,
                    });
                }
            }
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use nexus_parser::parse;

    fn parse_content(content: &str) -> Option<Program> {
        parse(content).ok()
    }

    #[test]
    fn test_hover_nested_calls() {
        // Test that in `outer(inner(x))`, hovering on `inner` shows `inner`'s hover info
        let content = r#"
std inner(): i64 { return 1 }
std outer(i64 x): i64 { return x }
std main(): void { m y = outer(inner()) }
"#;
        let ast = parse_content(content).unwrap();
        let mut documents = HashMap::new();
        documents.insert(
            "file:///test.nx".to_string(),
            (content.to_string(), Some(ast.clone())),
        );

        // Find the position of "inner()" inside "outer(inner())"
        let inner_pos = content.find("outer(inner").unwrap() + 6; // position of 'i' in inner

        let hover = find_hover(content, &ast, inner_pos, &documents);
        assert!(hover.is_some(), "Should find hover for nested inner()");
        let hover = hover.unwrap();
        // The hover should be for inner, not outer
        assert!(
            hover.contents.contains("inner"),
            "Hover should be for inner function"
        );
        assert!(
            !hover.contents.contains("outer") || hover.contents.contains("inner"),
            "Hover should primarily be about inner"
        );
    }

    #[test]
    fn test_hover_deeply_nested_calls() {
        // Test deeply nested calls: `a(b(c(x)))`
        let content = r#"
std c(): i64 { return 1 }
std b(i64 x): i64 { return x }
std a(i64 x): i64 { return x }
std main(): void { m y = a(b(c())) }
"#;
        let ast = parse_content(content).unwrap();
        let mut documents = HashMap::new();
        documents.insert(
            "file:///test.nx".to_string(),
            (content.to_string(), Some(ast.clone())),
        );

        // Find the position of "c()" inside "a(b(c()))"
        let c_pos = content.find("a(b(c").unwrap() + 4; // position of 'c'

        let hover = find_hover(content, &ast, c_pos, &documents);
        assert!(hover.is_some(), "Should find hover for deeply nested c()");
        let hover = hover.unwrap();
        assert!(
            hover.contents.contains("c()")
                || hover.contents.contains("c()")
                || hover.contents.contains("std c"),
            "Hover should be for c function"
        );
    }
}

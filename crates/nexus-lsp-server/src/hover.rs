//! Hover functionality for the LSP server.
//!
//! This module provides hover information for symbols in Nexus source code,
//! including function signatures, type information, documentation, and macro expansions.

use std::collections::HashMap;

use nexus_parser::{Expression, IfCondition, Item, Program, Statement};

use crate::builtins;
use crate::macro_expansion::{MacroExpansionContext, generate_macro_hover};
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

    // Look for macro definitions - show hover when on the name
    for item in &ast.items {
        if let Item::Macro(macro_def) = item {
            // Check if offset is on the macro name (after $)
            // The name starts at span.start + 1 (after $) and has length of name
            let name_start = macro_def.span.start + 1;
            let name_end = name_start + macro_def.name.len();
            if offset >= name_start && offset < name_end {
                let doc_comment = extract_doc_comment(content, &macro_def.span);

                let params: Vec<String> = macro_def
                    .params
                    .iter()
                    .map(|p| format!("{} {}", format_type_expr(&p.ty), p.name))
                    .collect();

                let mut hover_content = format!(
                    "```nexus\n${}({}): macro\n```",
                    macro_def.name,
                    params.join(", ")
                );

                if !doc_comment.is_empty() {
                    hover_content.push_str("\n\n---\n\n");
                    hover_content.push_str(&doc_comment);
                }

                return Some(HoverInfo {
                    contents: hover_content,
                    range: Some(span_to_range(content, &macro_def.span)),
                });
            }
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

    // Build macro expansion context from all available programs
    let mut programs_for_macros: Vec<&Program> = vec![ast];
    for (_, (_, maybe_prog)) in documents.iter() {
        if let Some(prog) = maybe_prog {
            programs_for_macros.push(prog);
        }
    }
    let macro_ctx = MacroExpansionContext::from_programs(programs_for_macros);

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
                &macro_ctx,
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
                &macro_ctx,
            )
        {
            return Some(hover);
        }
        if let Item::Macro(macro_def) = item
            && macro_def.body.span.contains(offset)
            && let Some(hover) = find_hover_in_statements(
                content,
                &macro_def.body.statements,
                offset,
                &imports,
                documents,
                &macro_ctx,
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
    macro_ctx: &MacroExpansionContext,
) -> Option<HoverInfo> {
    for stmt in statements {
        if let Some(hover) =
            find_hover_in_statement(content, stmt, offset, imports, documents, macro_ctx)
        {
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
    macro_ctx: &MacroExpansionContext,
) -> Option<HoverInfo> {
    match stmt {
        Statement::VarDecl(var_decl) => find_hover_in_expression(
            content,
            &var_decl.init,
            offset,
            imports,
            documents,
            macro_ctx,
        ),
        Statement::Assignment(assign) => find_hover_in_expression(
            content,
            &assign.value,
            offset,
            imports,
            documents,
            macro_ctx,
        ),
        Statement::Expression(expr) => {
            find_hover_in_expression(content, expr, offset, imports, documents, macro_ctx)
        }
        Statement::Return(ret) => {
            if let Some(ref val) = ret.value {
                find_hover_in_expression(content, val, offset, imports, documents, macro_ctx)
            } else {
                None
            }
        }
        Statement::If(if_stmt) => {
            // Check condition
            match &if_stmt.condition {
                IfCondition::Boolean(expr) => {
                    if let Some(hover) = find_hover_in_expression(
                        content, expr, offset, imports, documents, macro_ctx,
                    ) {
                        return Some(hover);
                    }
                }
                IfCondition::Pattern { matcher, .. } => {
                    if let Some(hover) = find_hover_in_expression(
                        content, matcher, offset, imports, documents, macro_ctx,
                    ) {
                        return Some(hover);
                    }
                }
            }

            // Check then block
            if let Some(hover) = find_hover_in_statements(
                content,
                &if_stmt.then_block.statements,
                offset,
                imports,
                documents,
                macro_ctx,
            ) {
                return Some(hover);
            }

            // Check else clause
            if let Some(else_clause) = &if_stmt.else_block {
                return find_hover_in_else_clause(
                    content,
                    else_clause,
                    offset,
                    imports,
                    documents,
                    macro_ctx,
                );
            }

            None
        }
        Statement::Block(block) => find_hover_in_statements(
            content,
            &block.statements,
            offset,
            imports,
            documents,
            macro_ctx,
        ),
        Statement::Subscope(subscope) => find_hover_in_statements(
            content,
            &subscope.body.statements,
            offset,
            imports,
            documents,
            macro_ctx,
        ),
        Statement::Defer(defer) => find_hover_in_statements(
            content,
            &defer.body.statements,
            offset,
            imports,
            documents,
            macro_ctx,
        ),
        _ => None,
    }
}

fn find_hover_in_else_clause(
    content: &str,
    else_clause: &nexus_parser::ElseClause,
    offset: usize,
    imports: &HashMap<String, (Vec<String>, bool)>,
    documents: &HashMap<String, (String, Option<Program>)>,
    macro_ctx: &MacroExpansionContext,
) -> Option<HoverInfo> {
    match else_clause {
        nexus_parser::ElseClause::Block(block) => find_hover_in_statements(
            content,
            &block.statements,
            offset,
            imports,
            documents,
            macro_ctx,
        ),
        nexus_parser::ElseClause::ElseIf(if_stmt) => {
            // Check condition
            match &if_stmt.condition {
                IfCondition::Boolean(expr) => {
                    if let Some(hover) = find_hover_in_expression(
                        content, expr, offset, imports, documents, macro_ctx,
                    ) {
                        return Some(hover);
                    }
                }
                IfCondition::Pattern { matcher, .. } => {
                    if let Some(hover) = find_hover_in_expression(
                        content, matcher, offset, imports, documents, macro_ctx,
                    ) {
                        return Some(hover);
                    }
                }
            }

            // Check then block
            if let Some(hover) = find_hover_in_statements(
                content,
                &if_stmt.then_block.statements,
                offset,
                imports,
                documents,
                macro_ctx,
            ) {
                return Some(hover);
            }

            // Check else clause
            if let Some(else_block) = &if_stmt.else_block {
                return find_hover_in_else_clause(
                    content, else_block, offset, imports, documents, macro_ctx,
                );
            }

            None
        }
    }
}

fn find_hover_in_expression(
    content: &str,
    expr: &Expression,
    offset: usize,
    imports: &HashMap<String, (Vec<String>, bool)>,
    documents: &HashMap<String, (String, Option<Program>)>,
    macro_ctx: &MacroExpansionContext,
) -> Option<HoverInfo> {
    match expr {
        Expression::Call(call) => {
            // IMPORTANT: Check arguments FIRST before the outer function.
            // This ensures that in nested calls like `foo(bar(x))`, if the cursor
            // is on `bar`, we show `bar`'s hover info, not `foo`'s.
            for arg in &call.args {
                if arg.span().contains(offset)
                    && let Some(hover) = find_hover_in_expression(
                        content, arg, offset, imports, documents, macro_ctx,
                    )
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
                    && let Some(hover) = find_hover_in_expression(
                        content, arg, offset, imports, documents, macro_ctx,
                    )
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
                    macro_ctx,
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
                    find_hover_in_expression(content, elem, offset, imports, documents, macro_ctx)
                {
                    return Some(hover);
                }
            }
            None
        }
        Expression::Index(index) => {
            if let Some(hover) = find_hover_in_expression(
                content,
                &index.array,
                offset,
                imports,
                documents,
                macro_ctx,
            ) {
                return Some(hover);
            }
            find_hover_in_expression(content, &index.index, offset, imports, documents, macro_ctx)
        }
        Expression::FieldAccess(field_access) => find_hover_in_expression(
            content,
            &field_access.object,
            offset,
            imports,
            documents,
            macro_ctx,
        ),
        Expression::Grouped(inner, _) => {
            find_hover_in_expression(content, inner, offset, imports, documents, macro_ctx)
        }
        Expression::MacroCall(macro_call) => {
            // Check if cursor is on the macro name itself (after $ and before ()
            // The macro call span starts at $, so name starts at span.start + 1
            let name_start = macro_call.span.start + 1;
            let name_end = name_start + macro_call.name.len();

            if offset >= name_start && offset < name_end {
                // Cursor is on the macro name - show macro info with expansion
                if let Some(macro_def) = macro_ctx.get_macro(&macro_call.name) {
                    let doc_comment = extract_doc_comment(content, &macro_def.span);
                    let expansion_result =
                        macro_ctx.expand_macro(&macro_call.name, &macro_call.args);
                    let hover_content =
                        generate_macro_hover(macro_def, &expansion_result, &doc_comment);

                    return Some(HoverInfo {
                        contents: hover_content,
                        range: Some(span_to_range(content, &macro_call.span)),
                    });
                }
            }

            // Check arguments
            for arg in &macro_call.args {
                if let Some(hover) =
                    find_hover_in_expression(content, arg, offset, imports, documents, macro_ctx)
                {
                    return Some(hover);
                }
            }
            None
        }
        Expression::Lambda(lambda) => match &lambda.body {
            nexus_parser::LambdaBody::Expression(expr) => {
                find_hover_in_expression(content, expr, offset, imports, documents, macro_ctx)
            }
            nexus_parser::LambdaBody::Block(block) => find_hover_in_statements(
                content,
                &block.statements,
                offset,
                imports,
                documents,
                macro_ctx,
            ),
        },
        _ => None,
    }
}

/// Get hover information for a function by name.
fn get_function_hover(
    name: &str,
    imports: &HashMap<String, (Vec<String>, bool)>,
    documents: &HashMap<String, (String, Option<Program>)>,
) -> Option<HoverInfo> {
    // First check if it's a builtin
    if let Some(builtin_info) = builtins::get_any_builtin(name) {
        return Some(HoverInfo {
            contents: builtin_info.hover_content(),
            range: None,
        });
    }

    // Check if it's an imported function
    if let Some((module_path, _)) = imports.get(name) {
        // Try to find the function in the imported module's document
        for (uri, (doc_content, maybe_ast)) in documents {
            // Check if this document is the imported module
            if uri.contains(&module_path.join("/")) || uri.contains(&module_path.join("\\")) {
                if let Some(ast) = maybe_ast {
                    for func in ast.functions() {
                        if func.name == name {
                            let params: Vec<String> =
                                func.params.iter().map(format_parameter).collect();
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
        }
    }

    // Try to find the function in any of the loaded documents
    for (_, (doc_content, maybe_ast)) in documents {
        if let Some(ast) = maybe_ast {
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
    use std::collections::HashMap;

    fn parse_content(content: &str) -> Option<Program> {
        parse(content).ok()
    }

    #[test]
    fn test_hover_nested_calls() {
        // Test that hovering on inner function shows inner function's info
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
        // Content: "std inner(): i64 { return 1 } std outer(i64 x): i64 { return x } std main(): void { m y = outer(inner()) }"
        // The inner() call is around position 95-100

        let inner_pos = content.rfind("inner()").unwrap() + 2; // Position inside "inner"

        let hover = find_hover(content, &ast, inner_pos, &documents);
        assert!(hover.is_some());
        let hover = hover.unwrap();
        // Should show inner's info, not outer's
        assert!(hover.contents.contains("inner"));
        assert!(!hover.contents.contains("outer"));
    }

    #[test]
    fn test_hover_deeply_nested_calls() {
        let content = r#"
std a(): i64 { return 1 }
std b(i64 x): i64 { return x }
std c(i64 x): i64 { return x }
std main(): void { m y = c(b(a())) }
"#;
        let ast = parse_content(content).unwrap();
        let mut documents = HashMap::new();
        documents.insert(
            "file:///test.nx".to_string(),
            (content.to_string(), Some(ast.clone())),
        );

        // Find position of "a()" in "c(b(a()))"
        let a_pos = content.rfind("a()").unwrap() + 1;

        let hover = find_hover(content, &ast, a_pos, &documents);
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(hover.contents.contains("a"));
    }

    #[test]
    fn test_hover_on_macro_call() {
        let content = r#"
$test([dyn]rune input): macro {
    return input
}

std main(): void {
    m x = $test("hello")
}
"#;
        let ast = parse_content(content).unwrap();
        let documents = HashMap::new();

        // Find position of "test" in "$test("hello")"
        let macro_pos = content.rfind("$test").unwrap() + 2; // Position on "test"

        let hover = find_hover(content, &ast, macro_pos, &documents);
        assert!(hover.is_some());
        let hover = hover.unwrap();
        assert!(hover.contents.contains("$test"));
        assert!(hover.contents.contains("macro"));
    }

    #[test]
    fn test_hover_on_macro_call_shows_expansion() {
        let content = r#"
$identity([dyn]rune input): macro {
    return input
}

std main(): void {
    m x = $identity("hello world")
}
"#;
        let ast = parse_content(content).unwrap();
        let documents = HashMap::new();

        // Find position of "identity" in "$identity("hello world")"
        let macro_pos = content.rfind("$identity").unwrap() + 2;

        let hover = find_hover(content, &ast, macro_pos, &documents);
        assert!(hover.is_some());
        let hover = hover.unwrap();

        // Should show the macro signature
        assert!(hover.contents.contains("$identity"));
        assert!(hover.contents.contains("input"));
        assert!(hover.contents.contains("macro"));

        // Should show the expanded code section
        assert!(hover.contents.contains("Macro Expansion"));
        // The expansion of $identity("hello world") should be "hello world"
        assert!(hover.contents.contains("hello world"));
    }
}

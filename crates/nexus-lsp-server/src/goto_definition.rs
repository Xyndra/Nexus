//! Go to definition functionality for the LSP server.
//!
//! This module provides navigation to symbol definitions in Nexus source code,
//! supporting functions, structs, and cross-file resolution.

use std::collections::HashMap;

use nexus_parser::{Expression, IfCondition, Item, Program, Statement};

use crate::types::Location;
use crate::utils::span_to_range;

/// Find the definition location for a symbol at the given offset.
pub fn find_definition(
    uri: &str,
    content: &str,
    ast: &Program,
    offset: usize,
    documents: &HashMap<String, (String, Option<Program>)>,
) -> Option<Location> {
    // Collect imports from this document for cross-file resolution
    let imports = collect_imports(ast);

    // Find what's at the cursor position by walking the AST
    // Look for function calls, variable references, type references, etc.

    // Check function calls - find the function definition
    for item in &ast.items {
        if let Item::Function(func) = item {
            // Check if cursor is in function body
            if func.body.span.contains(offset) {
                // Search for call expressions in this function
                for stmt in &func.body.statements {
                    if let Some(loc) = find_definition_in_statement(
                        uri, content, stmt, offset, ast, &imports, documents,
                    ) {
                        return Some(loc);
                    }
                }
            }
        }
        if let Item::Method(method) = item
            && method.body.span.contains(offset)
        {
            for stmt in &method.body.statements {
                if let Some(loc) = find_definition_in_statement(
                    uri, content, stmt, offset, ast, &imports, documents,
                ) {
                    return Some(loc);
                }
            }
        }
        if let Item::Macro(macro_def) = item
            && macro_def.body.span.contains(offset)
        {
            for stmt in &macro_def.body.statements {
                if let Some(loc) = find_definition_in_statement(
                    uri, content, stmt, offset, ast, &imports, documents,
                ) {
                    return Some(loc);
                }
            }
        }
    }

    None
}

/// Collect imports from the AST.
/// Returns a map of symbol name -> (module_path, is_module_import).
fn collect_imports(ast: &Program) -> HashMap<String, (Vec<String>, bool)> {
    let mut imports = HashMap::new();

    for item in &ast.items {
        if let Item::Use(use_stmt) = item {
            if use_stmt.symbols.is_empty() {
                // Module-level import like `use mathlib`
                // The module name is the last component of the path
                if let Some(module_name) = use_stmt.module_path.last() {
                    imports.insert(module_name.clone(), (use_stmt.module_path.clone(), true));
                }
            } else {
                // Symbol imports like `use { foo, bar } from module`
                for symbol in &use_stmt.symbols {
                    imports.insert(symbol.clone(), (use_stmt.module_path.clone(), false));
                }
            }
        }
    }

    imports
}

/// Try to find a function definition across all open documents.
fn find_function_in_all_documents(
    name: &str,
    documents: &HashMap<String, (String, Option<Program>)>,
) -> Option<Location> {
    for (doc_uri, (content, ast_opt)) in documents {
        if let Some(ast) = ast_opt {
            for func in ast.functions() {
                if func.name == name {
                    return Some(Location {
                        uri: doc_uri.clone(),
                        range: span_to_range(content, &func.name_span),
                    });
                }
            }
        }
    }
    None
}

/// Try to find a struct definition across all open documents.
fn find_struct_in_all_documents(
    name: &str,
    documents: &HashMap<String, (String, Option<Program>)>,
) -> Option<Location> {
    for (doc_uri, (content, ast_opt)) in documents {
        if let Some(ast) = ast_opt {
            for struct_def in ast.structs() {
                if struct_def.name == name {
                    return Some(Location {
                        uri: doc_uri.clone(),
                        range: span_to_range(content, &struct_def.name_span),
                    });
                }
            }
        }
    }
    None
}

/// Try to find a macro definition across all open documents.
fn find_macro_in_all_documents(
    name: &str,
    documents: &HashMap<String, (String, Option<Program>)>,
) -> Option<Location> {
    for (doc_uri, (content, ast_opt)) in documents {
        if let Some(ast) = ast_opt {
            for item in &ast.items {
                if let Item::Macro(macro_def) = item
                    && macro_def.name == name {
                        // The macro name starts after the $ sign
                        let name_start = macro_def.span.start + 1;
                        let name_end = name_start + macro_def.name.len();
                        let name_span = nexus_core::Span {
                            start: name_start,
                            end: name_end,
                            line: macro_def.span.line,
                            column: macro_def.span.column + 1,
                        };
                        return Some(Location {
                            uri: doc_uri.clone(),
                            range: span_to_range(content, &name_span),
                        });
                    }
            }
        }
    }
    None
}

/// Try to find a variable definition in a function body.
/// Returns the location of the variable declaration if found.
fn find_variable_in_function(
    uri: &str,
    content: &str,
    var_name: &str,
    statements: &[Statement],
) -> Option<Location> {
    for stmt in statements {
        match stmt {
            Statement::VarDecl(var_decl) => {
                if var_decl.name == var_name {
                    return Some(Location {
                        uri: uri.to_string(),
                        range: span_to_range(content, &var_decl.span),
                    });
                }
            }
            Statement::If(if_stmt) => {
                // Check in then block
                if let Some(loc) = find_variable_in_function(
                    uri,
                    content,
                    var_name,
                    &if_stmt.then_block.statements,
                ) {
                    return Some(loc);
                }
                // Check in else block
                if let Some(else_clause) = &if_stmt.else_block
                    && let Some(loc) =
                        find_variable_in_else_clause(uri, content, var_name, else_clause)
                    {
                        return Some(loc);
                    }
            }
            Statement::Subscope(subscope) => {
                if let Some(loc) =
                    find_variable_in_function(uri, content, var_name, &subscope.body.statements)
                {
                    return Some(loc);
                }
            }
            Statement::Block(block) => {
                if let Some(loc) =
                    find_variable_in_function(uri, content, var_name, &block.statements)
                {
                    return Some(loc);
                }
            }
            Statement::Defer(defer) => {
                if let Some(loc) =
                    find_variable_in_function(uri, content, var_name, &defer.body.statements)
                {
                    return Some(loc);
                }
            }
            _ => {}
        }
    }
    None
}

/// Helper for finding variables in else clauses.
fn find_variable_in_else_clause(
    uri: &str,
    content: &str,
    var_name: &str,
    else_clause: &nexus_parser::ElseClause,
) -> Option<Location> {
    match else_clause {
        nexus_parser::ElseClause::Block(block) => {
            find_variable_in_function(uri, content, var_name, &block.statements)
        }
        nexus_parser::ElseClause::ElseIf(if_stmt) => {
            if let Some(loc) =
                find_variable_in_function(uri, content, var_name, &if_stmt.then_block.statements)
            {
                return Some(loc);
            }
            if let Some(else_clause) = &if_stmt.else_block {
                find_variable_in_else_clause(uri, content, var_name, else_clause)
            } else {
                None
            }
        }
    }
}

/// Find definition in a statement.
fn find_definition_in_statement(
    uri: &str,
    content: &str,
    stmt: &Statement,
    offset: usize,
    ast: &Program,
    imports: &HashMap<String, (Vec<String>, bool)>,
    documents: &HashMap<String, (String, Option<Program>)>,
) -> Option<Location> {
    match stmt {
        Statement::Expression(expr) => {
            find_definition_in_expression(uri, content, expr, offset, ast, imports, documents)
        }
        Statement::VarDecl(var_decl) => find_definition_in_expression(
            uri,
            content,
            &var_decl.init,
            offset,
            ast,
            imports,
            documents,
        ),
        Statement::Assignment(assignment) => {
            // Check both target and value expressions
            if let Some(loc) = find_definition_in_expression(
                uri,
                content,
                &assignment.target,
                offset,
                ast,
                imports,
                documents,
            ) {
                return Some(loc);
            }
            find_definition_in_expression(
                uri,
                content,
                &assignment.value,
                offset,
                ast,
                imports,
                documents,
            )
        }
        Statement::Return(ret) => {
            if let Some(value) = &ret.value {
                find_definition_in_expression(uri, content, value, offset, ast, imports, documents)
            } else {
                None
            }
        }
        Statement::If(if_stmt) => {
            // Check condition based on type
            if let IfCondition::Boolean(cond_expr) = &if_stmt.condition
                && let Some(loc) = find_definition_in_expression(
                    uri, content, cond_expr, offset, ast, imports, documents,
                )
            {
                return Some(loc);
            }
            // Check then block
            for inner_stmt in &if_stmt.then_block.statements {
                if let Some(loc) = find_definition_in_statement(
                    uri, content, inner_stmt, offset, ast, imports, documents,
                ) {
                    return Some(loc);
                }
            }
            // Check else block
            if let Some(else_clause) = &if_stmt.else_block
                && let Some(loc) = find_definition_in_else_clause(
                    uri,
                    content,
                    else_clause,
                    offset,
                    ast,
                    imports,
                    documents,
                )
            {
                return Some(loc);
            }
            None
        }
        Statement::Defer(defer_stmt) => {
            for inner_stmt in &defer_stmt.body.statements {
                if let Some(loc) = find_definition_in_statement(
                    uri, content, inner_stmt, offset, ast, imports, documents,
                ) {
                    return Some(loc);
                }
            }
            None
        }
        Statement::Subscope(subscope_stmt) => {
            for inner_stmt in &subscope_stmt.body.statements {
                if let Some(loc) = find_definition_in_statement(
                    uri, content, inner_stmt, offset, ast, imports, documents,
                ) {
                    return Some(loc);
                }
            }
            None
        }
        Statement::Goto(_) => None,
        Statement::Block(block) => {
            for inner_stmt in &block.statements {
                if let Some(loc) = find_definition_in_statement(
                    uri, content, inner_stmt, offset, ast, imports, documents,
                ) {
                    return Some(loc);
                }
            }
            None
        }
    }
}

/// Find definition in an else clause.
fn find_definition_in_else_clause(
    uri: &str,
    content: &str,
    else_clause: &nexus_parser::ElseClause,
    offset: usize,
    ast: &Program,
    imports: &HashMap<String, (Vec<String>, bool)>,
    documents: &HashMap<String, (String, Option<Program>)>,
) -> Option<Location> {
    match else_clause {
        nexus_parser::ElseClause::Block(block) => {
            for inner_stmt in &block.statements {
                if let Some(loc) = find_definition_in_statement(
                    uri, content, inner_stmt, offset, ast, imports, documents,
                ) {
                    return Some(loc);
                }
            }
            None
        }
        nexus_parser::ElseClause::ElseIf(if_stmt) => {
            // Check condition
            if let IfCondition::Boolean(cond_expr) = &if_stmt.condition
                && let Some(loc) = find_definition_in_expression(
                    uri, content, cond_expr, offset, ast, imports, documents,
                )
            {
                return Some(loc);
            }
            // Check then block
            for inner_stmt in &if_stmt.then_block.statements {
                if let Some(loc) = find_definition_in_statement(
                    uri, content, inner_stmt, offset, ast, imports, documents,
                ) {
                    return Some(loc);
                }
            }
            // Recursively check else
            if let Some(inner_else) = &if_stmt.else_block
                && let Some(loc) = find_definition_in_else_clause(
                    uri, content, inner_else, offset, ast, imports, documents,
                )
            {
                return Some(loc);
            }
            None
        }
    }
}

/// Find definition in an expression.
fn find_definition_in_expression(
    uri: &str,
    content: &str,
    expr: &Expression,
    offset: usize,
    ast: &Program,
    imports: &HashMap<String, (Vec<String>, bool)>,
    documents: &HashMap<String, (String, Option<Program>)>,
) -> Option<Location> {
    match expr {
        Expression::Call(call) => {
            // IMPORTANT: Check arguments FIRST before the outer function.
            // This ensures that in nested calls like `foo(bar(x))`, if the cursor
            // is on `bar`, we find `bar`'s definition, not `foo`'s.
            for arg in &call.args {
                if arg.span().contains(offset)
                    && let Some(loc) = find_definition_in_expression(
                        uri, content, arg, offset, ast, imports, documents,
                    )
                {
                    return Some(loc);
                }
            }

            // Now check if the cursor is on the function name part of the call
            if call.span.contains(offset) {
                // First, try to find in current document
                for func in ast.functions() {
                    if func.name == call.function {
                        return Some(Location {
                            uri: uri.to_string(),
                            range: span_to_range(content, &func.name_span),
                        });
                    }
                }

                // Check if this is an imported symbol
                if imports.contains_key(&call.function) {
                    // Try to find in all open documents
                    if let Some(loc) = find_function_in_all_documents(&call.function, documents) {
                        return Some(loc);
                    }
                }

                // Fallback: search all open documents anyway
                if let Some(loc) = find_function_in_all_documents(&call.function, documents) {
                    return Some(loc);
                }
            }
            None
        }
        Expression::Variable(var_ref) => {
            // Check if cursor is on the variable reference
            if var_ref.span.contains(offset) {
                // Search for variable definition in current function/method
                for item in &ast.items {
                    match item {
                        Item::Function(func) => {
                            // Check function parameters first
                            for param in &func.params {
                                if param.name == var_ref.name {
                                    return Some(Location {
                                        uri: uri.to_string(),
                                        range: span_to_range(content, &param.span),
                                    });
                                }
                            }
                            // Check variable declarations in function body
                            if let Some(loc) = find_variable_in_function(
                                uri,
                                content,
                                &var_ref.name,
                                &func.body.statements,
                            ) {
                                return Some(loc);
                            }
                        }
                        Item::Method(method) => {
                            // Check receiver
                            if method.receiver_name == var_ref.name {
                                // Use the method's span as we don't have a separate receiver_span
                                return Some(Location {
                                    uri: uri.to_string(),
                                    range: span_to_range(content, &method.span),
                                });
                            }
                            // Check method parameters
                            for param in &method.params {
                                if param.name == var_ref.name {
                                    return Some(Location {
                                        uri: uri.to_string(),
                                        range: span_to_range(content, &param.span),
                                    });
                                }
                            }
                            // Check variable declarations in method body
                            if let Some(loc) = find_variable_in_function(
                                uri,
                                content,
                                &var_ref.name,
                                &method.body.statements,
                            ) {
                                return Some(loc);
                            }
                        }
                        Item::Macro(macro_def) => {
                            // Check macro parameters
                            for param in &macro_def.params {
                                if param.name == var_ref.name {
                                    return Some(Location {
                                        uri: uri.to_string(),
                                        range: span_to_range(content, &param.span),
                                    });
                                }
                            }
                            // Check variable declarations in macro body
                            if let Some(loc) = find_variable_in_function(
                                uri,
                                content,
                                &var_ref.name,
                                &macro_def.body.statements,
                            ) {
                                return Some(loc);
                            }
                        }
                        _ => {}
                    }
                }
            }
            None
        }
        Expression::StructInit(struct_init) => {
            // Check if cursor is on the struct name
            if struct_init.span.contains(offset) {
                // First check current document
                for struct_def in ast.structs() {
                    if struct_def.name == struct_init.name {
                        return Some(Location {
                            uri: uri.to_string(),
                            range: span_to_range(content, &struct_def.name_span),
                        });
                    }
                }

                // Try all open documents
                if let Some(loc) = find_struct_in_all_documents(&struct_init.name, documents) {
                    return Some(loc);
                }
            }
            None
        }
        Expression::MethodCall(method_call) => {
            // Check arguments first (same principle as Call)
            for arg in &method_call.args {
                if arg.span().contains(offset)
                    && let Some(loc) = find_definition_in_expression(
                        uri, content, arg, offset, ast, imports, documents,
                    )
                {
                    return Some(loc);
                }
            }

            // Check receiver
            if method_call.receiver.span().contains(offset)
                && let Some(loc) = find_definition_in_expression(
                    uri,
                    content,
                    &method_call.receiver,
                    offset,
                    ast,
                    imports,
                    documents,
                )
            {
                return Some(loc);
            }

            // Check if this is a module-qualified call like `mathlib.abs()`
            if method_call.span.contains(offset) {
                // Check if the receiver is a variable that matches a module import
                if let Expression::Variable(var_ref) = method_call.receiver.as_ref()
                    && let Some((_, is_module)) = imports.get(&var_ref.name)
                    && *is_module
                {
                    // This is a module-qualified call, search for the method as a function
                    if let Some(loc) =
                        find_function_in_all_documents(&method_call.method, documents)
                    {
                        return Some(loc);
                    }
                }
            }

            None
        }
        Expression::FieldAccess(field_access) => find_definition_in_expression(
            uri,
            content,
            &field_access.object,
            offset,
            ast,
            imports,
            documents,
        ),
        Expression::Index(index) => {
            if let Some(loc) = find_definition_in_expression(
                uri,
                content,
                &index.array,
                offset,
                ast,
                imports,
                documents,
            ) {
                return Some(loc);
            }
            find_definition_in_expression(
                uri,
                content,
                &index.index,
                offset,
                ast,
                imports,
                documents,
            )
        }
        Expression::Array(array) => {
            for elem in &array.elements {
                if let Some(loc) = find_definition_in_expression(
                    uri, content, elem, offset, ast, imports, documents,
                ) {
                    return Some(loc);
                }
            }
            None
        }
        Expression::Grouped(inner, _) => {
            find_definition_in_expression(uri, content, inner, offset, ast, imports, documents)
        }
        Expression::MacroCall(macro_call) => {
            // Check if cursor is on the macro name
            let name_start = macro_call.span.start + 1; // After $
            let name_end = name_start + macro_call.name.len();
            if offset >= name_start && offset < name_end {
                // Try to find macro definition in current document
                for item in &ast.items {
                    if let Item::Macro(macro_def) = item
                        && macro_def.name == macro_call.name {
                            let def_name_start = macro_def.span.start + 1;
                            let def_name_end = def_name_start + macro_def.name.len();
                            let name_span = nexus_core::Span {
                                start: def_name_start,
                                end: def_name_end,
                                line: macro_def.span.line,
                                column: macro_def.span.column + 1,
                            };
                            return Some(Location {
                                uri: uri.to_string(),
                                range: span_to_range(content, &name_span),
                            });
                        }
                }
                // Try to find in all documents
                if let Some(loc) = find_macro_in_all_documents(&macro_call.name, documents) {
                    return Some(loc);
                }
            }

            // Check arguments of macro calls
            for arg in &macro_call.args {
                if let Some(loc) = find_definition_in_expression(
                    uri, content, arg, offset, ast, imports, documents,
                ) {
                    return Some(loc);
                }
            }
            None
        }
        Expression::Lambda(lambda) => {
            // Check inside lambda body
            match &lambda.body {
                nexus_parser::LambdaBody::Expression(expr) => find_definition_in_expression(
                    uri, content, expr, offset, ast, imports, documents,
                ),
                nexus_parser::LambdaBody::Block(block) => {
                    for inner_stmt in &block.statements {
                        if let Some(loc) = find_definition_in_statement(
                            uri, content, inner_stmt, offset, ast, imports, documents,
                        ) {
                            return Some(loc);
                        }
                    }
                    None
                }
            }
        }
        Expression::Literal(_) => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_content(content: &str) -> Option<Program> {
        nexus_parser::parse(content).ok()
    }

    #[test]
    fn test_goto_definition_same_file() {
        let content = r#"
std helper(): i64 { return 42 }
std main(): void { helper() }
"#;
        let ast = parse_content(content).unwrap();
        let documents = HashMap::new();

        // Find the call to helper() and go to its definition
        // The call is around position 50
        let loc = find_definition("file:///test.nx", content, &ast, 55, &documents);
        assert!(loc.is_some());
        let loc = loc.unwrap();
        assert_eq!(loc.uri, "file:///test.nx");
    }

    #[test]
    fn test_goto_definition_cross_file() {
        let content1 = "std helper(): i64 { return 42 }";
        let content2 = r#"
use { helper } from other
std main(): void { helper() }
"#;
        let ast1 = parse_content(content1).unwrap();
        let ast2 = parse_content(content2).unwrap();

        let mut documents = HashMap::new();
        documents.insert(
            "file:///other.nx".to_string(),
            (content1.to_string(), Some(ast1)),
        );
        documents.insert(
            "file:///main.nx".to_string(),
            (content2.to_string(), Some(ast2.clone())),
        );

        // Find the call to helper() in the second file
        let loc = find_definition("file:///main.nx", content2, &ast2, 50, &documents);
        assert!(loc.is_some());
        let loc = loc.unwrap();
        assert_eq!(loc.uri, "file:///other.nx");
    }

    #[test]
    fn test_goto_definition_nested_calls() {
        // Test that in `outer(inner(x))`, hovering on `inner` goes to `inner`'s definition
        let content = r#"
std inner(): i64 { return 1 }
std outer(i64 x): i64 { return x }
std main(): void { m y = outer(inner()) }
"#;
        let ast = parse_content(content).unwrap();
        let documents = HashMap::new();

        // Find the position of "inner()" inside "outer(inner())"
        // The content structure is:
        // Line 0: empty
        // Line 1: std inner(): i64 { return 1 }
        // Line 2: std outer(i64 x): i64 { return x }
        // Line 3: std main(): void { m y = outer(inner()) }
        //                                       ^ inner starts around here

        // Find the offset for "inner" in the call
        let inner_pos = content.find("outer(inner").unwrap() + 6; // position of 'i' in inner

        let loc = find_definition("file:///test.nx", content, &ast, inner_pos, &documents);
        assert!(loc.is_some(), "Should find definition for nested inner()");
        let loc = loc.unwrap();
        assert_eq!(loc.uri, "file:///test.nx");

        // The definition should point to the inner function, not outer
        // We can verify by checking the range points to line 1 where inner is defined
        assert_eq!(
            loc.range.start.line, 1,
            "Should point to inner function on line 1"
        );
    }

    #[test]
    fn test_goto_definition_deeply_nested_calls() {
        // Test deeply nested calls: `a(b(c(x)))`
        let content = r#"
std c(): i64 { return 1 }
std b(i64 x): i64 { return x }
std a(i64 x): i64 { return x }
std main(): void { m y = a(b(c())) }
"#;
        let ast = parse_content(content).unwrap();
        let documents = HashMap::new();

        // Find the position of "c()" inside "a(b(c()))"
        let c_pos = content.find("a(b(c").unwrap() + 4; // position of 'c'

        let loc = find_definition("file:///test.nx", content, &ast, c_pos, &documents);
        assert!(
            loc.is_some(),
            "Should find definition for deeply nested c()"
        );
        let loc = loc.unwrap();
        assert_eq!(
            loc.range.start.line, 1,
            "Should point to c function on line 1"
        );

        // Now test finding b()
        let b_pos = content.find("a(b(c").unwrap() + 2; // position of 'b'
        let loc = find_definition("file:///test.nx", content, &ast, b_pos, &documents);
        assert!(loc.is_some(), "Should find definition for nested b()");
        let loc = loc.unwrap();
        assert_eq!(
            loc.range.start.line, 2,
            "Should point to b function on line 2"
        );
    }

    #[test]
    fn test_goto_definition_same_module_without_import() {
        // Test that goto-definition works for functions in the same module without imports
        // File 1: defines a helper function
        let content1 = "std helper(): i64 { return 42 }";
        let ast1 = parse_content(content1).unwrap();

        // File 2: calls helper without importing it (same module)
        let content2 = "std main(): void { m x = helper() }";
        let ast2 = parse_content(content2).unwrap();

        // Both files are in the same directory (same module)
        let mut documents = HashMap::new();
        documents.insert(
            "file:///D:/project/mymodule/helper.nx".to_string(),
            (content1.to_string(), Some(ast1)),
        );
        documents.insert(
            "file:///D:/project/mymodule/main.nx".to_string(),
            (content2.to_string(), Some(ast2.clone())),
        );

        // Find the position of "helper" in the call
        let helper_pos = content2.find("helper").unwrap();

        // This should find the helper function definition even without an import
        let loc = find_definition(
            "file:///D:/project/mymodule/main.nx",
            content2,
            &ast2,
            helper_pos,
            &documents,
        );
        assert!(
            loc.is_some(),
            "Should find definition for same-module function without import"
        );
        let loc = loc.unwrap();
        assert_eq!(loc.uri, "file:///D:/project/mymodule/helper.nx");
    }
}

//! Diagnostics functionality for the LSP server.
//!
//! This module provides diagnostic checking for Nexus source code,
//! including syntax errors and semantic analysis.

use std::collections::HashMap;

use nexus_core::Diagnostics;
use nexus_parser::{
    Block, CallExpr, ElseClause, Expression, GotoStmt, Item, LiteralKind, Program, Statement,
    SubscopeStmt, parse,
};

use crate::builtins::BUILTINS;

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

/// Information about a user-defined function for argument checking
#[derive(Debug, Clone)]
struct FunctionInfo {
    /// Number of parameters
    param_count: usize,
    /// Parameter types (as strings)
    param_types: Vec<String>,
}

/// Inferred type of an expression
#[derive(Debug, Clone, PartialEq)]
enum InferredType {
    I64,
    I32,
    I16,
    I8,
    U64,
    U32,
    U16,
    U8,
    F64,
    F32,
    Bool,
    Rune,
    String,
    Array(Box<InferredType>),
    Struct(String),
    Void,
    Unknown,
}

impl InferredType {
    /// Convert a type name string to InferredType
    fn from_type_name(name: &str) -> Self {
        match name {
            "i64" => InferredType::I64,
            "i32" => InferredType::I32,
            "i16" => InferredType::I16,
            "i8" => InferredType::I8,
            "u64" => InferredType::U64,
            "u32" => InferredType::U32,
            "u16" => InferredType::U16,
            "u8" => InferredType::U8,
            "f64" => InferredType::F64,
            "f32" => InferredType::F32,
            "bool" => InferredType::Bool,
            "rune" => InferredType::Rune,
            "string" | "[]rune" => InferredType::String,
            "void" => InferredType::Void,
            // "any" and "...any" (variadic) accept any type
            "any" | "...any" => InferredType::Unknown,
            _ => InferredType::Struct(name.to_string()),
        }
    }

    /// Check if this type is compatible with another type
    fn is_compatible_with(&self, other: &InferredType) -> bool {
        match (self, other) {
            (InferredType::Unknown, _) | (_, InferredType::Unknown) => true,
            (a, b) => a == b,
        }
    }
}

impl std::fmt::Display for InferredType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InferredType::I64 => write!(f, "i64"),
            InferredType::I32 => write!(f, "i32"),
            InferredType::I16 => write!(f, "i16"),
            InferredType::I8 => write!(f, "i8"),
            InferredType::U64 => write!(f, "u64"),
            InferredType::U32 => write!(f, "u32"),
            InferredType::U16 => write!(f, "u16"),
            InferredType::U8 => write!(f, "u8"),
            InferredType::F64 => write!(f, "f64"),
            InferredType::F32 => write!(f, "f32"),
            InferredType::Bool => write!(f, "bool"),
            InferredType::Rune => write!(f, "rune"),
            InferredType::String => write!(f, "string"),
            InferredType::Array(elem) => write!(f, "[]{}", elem),
            InferredType::Struct(name) => write!(f, "{}", name),
            InferredType::Void => write!(f, "void"),
            InferredType::Unknown => write!(f, "unknown"),
        }
    }
}

/// Infer the type of an expression
fn infer_expression_type(
    expr: &Expression,
    function_info: &HashMap<String, FunctionInfo>,
) -> InferredType {
    match expr {
        Expression::Literal(lit) => match &lit.kind {
            LiteralKind::Int(_) => InferredType::I64, // Default integer type
            LiteralKind::Float(_) => InferredType::F64, // Default float type
            LiteralKind::String(_) => InferredType::String,
            LiteralKind::Char(_) => InferredType::Rune,
            LiteralKind::Bool(_) => InferredType::Bool,
            LiteralKind::None => InferredType::Unknown,
        },
        Expression::Variable(_) => InferredType::Unknown, // Would need scope tracking
        Expression::Call(call) => {
            // Check user-defined functions first
            if let Some(_info) = function_info.get(&call.function) {
                // Would need return type tracking
                InferredType::Unknown
            } else if let Some(builtin) = BUILTINS.get(call.function.as_str()) {
                InferredType::from_type_name(builtin.return_type)
            } else {
                InferredType::Unknown
            }
        }
        Expression::Array(_) => InferredType::Array(Box::new(InferredType::Unknown)),
        Expression::MethodCall(_) => InferredType::Unknown,
        Expression::MacroCall(_) => InferredType::Unknown,
        Expression::FieldAccess(_) => InferredType::Unknown,
        Expression::Index(_) => InferredType::Unknown,
        Expression::StructInit(s) => InferredType::Struct(s.name.clone()),
        Expression::Lambda(_) => InferredType::Unknown,
        Expression::Grouped(inner, _) => infer_expression_type(inner, function_info),
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
        // Check for redefinitions at the top level
        check_top_level_redefinitions(program, &mut diagnostics);

        // Build function info map for argument checking
        let function_info = build_function_info_map(program);

        // Check each item
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    // Check for return statements with values inside subscopes
                    check_returns_in_block(&func.body, 0, &mut diagnostics);
                    // Check for duplicate subscope labels and goto validity
                    check_subscope_labels_in_block(&func.body, &mut diagnostics);
                    // Check for variable redefinitions within the function
                    check_variable_redefinitions_in_block(&func.body, &mut diagnostics);
                    // Check function call arguments
                    check_function_calls_in_block(&func.body, &function_info, &mut diagnostics);
                    // Check for extra content after goto statements
                    check_goto_followed_by_expression(&func.body, &mut diagnostics);
                }
                Item::Method(method) => {
                    // Check for return statements with values inside subscopes
                    check_returns_in_block(&method.body, 0, &mut diagnostics);
                    // Check for duplicate subscope labels and goto validity
                    check_subscope_labels_in_block(&method.body, &mut diagnostics);
                    // Check for variable redefinitions within the method
                    check_variable_redefinitions_in_block(&method.body, &mut diagnostics);
                    // Check function call arguments
                    check_function_calls_in_block(&method.body, &function_info, &mut diagnostics);
                    // Check for extra content after goto statements
                    check_goto_followed_by_expression(&method.body, &mut diagnostics);
                }
                _ => {}
            }
        }
    }

    diagnostics
}

/// Build a map of function names to their info for argument checking
fn build_function_info_map(program: &Program) -> HashMap<String, FunctionInfo> {
    let mut map = HashMap::new();

    for item in &program.items {
        match item {
            Item::Function(func) => {
                let param_types: Vec<String> = func
                    .params
                    .iter()
                    .map(|p| type_expr_to_string(&p.ty))
                    .collect();
                map.insert(
                    func.name.clone(),
                    FunctionInfo {
                        param_count: func.params.len(),
                        param_types,
                    },
                );
            }
            Item::Method(method) => {
                // Methods are called with receiver.method() syntax, so they're tracked differently
                // For now, we just track them by name (could be ambiguous with multiple types)
                let param_types: Vec<String> = method
                    .params
                    .iter()
                    .map(|p| type_expr_to_string(&p.ty))
                    .collect();
                map.insert(
                    method.name.clone(),
                    FunctionInfo {
                        param_count: method.params.len(),
                        param_types,
                    },
                );
            }
            _ => {}
        }
    }

    map
}

/// Convert a TypeExpr to a string representation
fn type_expr_to_string(ty: &nexus_parser::TypeExpr) -> String {
    match ty {
        nexus_parser::TypeExpr::Named { name, .. } => name.clone(),
        nexus_parser::TypeExpr::Array { element, .. } => {
            format!("[]{}", type_expr_to_string(element))
        }
        nexus_parser::TypeExpr::Void { .. } => "void".to_string(),
        nexus_parser::TypeExpr::Unknown { .. } => "unknown".to_string(),
        nexus_parser::TypeExpr::Function { .. } => "function".to_string(),
    }
}

/// Check for redefinitions at the top level (functions, structs, interfaces)
fn check_top_level_redefinitions(program: &Program, diagnostics: &mut Diagnostics) {
    let mut functions: HashMap<&str, &nexus_core::Span> = HashMap::new();
    let mut structs: HashMap<&str, &nexus_core::Span> = HashMap::new();
    let mut interfaces: HashMap<&str, &nexus_core::Span> = HashMap::new();

    for item in &program.items {
        match item {
            Item::Function(func) => {
                if let Some(prev_span) = functions.get(func.name.as_str()) {
                    diagnostics.error(
                        format!(
                            "Function '{}' is already defined at line {}",
                            func.name, prev_span.line
                        ),
                        func.name_span,
                    );
                } else {
                    functions.insert(&func.name, &func.name_span);
                }
            }
            Item::Method(method) => {
                // Methods can have the same name on different types, so we use type.method as key
                let key = format!("{}.{}", method.receiver_type, method.name);
                // For simplicity, we'll just check method name collisions within the same receiver type
                // This requires a different approach - let's skip for now since methods are type-scoped
                let _ = key;
            }
            Item::Struct(s) => {
                if let Some(prev_span) = structs.get(s.name.as_str()) {
                    diagnostics.error(
                        format!(
                            "Struct '{}' is already defined at line {}",
                            s.name, prev_span.line
                        ),
                        s.name_span,
                    );
                } else {
                    structs.insert(&s.name, &s.name_span);
                }
            }
            Item::Interface(i) => {
                if let Some(prev_span) = interfaces.get(i.name.as_str()) {
                    diagnostics.error(
                        format!(
                            "Interface '{}' is already defined at line {}",
                            i.name, prev_span.line
                        ),
                        i.name_span,
                    );
                } else {
                    interfaces.insert(&i.name, &i.name_span);
                }
            }
            _ => {}
        }
    }
}

/// Check for variable redefinitions within a block
fn check_variable_redefinitions_in_block(block: &Block, diagnostics: &mut Diagnostics) {
    let mut variables: HashMap<&str, &nexus_core::Span> = HashMap::new();

    for stmt in &block.statements {
        check_variable_redefinitions_in_statement(stmt, &mut variables, diagnostics);
    }
}

/// Check a statement for variable redefinitions
fn check_variable_redefinitions_in_statement<'a>(
    stmt: &'a Statement,
    variables: &mut HashMap<&'a str, &'a nexus_core::Span>,
    diagnostics: &mut Diagnostics,
) {
    match stmt {
        Statement::VarDecl(var) => {
            if let Some(prev_span) = variables.get(var.name.as_str()) {
                diagnostics.error(
                    format!(
                        "Variable '{}' is already defined at line {}",
                        var.name, prev_span.line
                    ),
                    var.span,
                );
            } else {
                variables.insert(&var.name, &var.span);
            }
        }
        Statement::If(if_stmt) => {
            // Check inside if/else blocks with fresh scopes
            check_variable_redefinitions_in_block(&if_stmt.then_block, diagnostics);
            if let Some(else_clause) = &if_stmt.else_block {
                check_variable_redefinitions_in_else_clause(else_clause, diagnostics);
            }
        }
        Statement::Subscope(subscope) => {
            // Subscopes don't create new variable scopes in Nexus - they're for control flow only
            // But nested blocks might, so we check recursively
            check_variable_redefinitions_in_block(&subscope.body, diagnostics);
        }
        Statement::Block(block) => {
            check_variable_redefinitions_in_block(block, diagnostics);
        }
        Statement::Defer(defer) => {
            check_variable_redefinitions_in_block(&defer.body, diagnostics);
        }
        _ => {}
    }
}

/// Check an else clause for variable redefinitions
fn check_variable_redefinitions_in_else_clause(
    else_clause: &ElseClause,
    diagnostics: &mut Diagnostics,
) {
    match else_clause {
        ElseClause::Block(block) => {
            check_variable_redefinitions_in_block(block, diagnostics);
        }
        ElseClause::ElseIf(else_if) => {
            check_variable_redefinitions_in_block(&else_if.then_block, diagnostics);
            if let Some(inner_else) = &else_if.else_block {
                check_variable_redefinitions_in_else_clause(inner_else, diagnostics);
            }
        }
    }
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
                check_returns_in_else_clause(else_clause, subscope_depth, diagnostics);
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
fn check_returns_in_else_clause(
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
                check_returns_in_else_clause(inner_else, subscope_depth, diagnostics);
            }
        }
    }
}

/// Check for duplicate subscope labels and validate goto targets in a block
fn check_subscope_labels_in_block(block: &Block, diagnostics: &mut Diagnostics) {
    // First, collect all subscope labels in this function body
    let mut labels: HashMap<&str, Vec<&SubscopeStmt>> = HashMap::new();
    collect_subscope_labels(block, &mut labels);

    // Check for duplicate labels
    for (name, subscopes) in &labels {
        if subscopes.len() > 1 {
            for subscope in subscopes.iter().skip(1) {
                diagnostics.error(
                    format!(
                        "Subscope label '{}' is already defined at line {}",
                        name, subscopes[0].span.line
                    ),
                    subscope.span,
                );
            }
        }
    }

    // Now check all goto statements reference valid labels
    check_goto_statements_in_block(block, &labels, diagnostics);
}

/// Collect all subscope labels in a block (recursively)
fn collect_subscope_labels<'a>(
    block: &'a Block,
    labels: &mut HashMap<&'a str, Vec<&'a SubscopeStmt>>,
) {
    for stmt in &block.statements {
        collect_subscope_labels_in_statement(stmt, labels);
    }
}

/// Collect subscope labels from a statement
fn collect_subscope_labels_in_statement<'a>(
    stmt: &'a Statement,
    labels: &mut HashMap<&'a str, Vec<&'a SubscopeStmt>>,
) {
    match stmt {
        Statement::Subscope(subscope) => {
            labels.entry(&subscope.name).or_default().push(subscope);
            // Also check inside the subscope for nested subscopes
            collect_subscope_labels(&subscope.body, labels);
        }
        Statement::If(if_stmt) => {
            collect_subscope_labels(&if_stmt.then_block, labels);
            if let Some(else_clause) = &if_stmt.else_block {
                collect_subscope_labels_in_else_clause(else_clause, labels);
            }
        }
        Statement::Block(block) => {
            collect_subscope_labels(block, labels);
        }
        Statement::Defer(defer) => {
            collect_subscope_labels(&defer.body, labels);
        }
        _ => {}
    }
}

/// Collect subscope labels from an else clause
fn collect_subscope_labels_in_else_clause<'a>(
    else_clause: &'a ElseClause,
    labels: &mut HashMap<&'a str, Vec<&'a SubscopeStmt>>,
) {
    match else_clause {
        ElseClause::Block(block) => {
            collect_subscope_labels(block, labels);
        }
        ElseClause::ElseIf(else_if) => {
            collect_subscope_labels(&else_if.then_block, labels);
            if let Some(inner_else) = &else_if.else_block {
                collect_subscope_labels_in_else_clause(inner_else, labels);
            }
        }
    }
}

/// Check goto statements reference valid labels
fn check_goto_statements_in_block(
    block: &Block,
    labels: &HashMap<&str, Vec<&SubscopeStmt>>,
    diagnostics: &mut Diagnostics,
) {
    for stmt in &block.statements {
        check_goto_statements_in_statement(stmt, labels, diagnostics);
    }
}

/// Check goto statements in a statement
fn check_goto_statements_in_statement(
    stmt: &Statement,
    labels: &HashMap<&str, Vec<&SubscopeStmt>>,
    diagnostics: &mut Diagnostics,
) {
    match stmt {
        Statement::Goto(goto) => {
            check_goto_target(goto, labels, diagnostics);
        }
        Statement::Subscope(subscope) => {
            check_goto_statements_in_block(&subscope.body, labels, diagnostics);
        }
        Statement::If(if_stmt) => {
            check_goto_statements_in_block(&if_stmt.then_block, labels, diagnostics);
            if let Some(else_clause) = &if_stmt.else_block {
                check_goto_statements_in_else_clause(else_clause, labels, diagnostics);
            }
        }
        Statement::Block(block) => {
            check_goto_statements_in_block(block, labels, diagnostics);
        }
        Statement::Defer(defer) => {
            check_goto_statements_in_block(&defer.body, labels, diagnostics);
        }
        _ => {}
    }
}

/// Check goto statements in an else clause
fn check_goto_statements_in_else_clause(
    else_clause: &ElseClause,
    labels: &HashMap<&str, Vec<&SubscopeStmt>>,
    diagnostics: &mut Diagnostics,
) {
    match else_clause {
        ElseClause::Block(block) => {
            check_goto_statements_in_block(block, labels, diagnostics);
        }
        ElseClause::ElseIf(else_if) => {
            check_goto_statements_in_block(&else_if.then_block, labels, diagnostics);
            if let Some(inner_else) = &else_if.else_block {
                check_goto_statements_in_else_clause(inner_else, labels, diagnostics);
            }
        }
    }
}

/// Check that a goto statement references exactly one valid label
fn check_goto_target(
    goto: &GotoStmt,
    labels: &HashMap<&str, Vec<&SubscopeStmt>>,
    diagnostics: &mut Diagnostics,
) {
    match labels.get(goto.label.as_str()) {
        None => {
            diagnostics.error(
                format!("Goto references undefined label '{}'", goto.label),
                goto.span,
            );
        }
        Some(subscopes) if subscopes.len() > 1 => {
            // This error is already reported when checking for duplicate labels,
            // but we can add additional context here
            diagnostics.error(
                format!(
                    "Goto references ambiguous label '{}' (defined {} times)",
                    goto.label,
                    subscopes.len()
                ),
                goto.span,
            );
        }
        Some(_) => {
            // Valid: exactly one label exists
        }
    }
}

/// Check function call arguments in a block
fn check_function_calls_in_block(
    block: &Block,
    function_info: &HashMap<String, FunctionInfo>,
    diagnostics: &mut Diagnostics,
) {
    for stmt in &block.statements {
        check_function_calls_in_statement(stmt, function_info, diagnostics);
    }
}

/// Check function calls in a statement
fn check_function_calls_in_statement(
    stmt: &Statement,
    function_info: &HashMap<String, FunctionInfo>,
    diagnostics: &mut Diagnostics,
) {
    match stmt {
        Statement::VarDecl(var) => {
            check_function_calls_in_expression(&var.init, function_info, diagnostics);
        }
        Statement::Assignment(assign) => {
            check_function_calls_in_expression(&assign.value, function_info, diagnostics);
        }
        Statement::Expression(expr) => {
            check_function_calls_in_expression(expr, function_info, diagnostics);
        }
        Statement::Return(ret) => {
            if let Some(value) = &ret.value {
                check_function_calls_in_expression(value, function_info, diagnostics);
            }
        }
        Statement::If(if_stmt) => {
            match &if_stmt.condition {
                nexus_parser::IfCondition::Boolean(expr) => {
                    check_function_calls_in_expression(expr, function_info, diagnostics);
                }
                nexus_parser::IfCondition::Pattern { matcher, cases } => {
                    check_function_calls_in_expression(matcher, function_info, diagnostics);
                    for case in cases {
                        match &case.body {
                            nexus_parser::PatternBody::Expression(expr) => {
                                check_function_calls_in_expression(
                                    expr,
                                    function_info,
                                    diagnostics,
                                );
                            }
                            nexus_parser::PatternBody::Block(block) => {
                                check_function_calls_in_block(block, function_info, diagnostics);
                            }
                        }
                    }
                }
            }
            check_function_calls_in_block(&if_stmt.then_block, function_info, diagnostics);
            if let Some(else_clause) = &if_stmt.else_block {
                check_function_calls_in_else_clause(else_clause, function_info, diagnostics);
            }
        }
        Statement::Subscope(subscope) => {
            check_function_calls_in_block(&subscope.body, function_info, diagnostics);
        }
        Statement::Block(block) => {
            check_function_calls_in_block(block, function_info, diagnostics);
        }
        Statement::Defer(defer) => {
            check_function_calls_in_block(&defer.body, function_info, diagnostics);
        }
        Statement::Goto(_) => {}
    }
}

/// Check function calls in an else clause
fn check_function_calls_in_else_clause(
    else_clause: &ElseClause,
    function_info: &HashMap<String, FunctionInfo>,
    diagnostics: &mut Diagnostics,
) {
    match else_clause {
        ElseClause::Block(block) => {
            check_function_calls_in_block(block, function_info, diagnostics);
        }
        ElseClause::ElseIf(else_if) => {
            match &else_if.condition {
                nexus_parser::IfCondition::Boolean(expr) => {
                    check_function_calls_in_expression(expr, function_info, diagnostics);
                }
                nexus_parser::IfCondition::Pattern { matcher, cases } => {
                    check_function_calls_in_expression(matcher, function_info, diagnostics);
                    for case in cases {
                        match &case.body {
                            nexus_parser::PatternBody::Expression(expr) => {
                                check_function_calls_in_expression(
                                    expr,
                                    function_info,
                                    diagnostics,
                                );
                            }
                            nexus_parser::PatternBody::Block(block) => {
                                check_function_calls_in_block(block, function_info, diagnostics);
                            }
                        }
                    }
                }
            }
            check_function_calls_in_block(&else_if.then_block, function_info, diagnostics);
            if let Some(inner_else) = &else_if.else_block {
                check_function_calls_in_else_clause(inner_else, function_info, diagnostics);
            }
        }
    }
}

/// Check function calls in an expression
fn check_function_calls_in_expression(
    expr: &Expression,
    function_info: &HashMap<String, FunctionInfo>,
    diagnostics: &mut Diagnostics,
) {
    match expr {
        Expression::Call(call) => {
            check_function_call(call, function_info, diagnostics);
            // Also check arguments recursively
            for arg in &call.args {
                check_function_calls_in_expression(arg, function_info, diagnostics);
            }
        }
        Expression::MethodCall(method_call) => {
            check_function_calls_in_expression(&method_call.receiver, function_info, diagnostics);
            for arg in &method_call.args {
                check_function_calls_in_expression(arg, function_info, diagnostics);
            }
        }
        Expression::MacroCall(macro_call) => {
            for arg in &macro_call.args {
                check_function_calls_in_expression(arg, function_info, diagnostics);
            }
        }
        Expression::FieldAccess(field_access) => {
            check_function_calls_in_expression(&field_access.object, function_info, diagnostics);
        }
        Expression::Index(index) => {
            check_function_calls_in_expression(&index.array, function_info, diagnostics);
            check_function_calls_in_expression(&index.index, function_info, diagnostics);
        }
        Expression::Array(array) => {
            for elem in &array.elements {
                check_function_calls_in_expression(elem, function_info, diagnostics);
            }
        }
        Expression::StructInit(struct_init) => {
            for field in &struct_init.fields {
                check_function_calls_in_expression(&field.value, function_info, diagnostics);
            }
        }
        Expression::Lambda(lambda) => match &lambda.body {
            nexus_parser::LambdaBody::Expression(expr) => {
                check_function_calls_in_expression(expr, function_info, diagnostics);
            }
            nexus_parser::LambdaBody::Block(block) => {
                check_function_calls_in_block(block, function_info, diagnostics);
            }
        },
        Expression::Grouped(inner, _span) => {
            check_function_calls_in_expression(inner, function_info, diagnostics);
        }
        Expression::Literal(_) | Expression::Variable(_) => {}
    }
}

/// Check a function call for correct argument count and types
fn check_function_call(
    call: &CallExpr,
    function_info: &HashMap<String, FunctionInfo>,
    diagnostics: &mut Diagnostics,
) {
    let func_name = &call.function;
    let arg_count = call.args.len();

    // First check user-defined functions
    if let Some(info) = function_info.get(func_name) {
        if arg_count != info.param_count {
            diagnostics.error(
                format!(
                    "Function '{}' expects {} argument{}, but {} {} provided",
                    func_name,
                    info.param_count,
                    if info.param_count == 1 { "" } else { "s" },
                    arg_count,
                    if arg_count == 1 { "was" } else { "were" }
                ),
                call.span,
            );
        } else {
            // Check argument types
            for (i, (arg, expected_type_str)) in
                call.args.iter().zip(info.param_types.iter()).enumerate()
            {
                let arg_type = infer_expression_type(arg, function_info);
                let expected_type = InferredType::from_type_name(expected_type_str);

                if !arg_type.is_compatible_with(&expected_type) {
                    diagnostics.error(
                        format!(
                            "Argument {} of '{}' has type '{}', expected '{}'",
                            i + 1,
                            func_name,
                            arg_type,
                            expected_type
                        ),
                        *arg.span(),
                    );
                }
            }
        }
        return;
    }

    // Then check builtins
    if let Some(builtin) = BUILTINS.get(func_name.as_str()) {
        // Check if this is a variadic function (has a parameter type starting with "...")
        let is_variadic = builtin.params.iter().any(|(_, ty)| ty.starts_with("..."));

        if is_variadic {
            // Variadic functions accept any number of arguments of any type
            // No argument count or type checking needed
        } else {
            let expected_count = builtin.params.len();
            if arg_count != expected_count {
                diagnostics.error(
                    format!(
                        "Builtin function '{}' expects {} argument{}, but {} {} provided",
                        func_name,
                        expected_count,
                        if expected_count == 1 { "" } else { "s" },
                        arg_count,
                        if arg_count == 1 { "was" } else { "were" }
                    ),
                    call.span,
                );
            } else {
                // Check argument types for builtins
                for (i, (arg, (_, expected_type_str))) in
                    call.args.iter().zip(builtin.params.iter()).enumerate()
                {
                    let arg_type = infer_expression_type(arg, function_info);
                    let expected_type = InferredType::from_type_name(expected_type_str);

                    if !arg_type.is_compatible_with(&expected_type) {
                        diagnostics.error(
                            format!(
                                "Argument {} of '{}' has type '{}', expected '{}'",
                                i + 1,
                                func_name,
                                arg_type,
                                expected_type
                            ),
                            *arg.span(),
                        );
                    }
                }
            }
        }
    }
    // If the function is not found, we don't report an error here
    // as it might be an imported function from another module
}

/// Check for expression statements that immediately follow a goto (likely a mistake)
fn check_goto_followed_by_expression(block: &Block, diagnostics: &mut Diagnostics) {
    let statements: Vec<_> = block.statements.iter().collect();

    for i in 0..statements.len() {
        if let Statement::Goto(goto) = statements[i]
            && i + 1 < statements.len()
            && let Statement::Expression(expr) = statements[i + 1]
        {
            // Check if it's on the same or immediately next line
            let goto_line = goto.span.line;
            let expr_line = expr.span().line;

            if expr_line == goto_line || expr_line == goto_line + 1 {
                // This is likely a mistake - extra content after goto
                if matches!(expr, Expression::Literal(_)) {
                    diagnostics.error(
                        format!(
                            "Unexpected expression after 'goto {}'. Goto statements should only have a single label.",
                            goto.label
                        ),
                        *expr.span(),
                    );
                }
            }
        }
    }

    // Recursively check nested blocks
    for stmt in &block.statements {
        match stmt {
            Statement::If(if_stmt) => {
                check_goto_followed_by_expression(&if_stmt.then_block, diagnostics);
                if let Some(else_clause) = &if_stmt.else_block {
                    check_goto_followed_by_expression_in_else(else_clause, diagnostics);
                }
            }
            Statement::Subscope(subscope) => {
                check_goto_followed_by_expression(&subscope.body, diagnostics);
            }
            Statement::Block(inner_block) => {
                check_goto_followed_by_expression(inner_block, diagnostics);
            }
            Statement::Defer(defer) => {
                check_goto_followed_by_expression(&defer.body, diagnostics);
            }
            _ => {}
        }
    }
}

/// Check goto followed by expression in else clause
fn check_goto_followed_by_expression_in_else(
    else_clause: &ElseClause,
    diagnostics: &mut Diagnostics,
) {
    match else_clause {
        ElseClause::Block(block) => {
            check_goto_followed_by_expression(block, diagnostics);
        }
        ElseClause::ElseIf(else_if) => {
            check_goto_followed_by_expression(&else_if.then_block, diagnostics);
            if let Some(inner_else) = &else_if.else_block {
                check_goto_followed_by_expression_in_else(inner_else, diagnostics);
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

    #[test]
    fn test_function_redefinition_error() {
        let content = r#"
            std foo(): void { return }
            std foo(): i64 { return 1 }
        "#;
        let ast = parse_content(content);
        let config = DiagnosticsConfig::default();
        let diagnostics = compute_diagnostics(content, &ast, &config);

        assert!(!diagnostics.is_empty());
        assert!(
            diagnostics
                .iter()
                .any(|d| d.message.contains("already defined"))
        );
    }

    #[test]
    fn test_struct_redefinition_error() {
        let content = r#"
            struct Point { i32 x = 0 }
            struct Point { i32 y = 0 }
        "#;
        let ast = parse_content(content);
        let config = DiagnosticsConfig::default();
        let diagnostics = compute_diagnostics(content, &ast, &config);

        assert!(!diagnostics.is_empty());
        assert!(
            diagnostics
                .iter()
                .any(|d| d.message.contains("Struct") && d.message.contains("already defined"))
        );
    }

    #[test]
    fn test_variable_redefinition_error() {
        let content = r#"
            std main(): void {
                m x = 1
                m x = 2
            }
        "#;
        let ast = parse_content(content);
        let config = DiagnosticsConfig::default();
        let diagnostics = compute_diagnostics(content, &ast, &config);

        assert!(!diagnostics.is_empty());
        assert!(
            diagnostics
                .iter()
                .any(|d| d.message.contains("Variable") && d.message.contains("already defined"))
        );
    }

    #[test]
    fn test_duplicate_subscope_label_error() {
        let content = r#"
            std main(): void {
                subscope loop {
                    goto loop
                }
                subscope loop {
                    goto loop
                }
            }
        "#;
        let ast = parse_content(content);
        let config = DiagnosticsConfig::default();
        let diagnostics = compute_diagnostics(content, &ast, &config);

        assert!(!diagnostics.is_empty());
        assert!(
            diagnostics
                .iter()
                .any(|d| d.message.contains("Subscope label")
                    && d.message.contains("already defined"))
        );
    }

    #[test]
    fn test_goto_undefined_label_error() {
        let content = r#"
            std main(): void {
                goto undefined_label
            }
        "#;
        let ast = parse_content(content);
        let config = DiagnosticsConfig::default();
        let diagnostics = compute_diagnostics(content, &ast, &config);

        assert!(!diagnostics.is_empty());
        assert!(
            diagnostics
                .iter()
                .any(|d| d.message.contains("undefined label"))
        );
    }

    #[test]
    fn test_goto_ambiguous_label_error() {
        let content = r#"
            std main(): void {
                subscope loop {
                    m x = 1
                }
                subscope loop {
                    goto loop
                }
            }
        "#;
        let ast = parse_content(content);
        let config = DiagnosticsConfig::default();
        let diagnostics = compute_diagnostics(content, &ast, &config);

        // Should have errors for duplicate label and ambiguous goto
        assert!(!diagnostics.is_empty());
    }

    #[test]
    fn test_function_call_wrong_arg_count_error() {
        let content = r#"
            std add(i64 a, i64 b): i64 {
                return addi64(a, b)
            }
            std main(): void {
                m x = add(1)
            }
        "#;
        let ast = parse_content(content);
        let config = DiagnosticsConfig::default();
        let diagnostics = compute_diagnostics(content, &ast, &config);

        assert!(!diagnostics.is_empty());
        assert!(diagnostics.iter().any(|d| d.message.contains("expects 2")));
    }

    #[test]
    fn test_builtin_wrong_arg_count_error() {
        let content = r#"
            std main(): void {
                m x = addi64(1)
            }
        "#;
        let ast = parse_content(content);
        let config = DiagnosticsConfig::default();
        let diagnostics = compute_diagnostics(content, &ast, &config);

        assert!(!diagnostics.is_empty());
        assert!(diagnostics.iter().any(|d| d.message.contains("expects 2")));
    }

    #[test]
    fn test_correct_function_call_ok() {
        let content = r#"
            std add(i64 a, i64 b): i64 {
                return addi64(a, b)
            }
            std main(): void {
                m x = add(1, 2)
            }
        "#;
        let ast = parse_content(content);
        let config = DiagnosticsConfig::default();
        let diagnostics = compute_diagnostics(content, &ast, &config);

        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_valid_goto_with_unique_label() {
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

        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_builtin_wrong_arg_type_error() {
        let content = r#"
            std main(): void {
                m x = addi64(1, "a")
            }
        "#;
        let ast = parse_content(content);
        let config = DiagnosticsConfig::default();
        let diagnostics = compute_diagnostics(content, &ast, &config);

        assert!(!diagnostics.is_empty());
        assert!(
            diagnostics
                .iter()
                .any(|d| d.message.contains("string") && d.message.contains("i64"))
        );
    }

    #[test]
    fn test_user_function_wrong_arg_type_error() {
        let content = r#"
            std add(i64 a, i64 b): i64 {
                return addi64(a, b)
            }
            std main(): void {
                m x = add(1, "hello")
            }
        "#;
        let ast = parse_content(content);
        let config = DiagnosticsConfig::default();
        let diagnostics = compute_diagnostics(content, &ast, &config);

        assert!(!diagnostics.is_empty());
        assert!(
            diagnostics
                .iter()
                .any(|d| d.message.contains("string") && d.message.contains("i64"))
        );
    }

    #[test]
    fn test_goto_with_extra_content_error() {
        let content = r#"
            std main(): void {
                subscope loop {
                    goto loop "extra"
                }
            }
        "#;
        let ast = parse_content(content);
        let config = DiagnosticsConfig::default();
        let diagnostics = compute_diagnostics(content, &ast, &config);

        assert!(!diagnostics.is_empty());
        assert!(
            diagnostics
                .iter()
                .any(|d| d.message.contains("Unexpected expression after"))
        );
    }

    #[test]
    fn test_correct_types_ok() {
        let content = r#"
            std main(): void {
                m x = addi64(1, 2)
                m y = and(true, false)
            }
        "#;
        let ast = parse_content(content);
        let config = DiagnosticsConfig::default();
        let diagnostics = compute_diagnostics(content, &ast, &config);

        assert!(diagnostics.is_empty());
    }
}

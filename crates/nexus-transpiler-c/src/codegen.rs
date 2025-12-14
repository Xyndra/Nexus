//! C code generator for Nexus programs.
//!
//! This module generates standard C code from type-checked Nexus AST.

use nexus_core::{FunctionColor, NexusError, NexusResult};
use nexus_interpreter::{BuiltinRegistry, Interpreter};
use nexus_lsp_server::macro_expansion::MacroExpansionContext;
use nexus_parser::*;
use nexus_types::{ArraySize, NexusType, PrimitiveType, TypeRegistry};
use std::collections::{HashMap, HashSet};
use std::path::Path;

use crate::TranspilerConfig;

/// C code generator
pub struct CCodeGenerator<'a> {
    config: &'a TranspilerConfig,
    type_registry: &'a TypeRegistry,
    output: String,
    indent_level: usize,
    includes: HashSet<String>,
    forward_decls: Vec<String>,
    global_vars: Vec<String>,
    function_impls: Vec<String>,
    current_function_color: Option<FunctionColor>,
    temp_var_counter: usize,
    /// Mapping from function name to module-qualified name
    function_to_module: HashMap<String, String>,
    /// Mapping from (module_name, function_name) to qualified name for disambiguation
    module_function_map: HashMap<(String, String), String>,
    /// Mapping from function name to return type
    function_return_types: HashMap<String, String>,
    /// Current module being generated
    current_module: String,
    builtins: BuiltinRegistry,
    interpreter: Option<Interpreter>,
    subscope_depth: usize,
    macro_context: Option<MacroExpansionContext>,
    variable_types: std::collections::HashMap<String, String>,
    statement_prelude: Vec<String>,
    /// Struct definitions for default value generation
    struct_defs: HashMap<String, StructDefAst>,
}

impl<'a> CCodeGenerator<'a> {
    pub fn new(config: &'a TranspilerConfig, type_registry: &'a TypeRegistry) -> Self {
        Self {
            config,
            type_registry,
            output: String::new(),
            indent_level: 0,
            includes: HashSet::new(),
            forward_decls: Vec::new(),
            global_vars: Vec::new(),
            function_impls: Vec::new(),
            current_function_color: None,
            temp_var_counter: 0,
            function_to_module: HashMap::new(),
            module_function_map: HashMap::new(),
            function_return_types: HashMap::new(),
            current_module: String::new(),
            builtins: BuiltinRegistry::new(),
            interpreter: None,
            subscope_depth: 0,
            macro_context: None,
            variable_types: std::collections::HashMap::new(),
            statement_prelude: Vec::new(),
            struct_defs: HashMap::new(),
        }
    }

    fn in_subscope(&self) -> bool {
        self.subscope_depth > 0
    }

    fn enter_subscope(&mut self) {
        self.subscope_depth += 1;
    }

    fn exit_subscope(&mut self) {
        self.subscope_depth = self.subscope_depth.saturating_sub(1);
    }

    /// Generate C code for a program
    pub fn generate(
        &mut self,
        program: &Program,
        module_path: &Path,
        module_name: &str,
        all_programs: &[&Program],
        all_module_info: &[(String, &Program)],
        is_root_module: bool,
    ) -> NexusResult<String> {
        // Store module name for later use in C main generation
        let module_name_for_main = module_name.to_string();
        // Track current module for function call resolution
        self.current_module = module_name.to_string();
        // Add standard includes
        self.includes.insert("stdint.h".to_string());
        self.includes.insert("stdbool.h".to_string());
        self.includes.insert("stddef.h".to_string());
        self.includes.insert("nexus_core.h".to_string());

        // Initialize interpreter for macro expansion
        self.interpreter = Some(Interpreter::new());
        if let Some(ref mut interp) = self.interpreter {
            let _ = interp.load_program(program);
        }

        // Initialize macro context with all programs (for cross-module macro support)
        self.macro_context = Some(MacroExpansionContext::from_programs(
            all_programs.iter().copied(),
        ));

        // Generate forward declarations for ALL functions from ALL modules
        // and populate function_to_module mapping
        for (other_module_name, other_program) in all_module_info {
            for item in &other_program.items {
                if let Item::Function(func) = item {
                    self.generate_function_forward_decl(func, other_module_name)?;
                    // Store mapping from function name to module-qualified name
                    // Always qualify, even main functions
                    let qualified_name =
                        format!("{}_{}", other_module_name.replace(".", "_"), func.name);
                    self.function_to_module
                        .insert(func.name.clone(), qualified_name.clone());

                    // Store per-module mapping for disambiguation
                    self.module_function_map.insert(
                        (other_module_name.clone(), func.name.clone()),
                        qualified_name.clone(),
                    );

                    // Store return type for type inference
                    let return_type =
                        self.nexus_type_to_c(&self.resolve_type_expr(&func.return_type))?;
                    self.function_return_types
                        .insert(qualified_name, return_type);
                }
            }
        }

        // Generate implementations for THIS module
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.generate_function_impl(func, module_name)?;
                }
                Item::Method(method) => self.generate_method(method)?,
                Item::Struct(struct_def) => self.generate_struct(struct_def)?,
                Item::Interface(_) => {
                    // Interfaces are not directly represented in C
                }
                Item::Macro(_) => {
                    // Macros are expanded during transpilation
                }
                Item::Use(_) => {
                    // Use statements are resolved during type checking
                }
            }
        }

        // Add C main entry point if this is the root module
        if is_root_module {
            self.generate_c_main(&module_name_for_main);
        }

        // Build final output
        self.build_output(module_path);

        Ok(self.output.clone())
    }

    /// Build the final output with includes, declarations, and implementations
    fn build_output(&mut self, module_path: &Path) {
        let mut output = String::new();

        // File header comment
        output.push_str(&format!(
            "// Generated C code from Nexus source: {}\n",
            module_path.display()
        ));
        output.push_str("// DO NOT EDIT - This file is automatically generated\n\n");

        // Includes
        for include in &self.includes {
            output.push_str(&format!("#include <{}>\n", include));
        }
        output.push('\n');

        // Forward declarations
        if !self.forward_decls.is_empty() {
            output.push_str("// Forward declarations\n");
            for decl in &self.forward_decls {
                output.push_str(decl);
                output.push('\n');
            }
            output.push('\n');
        }

        // Global variables
        if !self.global_vars.is_empty() {
            output.push_str("// Global variables\n");
            for var in &self.global_vars {
                output.push_str(var);
                output.push('\n');
            }
            output.push('\n');
        }

        // Function implementations
        output.push_str("// Function implementations\n");
        for func in &self.function_impls {
            output.push_str(func);
            output.push('\n');
        }

        self.output = output;
    }

    /// Generate a struct definition
    fn generate_struct(&mut self, struct_def: &StructDefAst) -> NexusResult<()> {
        // Store the struct definition for later use in default value generation
        self.struct_defs
            .insert(struct_def.name.clone(), struct_def.clone());

        let prefixed_name = self.prefix_struct_name(&struct_def.name);
        let mut struct_code = format!("typedef struct {} {{\n", prefixed_name);

        for field in &struct_def.fields {
            let c_type = self.nexus_type_to_c(&self.resolve_type_expr(&field.ty))?;
            struct_code.push_str(&format!("    {} {};\n", c_type, field.name));
        }

        struct_code.push_str(&format!("}} {};\n", prefixed_name));

        self.forward_decls.push(struct_code);
        Ok(())
    }

    /// Generate a prefixed struct name for C
    fn prefix_struct_name(&self, name: &str) -> String {
        format!("nx_{}_{}", self.current_module.replace(".", "_"), name)
    }

    /// Generate a function forward declaration
    fn generate_function_forward_decl(
        &mut self,
        func: &FunctionDef,
        module_name: &str,
    ) -> NexusResult<()> {
        let return_type = self.nexus_type_to_c(&self.resolve_type_expr(&func.return_type))?;

        // Always use module-qualified names for all functions
        let func_name = format!("{}_{}", module_name.replace(".", "_"), func.name);
        let mut sig = format!("{} nx_{}", return_type, func_name);
        sig.push('(');

        if func.params.is_empty() {
            sig.push_str("void");
        } else {
            let params: Vec<String> = func
                .params
                .iter()
                .map(|p| {
                    let c_type = self.nexus_type_to_c(&self.resolve_type_expr(&p.ty))?;
                    Ok(format!("{} {}", c_type, self.sanitize_identifier(&p.name)))
                })
                .collect::<NexusResult<Vec<_>>>()?;
            sig.push_str(&params.join(", "));
        }

        sig.push(')');
        self.forward_decls.push(format!("{};\n", sig));
        Ok(())
    }

    /// Generate a function implementation
    fn generate_function_impl(&mut self, func: &FunctionDef, module_name: &str) -> NexusResult<()> {
        self.current_function_color = Some(func.color);
        self.temp_var_counter = 0;

        // Clear and populate variable types for this function's parameters
        self.variable_types.clear();
        for param in &func.params {
            let c_type = self.nexus_type_to_c(&self.resolve_type_expr(&param.ty))?;
            self.variable_types.insert(param.name.clone(), c_type);
        }

        // Always use module-qualified names for all functions
        let return_type = self.nexus_type_to_c(&self.resolve_type_expr(&func.return_type))?;
        let func_name = format!("{}_{}", module_name.replace(".", "_"), func.name);
        let mut sig = format!("{} nx_{}", return_type, func_name);
        sig.push('(');

        if func.params.is_empty() {
            sig.push_str("void");
        } else {
            let params: Vec<String> = func
                .params
                .iter()
                .map(|p| {
                    let c_type = self.nexus_type_to_c(&self.resolve_type_expr(&p.ty))?;
                    Ok(format!("{} {}", c_type, self.sanitize_identifier(&p.name)))
                })
                .collect::<NexusResult<Vec<_>>>()?;
            sig.push_str(&params.join(", "));
        }

        sig.push(')');

        // Generate function body
        let mut func_impl = format!("{} {{\n", sig);

        // Generate local variable declarations (we'll collect these as we process the body)
        let body_code = self.generate_block_with_implicit_return(&func.body, &return_type)?;
        func_impl.push_str(&body_code);

        func_impl.push_str("}\n");

        self.function_impls.push(func_impl);
        self.current_function_color = None;

        Ok(())
    }

    /// Generate C main entry point that calls the root module's main
    fn generate_c_main(&mut self, root_module_name: &str) {
        let qualified_main = format!("nx_{}_main", root_module_name.replace(".", "_"));
        let main_impl = format!(
            r#"// C entry point
int main(int argc, char** argv) {{
    nx_init_args(argc, argv);
    {}();
    return 0;
}}
"#,
            qualified_main
        );
        self.function_impls.push(main_impl);
    }

    /// Generate a method
    fn generate_method(&mut self, method: &MethodDef) -> NexusResult<()> {
        self.current_function_color = Some(method.color);
        self.temp_var_counter = 0;

        let return_type = self.nexus_type_to_c(&self.resolve_type_expr(&method.return_type))?;
        let receiver_type =
            self.nexus_type_to_c(&NexusType::Named(method.receiver_type.clone()))?;

        // Generate method signature (methods become regular functions with receiver as first param)
        let method_c_name = format!("{}_{}", method.receiver_type, method.name);
        let mut sig = format!(
            "{} {}",
            return_type,
            self.mangle_function_name(&method_c_name)
        );
        sig.push('(');

        // Add receiver parameter
        let receiver_param = if method.receiver_mutable {
            format!(
                "{}* {}",
                receiver_type,
                self.sanitize_identifier(&method.receiver_name)
            )
        } else {
            format!(
                "const {}* {}",
                receiver_type,
                self.sanitize_identifier(&method.receiver_name)
            )
        };

        let mut all_params = vec![receiver_param];

        for p in &method.params {
            let c_type = self.nexus_type_to_c(&self.resolve_type_expr(&p.ty))?;
            all_params.push(format!("{} {}", c_type, self.sanitize_identifier(&p.name)));
        }

        sig.push_str(&all_params.join(", "));
        sig.push(')');

        // Add forward declaration
        self.forward_decls.push(format!("{};\n", sig));

        // Generate method body
        let mut method_impl = format!("{} {{\n", sig);
        let body_code = self.generate_block(&method.body)?;
        method_impl.push_str(&body_code);
        method_impl.push_str("}\n");

        self.function_impls.push(method_impl);
        self.current_function_color = None;

        Ok(())
    }

    /// Generate a block of statements
    fn generate_block(&mut self, block: &Block) -> NexusResult<String> {
        let mut code = String::new();
        self.indent_level += 1;

        for stmt in &block.statements {
            code.push_str(&self.generate_statement(stmt)?);
        }

        self.indent_level -= 1;
        Ok(code)
    }

    /// Generate a block of statements with implicit return handling for function bodies
    fn generate_block_with_implicit_return(
        &mut self,
        block: &Block,
        return_type: &str,
    ) -> NexusResult<String> {
        let mut code = String::new();
        self.indent_level += 1;

        // Check if we need to handle implicit returns (non-void return type)
        let needs_implicit_return = return_type != "void";

        let num_stmts = block.statements.len();
        for (i, stmt) in block.statements.iter().enumerate() {
            let is_last = i == num_stmts - 1;

            // If this is the last statement and we need implicit returns
            if is_last && needs_implicit_return {
                // Check if it's an expression statement (not already a return)
                if let Statement::Expression(expr) = stmt {
                    // Convert the expression statement into a return statement
                    let mut expr_code = self.generate_expression(expr)?;
                    // Strip redundant outer parentheses from return value
                    // e.g., "((a + b))" -> "(a + b)"
                    if expr_code.starts_with('(') && expr_code.ends_with(')') {
                        expr_code = expr_code[1..expr_code.len() - 1].to_string();
                    }
                    // Drain prelude into a local variable to avoid borrow conflicts
                    let prelude: Vec<String> = self.statement_prelude.drain(..).collect();
                    // Emit any prelude statements first
                    for prelude_stmt in prelude {
                        code.push_str(&self.indent(&format!("{};\n", prelude_stmt)));
                    }
                    code.push_str(&self.indent(&format!("return {};\n", expr_code)));
                    continue;
                }
            }

            // For all other statements, generate normally
            code.push_str(&self.generate_statement(stmt)?);
        }

        self.indent_level -= 1;
        Ok(code)
    }

    /// Generate a statement
    fn generate_statement(&mut self, stmt: &Statement) -> NexusResult<String> {
        match stmt {
            Statement::VarDecl(decl) => self.generate_var_decl(decl),
            Statement::Assignment(assign) => self.generate_assignment(assign),
            Statement::Expression(expr) => {
                let expr_code = self.generate_expression(expr)?;
                // Drain prelude into a local variable to avoid borrow conflicts
                let prelude: Vec<String> = self.statement_prelude.drain(..).collect();
                let mut result = String::new();
                // Emit any prelude statements first
                for prelude_stmt in prelude {
                    result.push_str(&self.indent(&format!("{};\n", prelude_stmt)));
                }
                result.push_str(&self.indent(&format!("{};\n", expr_code)));
                Ok(result)
            }
            Statement::Return(ret) => self.generate_return(ret),
            Statement::If(if_stmt) => self.generate_if(if_stmt),
            Statement::Defer(_) => {
                // Defer in C requires collecting deferred statements and executing at function exit
                // For simplicity, we'll generate a comment for now
                Ok(self.indent("// defer: not fully implemented\n"))
            }
            Statement::Subscope(subscope) => self.generate_subscope(subscope),
            Statement::Goto(goto) => {
                let label = self.sanitize_label(&goto.label);
                Ok(self.indent(&format!("goto {};\n", label)))
            }
            Statement::Block(block) => {
                let mut code = self.indent("{\n");
                code.push_str(&self.generate_block(block)?);
                code.push_str(&self.indent("}\n"));
                Ok(code)
            }
        }
    }

    /// Generate variable declaration
    fn generate_var_decl(&mut self, decl: &VarDecl) -> NexusResult<String> {
        // Track variable type for later use
        let (var_type, array_suffix) = if let Some(ty_expr) = &decl.ty {
            (
                self.nexus_type_to_c(&self.resolve_type_expr(ty_expr))?,
                String::new(),
            )
        } else {
            // Type inference - determine type from initializer
            // Special handling for arrays
            if let Expression::Array(_arr) = &decl.init {
                // Arrays are always nx_array type
                ("nx_array".to_string(), String::new())
            } else {
                (self.infer_c_type_from_expr(&decl.init)?, String::new())
            }
        };

        // Store the variable type for later lookup
        self.variable_types
            .insert(decl.name.clone(), var_type.clone());

        let mut code = String::new();

        // Handle global modifier
        if decl.modifiers.global {
            let init_expr = format!(" = {}", self.generate_expression(&decl.init)?);

            let global_var = format!(
                "{} {}{}{};\n",
                var_type,
                self.sanitize_identifier(&decl.name),
                array_suffix,
                init_expr
            );
            self.global_vars.push(global_var);
            return Ok(String::new());
        }

        // Generate the initializer expression (may add to prelude)
        let init_expr = self.generate_expression(&decl.init)?;

        // Drain prelude into a local variable to avoid borrow conflicts
        let prelude: Vec<String> = self.statement_prelude.drain(..).collect();
        // Emit any prelude statements first
        for prelude_stmt in prelude {
            code.push_str(&self.indent(&format!("{};\n", prelude_stmt)));
        }

        // Handle heap modifier
        if decl.modifiers.heap {
            code.push_str(&self.indent(&format!(
                "{}* {}{} = ({}*)nx_malloc(sizeof({}));\n",
                var_type,
                self.sanitize_identifier(&decl.name),
                array_suffix,
                var_type,
                var_type
            )));

            code.push_str(&self.indent(&format!(
                "*{} = {};\n",
                self.sanitize_identifier(&decl.name),
                init_expr
            )));
        } else {
            // Stack allocation
            code.push_str(&self.indent(&format!(
                "{} {}{} = {};\n",
                var_type,
                self.sanitize_identifier(&decl.name),
                array_suffix,
                init_expr
            )));
        }

        Ok(code)
    }

    /// Generate assignment
    fn generate_assignment(&mut self, assign: &Assignment) -> NexusResult<String> {
        let target = self.generate_expression(&assign.target)?;
        let value = self.generate_expression(&assign.value)?;

        let mut code = String::new();
        // Drain prelude into a local variable to avoid borrow conflicts
        let prelude: Vec<String> = self.statement_prelude.drain(..).collect();
        // Emit any prelude statements first
        for prelude_stmt in prelude {
            code.push_str(&self.indent(&format!("{};\n", prelude_stmt)));
        }
        code.push_str(&self.indent(&format!("{} = {};\n", target, value)));
        Ok(code)
    }

    /// Generate return statement
    fn generate_return(&mut self, ret: &ReturnStmt) -> NexusResult<String> {
        // If we're in a subscope, return acts as break
        if self.in_subscope() {
            // In subscope, bare return means break out
            if ret.value.is_none() {
                return Ok(self.indent("break;\n"));
            }
        }

        let mut code = String::new();

        if let Some(ref value) = ret.value {
            let mut expr = self.generate_expression(value)?;
            // Strip redundant outer parentheses from return value
            // e.g., "((a + b))" -> "(a + b)"
            if expr.starts_with('(') && expr.ends_with(')') {
                expr = expr[1..expr.len() - 1].to_string();
            }
            // Drain prelude into a local variable to avoid borrow conflicts
            let prelude: Vec<String> = self.statement_prelude.drain(..).collect();
            // Emit any prelude statements first
            for prelude_stmt in prelude {
                code.push_str(&self.indent(&format!("{};\n", prelude_stmt)));
            }
            code.push_str(&self.indent(&format!("return {};\n", expr)));
            Ok(code)
        } else {
            Ok(self.indent("return;\n"))
        }
    }

    /// Generate if statement
    fn generate_if(&mut self, if_stmt: &IfStmt) -> NexusResult<String> {
        let mut code = String::new();

        let condition = match &if_stmt.condition {
            IfCondition::Boolean(expr) => {
                let cond = self.generate_expression(expr)?;
                // Strip redundant outer parentheses from condition
                // e.g., "((a < b))" -> "(a < b)"
                if cond.starts_with('(') && cond.ends_with(')') {
                    cond[1..cond.len() - 1].to_string()
                } else {
                    cond
                }
            }
            IfCondition::Pattern { matcher, cases } => {
                // Pattern matching - generate switch-like code
                return self.generate_pattern_match(matcher, cases, &if_stmt.then_block);
            }
        };

        code.push_str(&self.indent(&format!("if ({}) {{\n", condition)));
        code.push_str(&self.generate_block(&if_stmt.then_block)?);
        code.push_str(&self.indent("}\n"));

        if let Some(ref else_clause) = if_stmt.else_block {
            match else_clause {
                ElseClause::Block(block) => {
                    code.push_str(&self.indent("else {\n"));
                    code.push_str(&self.generate_block(block)?);
                    code.push_str(&self.indent("}\n"));
                }
                ElseClause::ElseIf(else_if) => {
                    code.push_str(&self.indent("else "));
                    code.push_str(self.generate_if(else_if)?.trim_start());
                }
            }
        }

        Ok(code)
    }

    /// Generate pattern matching
    fn generate_pattern_match(
        &mut self,
        _matcher: &Expression,
        _cases: &[PatternCase],
        _then_block: &Block,
    ) -> NexusResult<String> {
        // Pattern matching is complex, for now return a placeholder
        Ok(self.indent("// Pattern matching not fully implemented\n"))
    }

    /// Generate subscope statement
    fn generate_subscope(&mut self, subscope: &SubscopeStmt) -> NexusResult<String> {
        let mut code = String::new();

        // Subscopes in Nexus are loops that can be exited with return (becomes break)
        // and jumped to with goto (becomes continue to loop start)
        // We use a do-while(0) loop that can be repeated with a flag

        // Sanitize label name (replace reserved C keywords)
        let label = self.sanitize_label(&subscope.name);

        code.push_str(&self.indent(&format!("{}:\n", label)));
        code.push_str(&self.indent("do {\n"));

        // Track that we're in a subscope
        let was_in_subscope = self.in_subscope();
        self.enter_subscope();

        code.push_str(&self.generate_block(&subscope.body)?);

        if was_in_subscope {
            self.enter_subscope();
        } else {
            self.exit_subscope();
        }

        code.push_str(&self.indent("} while (0);\n"));

        Ok(code)
    }

    /// Generate an expression
    fn generate_expression(&mut self, expr: &Expression) -> NexusResult<String> {
        match expr {
            Expression::Literal(lit) => self.generate_literal(lit),
            Expression::Variable(var) => Ok(self.sanitize_identifier(&var.name)),
            Expression::Call(call) => self.generate_call(call),
            Expression::MethodCall(method_call) => self.generate_method_call(method_call),
            Expression::MacroCall(macro_call) => self.generate_macro_call(macro_call),
            Expression::FieldAccess(field_access) => self.generate_field_access(field_access),
            Expression::Index(index) => self.generate_index(index),
            Expression::Array(array) => self.generate_array(array),
            Expression::StructInit(struct_init) => self.generate_struct_init(struct_init),
            Expression::Lambda(_lambda) => Ok("/* lambda not implemented */".to_string()),
            Expression::Grouped(inner, _span) => self.generate_expression(inner),
        }
    }

    /// Generate a literal
    fn generate_literal(&self, lit: &Literal) -> NexusResult<String> {
        match &lit.kind {
            LiteralKind::Int(n) => Ok(n.to_string()),
            LiteralKind::Float(f) => Ok(f.to_string()),
            LiteralKind::String(s) => {
                // Convert to C string literal and wrap in nx_string_from_cstr
                let escaped = s
                    .replace('\\', "\\\\")
                    .replace('"', "\\\"")
                    .replace('\n', "\\n")
                    .replace('\r', "\\r")
                    .replace('\t', "\\t");
                Ok(format!("nx_string_from_cstr(\"{}\")", escaped))
            }
            LiteralKind::Char(c) => {
                // Runes are uint32_t (UTF-32)
                let code_point = *c as u32;
                Ok(format!("((uint32_t){})", code_point))
            }
            LiteralKind::Bool(b) => Ok(if *b { "true" } else { "false" }.to_string()),
            LiteralKind::None => Ok("NULL".to_string()),
        }
    }

    /// Generate a function call
    fn generate_call(&mut self, call: &CallExpr) -> NexusResult<String> {
        // Check if this is a builtin function
        if self.builtins.get(&call.function).is_some() {
            // Map builtin to C operation
            return self.generate_builtin_call(&call.function, &call.args);
        }

        // Check if this is a compat.io builtin
        if self.builtins.get_compat_io(&call.function).is_some() {
            return self.generate_builtin_call(&call.function, &call.args);
        }

        // Regular function call - use module-qualified name if available
        let args: Vec<String> = call
            .args
            .iter()
            .map(|arg| self.generate_expression(arg))
            .collect::<NexusResult<Vec<_>>>()?;

        // First check if this function exists in the current module
        let func_name = if let Some(qualified) = self
            .module_function_map
            .get(&(self.current_module.clone(), call.function.clone()))
        {
            // Use the local module's version
            format!("nx_{}", qualified)
        } else if let Some(qualified) = self.function_to_module.get(&call.function) {
            // Fall back to any module's version
            format!("nx_{}", qualified)
        } else {
            self.mangle_function_name(&call.function)
        };

        Ok(format!("{}({})", func_name, args.join(", ")))
    }

    /// Generate a builtin function call
    fn generate_builtin_call(&mut self, name: &str, args: &[Expression]) -> NexusResult<String> {
        // Special handling for print/println - generate code for each argument with type info
        if name == "print" || name == "println" {
            return self.generate_print_call(name, args);
        }

        // Check if this is a compat/plat function
        let c_name = self.map_builtin_to_c(name);
        if c_name.starts_with("nx_") {
            // This is a compat/plat function, call it directly
            let arg_strs: Vec<String> = args
                .iter()
                .map(|arg| self.generate_expression(arg))
                .collect::<NexusResult<Vec<_>>>()?;
            return Ok(format!("{}({})", c_name, arg_strs.join(", ")));
        }

        // Map Nexus builtins to C operations
        match name {
            // i64 operations
            "addi64" => self.binary_op(args, "+"),
            "subi64" => self.binary_op(args, "-"),
            "muli64" => self.binary_op(args, "*"),
            "divi64" => self.binary_op(args, "/"),
            "modi64" => self.binary_op(args, "%"),
            "eqi64" => self.binary_op(args, "=="),
            "nei64" => self.binary_op(args, "!="),
            "lti64" => self.binary_op(args, "<"),
            "lei64" => self.binary_op(args, "<="),
            "gti64" => self.binary_op(args, ">"),
            "gei64" => self.binary_op(args, ">="),

            // i32 operations
            "addi32" => self.binary_op(args, "+"),
            "subi32" => self.binary_op(args, "-"),
            "muli32" => self.binary_op(args, "*"),
            "divi32" => self.binary_op(args, "/"),
            "modi32" => self.binary_op(args, "%"),
            "eqi32" => self.binary_op(args, "=="),
            "nei32" => self.binary_op(args, "!="),
            "lti32" => self.binary_op(args, "<"),
            "lei32" => self.binary_op(args, "<="),
            "gti32" => self.binary_op(args, ">"),
            "gei32" => self.binary_op(args, ">="),

            // f64 operations
            "addf64" => self.binary_op(args, "+"),
            "subf64" => self.binary_op(args, "-"),
            "mulf64" => self.binary_op(args, "*"),
            "divf64" => self.binary_op(args, "/"),
            "eqf64" => self.binary_op(args, "=="),
            "nef64" => self.binary_op(args, "!="),
            "ltf64" => self.binary_op(args, "<"),
            "lef64" => self.binary_op(args, "<="),
            "gtf64" => self.binary_op(args, ">"),
            "gef64" => self.binary_op(args, ">="),

            // f32 operations
            "addf32" => self.binary_op(args, "+"),
            "subf32" => self.binary_op(args, "-"),
            "mulf32" => self.binary_op(args, "*"),
            "divf32" => self.binary_op(args, "/"),
            "eqf32" => self.binary_op(args, "=="),
            "nef32" => self.binary_op(args, "!="),
            "ltf32" => self.binary_op(args, "<"),
            "lef32" => self.binary_op(args, "<="),
            "gtf32" => self.binary_op(args, ">"),
            "gef32" => self.binary_op(args, ">="),

            // Negation operations
            "negi32" => self.unary_op(args, "-"),
            "negi64" => self.unary_op(args, "-"),
            "negf32" => self.unary_op(args, "-"),
            "negf64" => self.unary_op(args, "-"),

            // Logical operations
            "and" => self.binary_op(args, "&&"),
            "or" => self.binary_op(args, "||"),
            "not" => self.unary_op(args, "!"),

            // Bitwise operations
            "bitand" => self.binary_op(args, "&"),
            "bitor" => self.binary_op(args, "|"),
            "bitxor" => self.binary_op(args, "^"),
            "bitnot" => self.unary_op(args, "~"),
            "shl" => self.binary_op(args, "<<"),
            "shr" => self.binary_op(args, ">>"),

            // Collections
            "len" => {
                if args.len() != 1 {
                    return Err(NexusError::TranspileError {
                        message: "len expects 1 argument".to_string(),
                    });
                }
                let arg = self.generate_expression(&args[0])?;
                Ok(format!("nx_array_len(&({}))", arg))
            }

            // String operations
            "concat" => {
                if args.len() != 2 {
                    return Err(NexusError::TranspileError {
                        message: "concat expects 2 arguments".to_string(),
                    });
                }
                let left = self.generate_expression(&args[0])?;
                let right = self.generate_expression(&args[1])?;
                let _left_type = self.infer_c_type_from_expr(&args[0])?;
                let _right_type = self.infer_c_type_from_expr(&args[1])?;

                // Create temp variables for both strings to ensure they have stable addresses
                let left_temp = format!("_concat_l_{}", self.temp_var_counter);
                self.temp_var_counter += 1;
                let right_temp = format!("_concat_r_{}", self.temp_var_counter);
                self.temp_var_counter += 1;

                // Add temp variable declarations to prelude (no semicolon, added when emitting)
                self.statement_prelude
                    .push(format!("nx_string {} = {}", left_temp, left));
                self.statement_prelude
                    .push(format!("nx_string {} = {}", right_temp, right));

                Ok(format!("nx_string_concat(&{}, &{})", left_temp, right_temp))
            }
            "str" => {
                if args.len() != 1 {
                    return Err(NexusError::TranspileError {
                        message: "str expects 1 argument".to_string(),
                    });
                }

                let arg_type = self.infer_c_type_from_expr(&args[0])?;
                let arg = self.generate_expression(&args[0])?;

                // Convert value to string based on type using nx_value approach
                match arg_type.as_str() {
                    "nx_string" => Ok(arg), // Already a string
                    "int64_t" => Ok(format!(
                        "nx_string_from_value((nx_value){{NX_TYPE_I64, {{.as_i64 = {}}}}})",
                        arg
                    )),
                    "int32_t" => Ok(format!(
                        "nx_string_from_value((nx_value){{NX_TYPE_I32, {{.as_i32 = {}}}}})",
                        arg
                    )),
                    "uint64_t" => Ok(format!(
                        "nx_string_from_value((nx_value){{NX_TYPE_U64, {{.as_u64 = {}}}}})",
                        arg
                    )),
                    "uint32_t" => Ok(format!(
                        "nx_string_from_value((nx_value){{NX_TYPE_U32, {{.as_u32 = {}}}}})",
                        arg
                    )),
                    "double" => Ok(format!(
                        "nx_string_from_value((nx_value){{NX_TYPE_F64, {{.as_f64 = {}}}}})",
                        arg
                    )),
                    "float" => Ok(format!(
                        "nx_string_from_value((nx_value){{NX_TYPE_F32, {{.as_f32 = {}}}}})",
                        arg
                    )),
                    "bool" => Ok(format!(
                        "nx_string_from_value((nx_value){{NX_TYPE_BOOL, {{.as_bool = {}}}}})",
                        arg
                    )),
                    "nx_array" => Ok(format!(
                        "nx_string_from_value((nx_value){{NX_TYPE_ARRAY, {{.as_array = &{}}}}})",
                        arg
                    )),
                    _ => Ok("nx_string_from_cstr(\"[unknown]\")".into()),
                }
            }

            // Default: generate as function call
            _ => {
                let arg_strs: Vec<String> = args
                    .iter()
                    .map(|arg| self.generate_expression(arg))
                    .collect::<NexusResult<Vec<_>>>()?;
                Ok(format!("{}({})", name, arg_strs.join(", ")))
            }
        }
    }

    /// Generate print/println call with compile-time type information
    /// Creates an array of nx_value with type tags determined at compile time
    fn generate_print_call(&mut self, name: &str, args: &[Expression]) -> NexusResult<String> {
        if args.is_empty() {
            // Empty print/println
            return Ok(if name == "println" {
                "nx_compat_println(NULL, 0)".to_string()
            } else {
                "nx_compat_print(NULL, 0)".to_string()
            });
        }

        // Generate array of nx_value with type information
        let mut temp_string_decls = Vec::new();
        let mut value_inits = Vec::new();

        for arg in args {
            let arg_code = self.generate_expression(arg)?;
            let inferred_type = self.infer_c_type_from_expr(arg)?;

            // Determine type tag for nx_value
            let type_tag = match inferred_type.as_str() {
                "int64_t" => "NX_TYPE_I64",
                "int32_t" => "NX_TYPE_I32",
                "uint64_t" => "NX_TYPE_U64",
                "uint32_t" => "NX_TYPE_U32",
                "float" => "NX_TYPE_F32",
                "double" => "NX_TYPE_F64",
                "bool" => "NX_TYPE_BOOL",
                "nx_string" => "NX_TYPE_STRING",
                "nx_array" => "NX_TYPE_ARRAY",
                _ => "NX_TYPE_UNKNOWN",
            };

            // Create nx_value initializer
            // For strings and arrays, we need to store them in temp variables and pass pointers
            if inferred_type == "nx_string" {
                let temp_var = format!("_str_{}", self.temp_var_counter);
                self.temp_var_counter += 1;
                temp_string_decls.push(format!("nx_string {} = {}", temp_var, arg_code));
                value_inits.push(format!(
                    "(nx_value){{{}, {{.as_string = &{}}}}}",
                    type_tag, temp_var
                ));
            } else if inferred_type == "nx_array" {
                let temp_var = format!("_arr_{}", self.temp_var_counter);
                self.temp_var_counter += 1;
                temp_string_decls.push(format!("nx_array {} = {}", temp_var, arg_code));
                value_inits.push(format!(
                    "(nx_value){{{}, {{.as_array = &{}}}}}",
                    type_tag, temp_var
                ));
            } else {
                // For other types, use appropriate union field
                let union_field = match inferred_type.as_str() {
                    "int64_t" => "as_i64",
                    "int32_t" => "as_i32",
                    "uint64_t" => "as_u64",
                    "uint32_t" => "as_u32",
                    "float" => "as_f32",
                    "double" => "as_f64",
                    "bool" => "as_bool",
                    _ => "as_ptr",
                };
                value_inits.push(format!(
                    "(nx_value){{{}, {{.{} = {}}}}}",
                    type_tag, union_field, arg_code
                ));
            }
        }

        // Generate the print call
        let count = args.len();
        let func_name = if name == "println" {
            "nx_compat_println"
        } else {
            "nx_compat_print"
        };

        // Add temp string declarations to prelude instead of using compound expressions
        for decl in temp_string_decls {
            self.statement_prelude.push(decl);
        }

        let array_init = format!("(nx_value[]){{ {} }}", value_inits.join(", "));
        Ok(format!("{}({}, {})", func_name, array_init, count))
    }

    /// Generate binary operation
    fn binary_op(&mut self, args: &[Expression], op: &str) -> NexusResult<String> {
        if args.len() != 2 {
            return Err(NexusError::TranspileError {
                message: format!("Binary operation expects 2 arguments, got {}", args.len()),
            });
        }

        let left = self.generate_expression(&args[0])?;
        let right = self.generate_expression(&args[1])?;
        Ok(format!("({} {} {})", left, op, right))
    }

    /// Generate unary operation
    fn unary_op(&mut self, args: &[Expression], op: &str) -> NexusResult<String> {
        if args.len() != 1 {
            return Err(NexusError::TranspileError {
                message: format!("Unary operation expects 1 argument, got {}", args.len()),
            });
        }

        let arg = self.generate_expression(&args[0])?;
        Ok(format!("({}{})", op, arg))
    }

    /// Generate a method call
    fn generate_method_call(&mut self, call: &MethodCallExpr) -> NexusResult<String> {
        // Check if this is a module-qualified function call (e.g., mathlib.abs())
        if let Expression::Variable(var_ref) = call.receiver.as_ref() {
            // This looks like a module-qualified call
            // Generate as: module_function(args) instead of function(&(module), args)
            let qualified_name = format!("{}_{}", var_ref.name, call.method);
            let mut args = Vec::new();
            for arg in &call.args {
                args.push(self.generate_expression(arg)?);
            }
            return Ok(format!("nx_{}({})", qualified_name, args.join(", ")));
        }

        // Otherwise, treat as a regular method call
        // In C, methods become regular functions with receiver as first argument
        let receiver = self.generate_expression(&call.receiver)?;

        let mut args = vec![format!("&({})", receiver)];
        for arg in &call.args {
            args.push(self.generate_expression(arg)?);
        }

        // We need to determine the receiver type to generate the correct function name
        // For now, use a generic pattern
        Ok(format!("{}({})", call.method, args.join(", ")))
    }

    /// Generate a macro call (expand at compile time)
    fn generate_macro_call(&mut self, macro_call: &MacroCallExpr) -> NexusResult<String> {
        // Use the macro expansion context (which supports cross-module macros)
        if let Some(ref macro_ctx) = self.macro_context {
            use nexus_lsp_server::macro_expansion::MacroExpansionResult;

            let expansion_result = macro_ctx.expand_macro(&macro_call.name, &macro_call.args);

            match expansion_result {
                MacroExpansionResult::Success { code } => {
                    // Parse the expanded code as an expression
                    let parsed_expr = nexus_parser::parse_expression(&code).map_err(|e| {
                        NexusError::RuntimeError {
                            message: format!(
                                "Failed to parse macro expansion of '{}': {}",
                                macro_call.name, e
                            ),
                            span: Some(macro_call.span),
                        }
                    })?;

                    // Generate C code from the expanded expression
                    return self.generate_expression(&parsed_expr);
                }
                MacroExpansionResult::RuntimeOnly { reason: _ } => {
                    // Arguments require runtime evaluation - can't expand at compile time
                    return Ok(format!("\"[MACRO:{} - runtime only]\"", macro_call.name));
                }
                MacroExpansionResult::NotFound => {
                    // Macro not found - might be a builtin or missing import
                    return Ok(format!("\"[MACRO:{} - not found]\"", macro_call.name));
                }
                MacroExpansionResult::Error { message } => {
                    return Err(NexusError::RuntimeError {
                        message: format!(
                            "Macro expansion error for '{}': {}",
                            macro_call.name, message
                        ),
                        span: Some(macro_call.span),
                    });
                }
            }
        }

        // Fallback: if macro context not available
        Ok(format!("\"[MACRO:{}]\"", macro_call.name))
    }

    /// Generate field access
    fn generate_field_access(&mut self, access: &FieldAccessExpr) -> NexusResult<String> {
        let object = self.generate_expression(&access.object)?;
        Ok(format!("{}.{}", object, access.field))
    }

    /// Generate index expression
    fn generate_index(&mut self, index: &IndexExpr) -> NexusResult<String> {
        let array = self.generate_expression(&index.array)?;
        let idx = self.generate_expression(&index.index)?;

        if self.config.bounds_checking && !index.unchecked {
            // For bounds-checked access, nx_array_get_checked returns void* which we need to cast and dereference
            // Default to int64_t for now; ideally we'd track the element type
            let elem_type = "int64_t";
            Ok(format!(
                "*(({}*)nx_array_get_checked(&({}), {}))",
                elem_type, array, idx
            ))
        } else {
            // For unchecked access, directly access the data pointer
            // Arrays are nx_array structs, so we need to access .data and cast appropriately
            // Default to int64_t for now; ideally we'd track the element type
            let elem_type = "int64_t";
            Ok(format!("(({}*)({}).data)[{}]", elem_type, array, idx))
        }
    }

    /// Generate array literal
    fn generate_array(&mut self, array: &ArrayExpr) -> NexusResult<String> {
        if array.elements.is_empty() {
            // Return empty dynamic array
            return Ok("nx_array_new(sizeof(void*))".to_string());
        }

        // Generate array as a runtime array creation with static data
        let elements: Vec<String> = array
            .elements
            .iter()
            .map(|elem| self.generate_expression(elem))
            .collect::<NexusResult<Vec<_>>>()?;

        // Determine element type from first element
        let elem_type = if !array.elements.is_empty() {
            self.infer_c_type_from_expr(&array.elements[0])?
        } else {
            "void*".to_string()
        };

        // Create a call to array creation helper
        Ok(format!(
            "nx_array_from_literal(({}[]){{ {} }}, {}, sizeof({}))",
            elem_type,
            elements.join(", "),
            array.elements.len(),
            elem_type
        ))
    }

    /// Generate struct initialization
    fn generate_struct_init(&mut self, init: &StructInitExpr) -> NexusResult<String> {
        let mut fields = Vec::new();

        // Get the struct definition to access default values
        let struct_def = self.struct_defs.get(&init.name).cloned();

        // Collect explicitly provided field names
        let provided_fields: HashSet<String> = init.fields.iter().map(|f| f.name.clone()).collect();

        // Process explicitly provided fields
        for field_init in &init.fields {
            let value = self.generate_expression(&field_init.value)?;
            fields.push(format!(".{} = {}", field_init.name, value));
        }

        // Process fields with defaults that weren't explicitly provided
        if let Some(struct_def) = struct_def {
            for field in &struct_def.fields {
                if !provided_fields.contains(&field.name) {
                    if let Some(default_expr) = &field.default {
                        // Generate a unique temporary variable for the default value
                        let temp_var = format!(
                            "_default_{}_{}_{}",
                            init.name, field.name, self.temp_var_counter
                        );
                        self.temp_var_counter += 1;
                        let c_type = self.nexus_type_to_c(&self.resolve_type_expr(&field.ty))?;
                        let default_value = self.generate_expression(default_expr)?;

                        // Add the temporary variable to the statement prelude (no semicolon, added when emitting)
                        self.statement_prelude
                            .push(format!("{} {} = {}", c_type, temp_var, default_value));

                        // Use the temporary variable in the initializer
                        fields.push(format!(".{} = {}", field.name, temp_var));
                    }
                }
            }
        }

        let prefixed_name = self.prefix_struct_name(&init.name);
        Ok(format!("({}){{{}}}", prefixed_name, fields.join(", ")))
    }

    /// Convert Nexus type to C type string
    fn nexus_type_to_c(&self, ty: &NexusType) -> NexusResult<String> {
        match ty {
            NexusType::Primitive(prim) => Ok(self.primitive_to_c(prim)),
            NexusType::Array(arr) => {
                // Check if this is a string type (array of runes)
                if let NexusType::Primitive(PrimitiveType::Rune) = *arr.element_type {
                    // This is a string (array of runes)
                    Ok("nx_string".to_string())
                } else {
                    // Regular array
                    Ok("nx_array".to_string())
                }
            }
            NexusType::Struct(s) => Ok(self.prefix_struct_name(&s.name)),
            NexusType::Interface(_) => Ok("void*".to_string()),
            NexusType::Unknown(_) => Ok("nx_unknown".to_string()),
            NexusType::Function(_) => Ok("void*".to_string()),
            NexusType::Macro => Ok("nx_string".to_string()),
            NexusType::Named(name) => {
                let resolved = self.type_registry.resolve_type(ty);
                if resolved == *ty {
                    // Check if this is a struct name and prefix it
                    if self.type_registry.get_struct(name).is_some() {
                        Ok(self.prefix_struct_name(name))
                    } else {
                        Ok(name.clone())
                    }
                } else {
                    self.nexus_type_to_c(&resolved)
                }
            }
            NexusType::TypeParam(_) => Ok("void*".to_string()),
            NexusType::Error => Ok("void".to_string()),
        }
    }

    /// Convert primitive type to C type
    fn primitive_to_c(&self, prim: &PrimitiveType) -> String {
        match prim {
            PrimitiveType::I8 => "int8_t",
            PrimitiveType::I16 => "int16_t",
            PrimitiveType::I32 => "int32_t",
            PrimitiveType::I64 => "int64_t",
            PrimitiveType::U8 => "uint8_t",
            PrimitiveType::U16 => "uint16_t",
            PrimitiveType::U32 => "uint32_t",
            PrimitiveType::U64 => "uint64_t",
            PrimitiveType::F32 => "float",
            PrimitiveType::F64 => "double",
            PrimitiveType::Bool => "bool",
            PrimitiveType::Rune => "uint32_t",
            PrimitiveType::Void => "void",
        }
        .to_string()
    }

    /// Resolve a type expression
    fn resolve_type_expr(&self, ty_expr: &TypeExpr) -> NexusType {
        match ty_expr {
            TypeExpr::Named { name, .. } => {
                let ty = NexusType::Named(name.clone());
                self.type_registry.resolve_type(&ty)
            }
            TypeExpr::Array { element, size, .. } => {
                let element_type = Box::new(self.resolve_type_expr(element));
                let array_size = match size {
                    ArraySizeExpr::Fixed(n) => ArraySize::Fixed(*n),
                    ArraySizeExpr::Dynamic => ArraySize::Dynamic,
                };
                NexusType::Array(nexus_types::ArrayType {
                    element_type,
                    size: array_size,
                    prealloc: None,
                })
            }
            TypeExpr::Void { .. } => NexusType::Primitive(PrimitiveType::Void),
            _ => NexusType::Error,
        }
    }

    /// Mangle a function name for C
    fn mangle_function_name(&self, name: &str) -> String {
        // Simple mangling: prefix with nx_
        format!("nx_{}", self.sanitize_identifier(name))
    }

    /// Map a Nexus builtin name to its C function name
    fn map_builtin_to_c(&self, name: &str) -> String {
        match name {
            // compat.io functions
            "print" => "nx_compat_print".to_string(),
            "println" => "nx_compat_println".to_string(),
            "readln" => "nx_compat_readln".to_string(),
            "read_file" => "nx_compat_read_file".to_string(),
            "write_file" => "nx_compat_write_file".to_string(),
            "file_exists" => "nx_compat_file_exists".to_string(),

            // compat.time functions
            "time_now_ms" => "nx_compat_time_now_ms".to_string(),
            "sleep_ms" => "nx_compat_sleep_ms".to_string(),

            // compat.process functions
            "exit" => "nx_compat_exit".to_string(),
            "getenv" => "nx_compat_getenv".to_string(),
            "args" => "nx_compat_args".to_string(),

            // plat functions
            "system" => "nx_plat_system".to_string(),
            "plat_name" => "nx_plat_name".to_string(),
            "plat_arch" => "nx_plat_arch".to_string(),

            // Everything else stays the same
            _ => name.to_string(),
        }
    }

    /// Sanitize an identifier for C
    fn sanitize_identifier(&self, name: &str) -> String {
        name.replace("$", "_dollar_")
            .replace("@", "_at_")
            .replace("!", "_bang_")
            .replace("?", "_question_")
    }

    /// Sanitize a label name for C (handles reserved keywords)
    fn sanitize_label(&self, name: &str) -> String {
        // Replace C keywords with safe alternatives
        let name = match name {
            "for" => "nx_for",
            "while" => "nx_while",
            "do" => "nx_do",
            "if" => "nx_if",
            "else" => "nx_else",
            "switch" => "nx_switch",
            "case" => "nx_case",
            "default" => "nx_default",
            "break" => "nx_break",
            "continue" => "nx_continue",
            "return" => "nx_return",
            "goto" => "nx_goto",
            _ => name,
        };
        name.to_string()
    }

    /// Infer C type from an expression
    fn infer_c_type_from_expr(&mut self, expr: &Expression) -> NexusResult<String> {
        match expr {
            Expression::Literal(lit) => match &lit.kind {
                LiteralKind::Int(_) => Ok("int64_t".to_string()),
                LiteralKind::Float(_) => Ok("double".to_string()),
                LiteralKind::String(_) => Ok("nx_string".to_string()),
                LiteralKind::Char(_) => Ok("uint32_t".to_string()),
                LiteralKind::Bool(_) => Ok("bool".to_string()),
                LiteralKind::None => Ok("void*".to_string()),
            },
            Expression::Array(_) => {
                // Arrays are nx_array type
                Ok("nx_array".to_string())
            }
            Expression::Variable(var_ref) => {
                // Look up variable type from our tracking map
                if let Some(var_type) = self.variable_types.get(&var_ref.name) {
                    Ok(var_type.clone())
                } else {
                    // Default to int64_t for unknown variables
                    Ok("int64_t".to_string())
                }
            }
            Expression::Call(call) => {
                // Infer return type based on known functions
                match call.function.as_str() {
                    "concat"
                    | "str"
                    | "nx_string_concat"
                    | "nx_string_from_value"
                    | "nx_string_from_cstr" => Ok("nx_string".to_string()),
                    // Arithmetic operations that may involve floats
                    "addf64" | "subf64" | "mulf64" | "divf64" | "negf64" => {
                        Ok("double".to_string())
                    }
                    "addf32" | "subf32" | "mulf32" | "divf32" | "negf32" => Ok("float".to_string()),
                    _ => {
                        // Look up function return type from our registry
                        if let Some(qualified) = self.function_to_module.get(&call.function)
                            && let Some(return_type) = self.function_return_types.get(qualified)
                        {
                            return Ok(return_type.clone());
                        }

                        // Try to infer from arguments
                        if !call.args.is_empty() {
                            let first_arg_type = self.infer_c_type_from_expr(&call.args[0])?;
                            if first_arg_type == "double" || first_arg_type == "float" {
                                return Ok(first_arg_type);
                            }
                        }
                        Ok("int64_t".to_string()) // Default for unknown functions
                    }
                }
            }
            Expression::MacroCall(_) => Ok("nx_string".to_string()), // Macros typically return strings
            Expression::Grouped(inner, _) => self.infer_c_type_from_expr(inner),
            Expression::MethodCall(method_call) => {
                // Check if this is a module-qualified function call
                if let Expression::Variable(var_ref) = method_call.receiver.as_ref() {
                    // Module-qualified call like mathlib.is_even()
                    let qualified_name = format!("{}_{}", var_ref.name, method_call.method);
                    if let Some(return_type) = self.function_return_types.get(&qualified_name) {
                        return Ok(return_type.clone());
                    }
                }
                Ok("int64_t".to_string()) // Default for method calls
            }
            Expression::StructInit(struct_init) => {
                // Return the prefixed struct type name
                Ok(self.prefix_struct_name(&struct_init.name))
            }
            Expression::FieldAccess(field_access) => {
                // Get the type of the object being accessed
                if let Expression::Variable(var_ref) = field_access.object.as_ref() {
                    // Look up the variable type
                    if let Some(var_type) = self.variable_types.get(&var_ref.name) {
                        // If it's a struct, look up the field type
                        if var_type.starts_with("nx_") {
                            // Extract struct name from prefixed type (nx_module_StructName)
                            let parts: Vec<&str> = var_type.split('_').collect();
                            if parts.len() >= 3 {
                                let struct_name = parts[2..].join("_");
                                if let Some(struct_def) =
                                    self.type_registry.get_struct(&struct_name)
                                {
                                    if let Some(field) = struct_def.get_field(&field_access.field) {
                                        return Ok(self.nexus_type_to_c(&field.field_type)?);
                                    }
                                }
                            }
                        }
                    }
                }
                Ok("int64_t".to_string()) // Default fallback for field access
            }
            _ => Ok("int64_t".to_string()), // Default fallback
        }
    }

    /// Generate indentation
    fn indent(&self, s: &str) -> String {
        let indent = "    ".repeat(self.indent_level);
        format!("{}{}", indent, s)
    }
}

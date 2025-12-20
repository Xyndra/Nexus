//! Type checker for Nexus programs.
//!
//! This module validates that all types are correct and compatible
//! before transpiling to C.

use nexus_core::{FunctionColor, NexusError, NexusResult};
use nexus_interpreter::BuiltinRegistry;
use nexus_parser::*;
use nexus_types::{
    ArraySize, ArrayType, FunctionType, InterfaceDef, InterfaceMethod, MethodParam, NexusType,
    PrimitiveType, StructDef, StructField, TypeRegistry, UnknownType,
};
use std::collections::{HashMap, HashSet};

/// Type information for expressions
#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub ty: NexusType,
    pub span: nexus_core::Span,
}

/// Type checker for Nexus programs
pub struct TypeChecker<'a> {
    type_registry: &'a mut TypeRegistry,
    variables: HashMap<String, NexusType>,
    functions: HashMap<String, FunctionSignature>,
    methods: HashMap<String, Vec<MethodSignature>>,
    current_function_color: Option<FunctionColor>,
    current_return_type: Option<NexusType>,
    scope_depth: usize,
    subscope_depth: usize,
    builtins: BuiltinRegistry,
    /// Imported modules for module-qualified calls (e.g., use mathlib)
    imported_modules: HashSet<String>,
}

#[derive(Debug, Clone)]
struct FunctionSignature {
    params: Vec<NexusType>,
    return_type: NexusType,
    color: FunctionColor,
}

#[derive(Debug, Clone)]
struct MethodSignature {
    receiver_type: String,
    _receiver_mutable: bool,
    params: Vec<NexusType>,
    return_type: NexusType,
    _color: FunctionColor,
}

impl<'a> TypeChecker<'a> {
    pub fn new(type_registry: &'a mut TypeRegistry) -> Self {
        let mut checker = Self {
            type_registry,
            variables: HashMap::new(),
            functions: HashMap::new(),
            methods: HashMap::new(),
            current_function_color: None,
            current_return_type: None,
            scope_depth: 0,
            subscope_depth: 0,
            builtins: BuiltinRegistry::new(),
            imported_modules: HashSet::new(),
        };
        checker.register_builtins();
        checker
    }

    /// Register all builtin functions in the type checker
    fn register_builtins(&mut self) {
        // Register all std builtins
        for builtin in self.builtins.iter() {
            let params: Vec<NexusType> = builtin
                .params
                .iter()
                .map(|p| Self::parse_type_string(p.ty))
                .collect();

            let return_type = Self::parse_type_string(builtin.return_type);

            self.functions.insert(
                builtin.name.clone(),
                FunctionSignature {
                    params,
                    return_type,
                    color: builtin.color,
                },
            );
        }

        // Register compat.io builtins
        for builtin in self.builtins.iter_compat_io() {
            let params: Vec<NexusType> = builtin
                .params
                .iter()
                .map(|p| Self::parse_type_string(p.ty))
                .collect();

            let return_type = Self::parse_type_string(builtin.return_type);

            self.functions.insert(
                builtin.name.clone(),
                FunctionSignature {
                    params,
                    return_type,
                    color: builtin.color,
                },
            );
        }

        // Register compat.fs builtins
        for builtin in self.builtins.iter_compat_fs() {
            let params: Vec<NexusType> = builtin
                .params
                .iter()
                .map(|p| Self::parse_type_string(p.ty))
                .collect();

            let return_type = Self::parse_type_string(builtin.return_type);

            self.functions.insert(
                builtin.name.clone(),
                FunctionSignature {
                    params,
                    return_type,
                    color: builtin.color,
                },
            );
        }

        // Register compat.proc builtins
        for builtin in self.builtins.iter_compat_proc() {
            let params: Vec<NexusType> = builtin
                .params
                .iter()
                .map(|p| Self::parse_type_string(p.ty))
                .collect();

            let return_type = Self::parse_type_string(builtin.return_type);

            self.functions.insert(
                builtin.name.clone(),
                FunctionSignature {
                    params,
                    return_type,
                    color: builtin.color,
                },
            );
        }
    }

    /// Parse a type string like "i64", "[]rune", "void" into NexusType
    fn parse_type_string(type_str: &str) -> NexusType {
        // Handle variadic types (like "...any")
        if type_str.starts_with("...") || type_str == "any" {
            // For variadic/any types, we'll use a special marker that accepts anything
            // We'll represent this as an Error type which is assignable from anything
            return NexusType::Error;
        }

        match type_str {
            "void" => NexusType::Primitive(PrimitiveType::Void),
            "bool" => NexusType::Primitive(PrimitiveType::Bool),
            "i8" => NexusType::Primitive(PrimitiveType::I8),
            "i16" => NexusType::Primitive(PrimitiveType::I16),
            "i32" => NexusType::Primitive(PrimitiveType::I32),
            "i64" => NexusType::Primitive(PrimitiveType::I64),
            "u8" => NexusType::Primitive(PrimitiveType::U8),
            "u16" => NexusType::Primitive(PrimitiveType::U16),
            "u32" => NexusType::Primitive(PrimitiveType::U32),
            "u64" => NexusType::Primitive(PrimitiveType::U64),
            "f32" => NexusType::Primitive(PrimitiveType::F32),
            "f64" => NexusType::Primitive(PrimitiveType::F64),
            "rune" => NexusType::Primitive(PrimitiveType::Rune),
            // "str" is an alias for [dyn]rune (dynamic array of runes)
            "str" | "[]rune" => NexusType::Array(ArrayType {
                element_type: Box::new(NexusType::Primitive(PrimitiveType::Rune)),
                size: ArraySize::Dynamic,
                prealloc: None,
            }),
            "[]u8" => NexusType::Array(ArrayType {
                element_type: Box::new(NexusType::Primitive(PrimitiveType::U8)),
                size: ArraySize::Dynamic,
                prealloc: None,
            }),
            _ => {
                // Handle array types like "[dyn]T", "[N]T", or "[T]" (shorthand for []T)
                if type_str.starts_with('[') {
                    // Find the matching closing bracket (handling nested brackets)
                    let mut depth = 0;
                    let mut end = None;
                    for (i, c) in type_str.char_indices() {
                        match c {
                            '[' => depth += 1,
                            ']' => {
                                depth -= 1;
                                if depth == 0 {
                                    end = Some(i);
                                    break;
                                }
                            }
                            _ => {}
                        }
                    }

                    if let Some(end) = end {
                        let inside_brackets = &type_str[1..end];
                        let after_brackets = &type_str[end + 1..];

                        // Check if this is [T] format (element type inside brackets, nothing after)
                        // vs [dyn]T or [N]T format (size inside brackets, element type after)
                        if after_brackets.is_empty() && !inside_brackets.is_empty() {
                            // This is [T] format - element type is inside brackets
                            let elem_type = Self::parse_type_string(inside_brackets);
                            return NexusType::Array(ArrayType {
                                element_type: Box::new(elem_type),
                                size: ArraySize::Dynamic,
                                prealloc: None,
                            });
                        } else {
                            // This is [dyn]T or [N]T format - element type is after brackets
                            let elem_type = Self::parse_type_string(after_brackets);
                            return NexusType::Array(ArrayType {
                                element_type: Box::new(elem_type),
                                size: ArraySize::Dynamic,
                                prealloc: None,
                            });
                        }
                    }
                }
                // Unknown type, treat as named type
                NexusType::Named(type_str.to_string())
            }
        }
    }

    /// Register all functions and methods from a program (without checking bodies)
    pub fn register_functions(&mut self, program: &Program, module_name: &str) -> NexusResult<()> {
        // Register all functions
        for item in &program.items {
            if let Item::Function(func) = item {
                let param_types = func
                    .params
                    .iter()
                    .map(|p| self.resolve_type_expr(&p.ty))
                    .collect();

                let return_type = self.resolve_type_expr(&func.return_type);

                let sig = FunctionSignature {
                    params: param_types,
                    return_type,
                    color: func.color,
                };

                // Register with simple name
                self.functions.insert(func.name.clone(), sig.clone());

                // Also register with module-qualified name (e.g., "mathlib.abs")
                let qualified_name = format!("{}.{}", module_name, func.name);
                self.functions.insert(qualified_name, sig);
            } else if let Item::Method(method) = item {
                let param_types = method
                    .params
                    .iter()
                    .map(|p| self.resolve_type_expr(&p.ty))
                    .collect();

                let return_type = self.resolve_type_expr(&method.return_type);

                self.methods
                    .entry(method.name.clone())
                    .or_default()
                    .push(MethodSignature {
                        receiver_type: method.receiver_type.clone(),
                        _receiver_mutable: method.receiver_mutable,
                        params: param_types,
                        return_type,
                        _color: method.color,
                    });
            }
        }

        Ok(())
    }

    /// Register all types from a program (structs, interfaces)
    pub fn register_types(&mut self, program: &Program) -> NexusResult<()> {
        // First pass: register all struct and interface names
        for item in &program.items {
            match item {
                Item::Struct(s) => {
                    let mut struct_def = StructDef::new(s.name.clone(), s.span);

                    for f in &s.fields {
                        let ty = self.resolve_type_expr(&f.ty);
                        let field = StructField::new(f.name.clone(), ty, f.span);
                        struct_def.add_field(field);
                    }

                    // Add interface implementations
                    for impl_name in &s.implements {
                        struct_def.add_impl(impl_name.clone());
                    }

                    self.type_registry.register_struct(struct_def);
                }
                Item::Interface(i) => {
                    let mut interface_def = InterfaceDef::new(i.name.clone(), i.span);

                    for m in &i.methods {
                        let mut method = InterfaceMethod::new(
                            m.name.clone(),
                            self.resolve_type_expr(&m.return_type),
                            m.span,
                        )
                        .with_color(m.color);

                        if m.receiver_mutable {
                            method = method.with_mutable_receiver();
                        }

                        for p in &m.params {
                            let param = MethodParam::new(
                                p.name.clone(),
                                self.resolve_type_expr(&p.ty),
                                p.span,
                            );
                            method = method.with_param(param);
                        }

                        interface_def.add_method(method);
                    }

                    for ext in &i.extends {
                        interface_def.extend(ext.clone());
                    }

                    self.type_registry.register_interface(interface_def);
                }
                _ => {}
            }
        }

        Ok(())
    }

    /// Check an entire program for type correctness
    pub fn check_program(&mut self, program: &Program) -> NexusResult<()> {
        // First, process use statements to track imported modules
        for item in &program.items {
            if let Item::Use(use_stmt) = item {
                self.process_use_statement(use_stmt);
            }
        }

        // Check each function body (functions should already be registered)
        for item in &program.items {
            match item {
                Item::Function(func) => self.check_function(func)?,
                Item::Method(method) => self.check_method(method)?,
                _ => {}
            }
        }

        Ok(())
    }

    /// Process use statements to track module imports
    fn process_use_statement(&mut self, use_stmt: &UseStatement) {
        // If symbols is empty, it's a module-level import (e.g., use mathlib)
        if use_stmt.symbols.is_empty() {
            let module_name = use_stmt.module_path.join(".");
            self.imported_modules.insert(module_name);
        }
    }

    /// Check a function definition
    fn check_function(&mut self, func: &FunctionDef) -> NexusResult<()> {
        self.push_scope();
        self.current_function_color = Some(func.color);
        self.current_return_type = Some(self.resolve_type_expr(&func.return_type));

        // Add parameters to scope
        for param in &func.params {
            let param_type = self.resolve_type_expr(&param.ty);
            self.variables.insert(param.name.clone(), param_type);
        }

        // Check function body
        self.check_block(&func.body)?;

        self.current_return_type = None;
        self.current_function_color = None;
        self.pop_scope();

        Ok(())
    }

    /// Check a method definition
    fn check_method(&mut self, method: &MethodDef) -> NexusResult<()> {
        self.push_scope();
        self.current_function_color = Some(method.color);
        self.current_return_type = Some(self.resolve_type_expr(&method.return_type));

        // Add receiver to scope
        let receiver_type = NexusType::Named(method.receiver_type.clone());
        let receiver_type = self.type_registry.resolve_type(&receiver_type);
        self.variables
            .insert(method.receiver_name.clone(), receiver_type);

        // Add parameters to scope
        for param in &method.params {
            let param_type = self.resolve_type_expr(&param.ty);
            self.variables.insert(param.name.clone(), param_type);
        }

        // Check method body
        self.check_block(&method.body)?;

        self.current_return_type = None;
        self.current_function_color = None;
        self.pop_scope();

        Ok(())
    }

    /// Check a block of statements
    fn check_block(&mut self, block: &Block) -> NexusResult<()> {
        for statement in &block.statements {
            self.check_statement(statement)?;
        }
        Ok(())
    }

    /// Check a single statement
    fn check_statement(&mut self, stmt: &Statement) -> NexusResult<()> {
        match stmt {
            Statement::VarDecl(decl) => self.check_var_decl(decl),
            Statement::Assignment(assign) => self.check_assignment(assign),
            Statement::Expression(expr) => {
                self.check_expression(expr)?;
                Ok(())
            }
            Statement::Return(ret) => self.check_return(ret),
            Statement::If(if_stmt) => self.check_if(if_stmt),
            Statement::Defer(defer) => self.check_block(&defer.body),
            Statement::Subscope(subscope) => self.check_subscope(subscope),
            Statement::Goto(_) => Ok(()), // Goto doesn't need type checking
            Statement::Block(block) => self.check_block(block),
        }
    }

    /// Check variable declaration
    fn check_var_decl(&mut self, decl: &VarDecl) -> NexusResult<()> {
        // Validate modifiers
        if !decl.modifiers.is_valid() {
            return Err(NexusError::TypeError {
                message: format!("Invalid variable modifiers: {}", decl.modifiers),
                span: decl.span,
            });
        }

        // Infer or check type
        let var_type = if let Some(ty_expr) = &decl.ty {
            self.resolve_type_expr(ty_expr)
        } else {
            self.check_expression(&decl.init)?.ty
        };

        // Check initializer
        let init_type = self.check_expression(&decl.init)?;
        if !init_type.ty.is_assignable_to(&var_type) {
            return Err(NexusError::TypeError {
                message: format!(
                    "Cannot assign value of type '{}' to variable '{}' of type '{}'",
                    init_type.ty, decl.name, var_type
                ),
                span: decl.span,
            });
        }

        self.variables.insert(decl.name.clone(), var_type);
        Ok(())
    }

    /// Check assignment statement
    fn check_assignment(&mut self, assign: &Assignment) -> NexusResult<()> {
        let target_type = self.check_expression(&assign.target)?;
        let value_type = self.check_expression(&assign.value)?;

        if !value_type.ty.is_assignable_to(&target_type.ty) {
            return Err(NexusError::TypeError {
                message: format!(
                    "Cannot assign value of type '{}' to target of type '{}'",
                    value_type.ty, target_type.ty
                ),
                span: assign.span,
            });
        }

        Ok(())
    }

    /// Check return statement
    fn check_return(&mut self, ret: &ReturnStmt) -> NexusResult<()> {
        // If we're inside a subscope, return acts as a break and should have no value
        if self.subscope_depth > 0 {
            if ret.value.is_some() {
                return Err(NexusError::TypeError {
                    message: "Return statement inside subscope cannot have a value (use bare 'return' to break from subscope)".to_string(),
                    span: ret.span,
                });
            }
            return Ok(());
        }

        // Normal return statement (not in subscope)
        let expected_type =
            self.current_return_type
                .as_ref()
                .ok_or_else(|| NexusError::TypeError {
                    message: "Return statement outside of function".to_string(),
                    span: ret.span,
                })?;

        let expected_type_clone = expected_type.clone();
        if let Some(value) = &ret.value {
            let value_type = self.check_expression(value)?;
            if !value_type.ty.is_assignable_to(&expected_type_clone) {
                return Err(NexusError::TypeError {
                    message: format!(
                        "Cannot return value of type '{}', expected '{}'",
                        value_type.ty, expected_type_clone
                    ),
                    span: ret.span,
                });
            }
        } else if !expected_type_clone.is_void() {
            return Err(NexusError::TypeError {
                message: format!(
                    "Function must return a value of type '{}'",
                    expected_type_clone
                ),
                span: ret.span,
            });
        }

        Ok(())
    }

    /// Check if statement
    fn check_if(&mut self, if_stmt: &IfStmt) -> NexusResult<()> {
        match &if_stmt.condition {
            IfCondition::Boolean(expr) => {
                let _cond_type = self.check_expression(expr)?;
                // In Nexus, any type can be used as a condition (truthiness)
                // so we don't need to strictly check for bool
            }
            IfCondition::Pattern { matcher, cases } => {
                let _matcher_type = self.check_expression(matcher)?;
                for case in cases {
                    match &case.body {
                        PatternBody::Expression(expr) => {
                            self.check_expression(expr)?;
                        }
                        PatternBody::Block(block) => {
                            self.check_block(block)?;
                        }
                    }
                }
            }
        }

        self.check_block(&if_stmt.then_block)?;

        if let Some(ref else_clause) = if_stmt.else_block {
            match else_clause {
                ElseClause::Block(block) => self.check_block(block)?,
                ElseClause::ElseIf(else_if) => self.check_if(else_if)?,
            }
        }

        Ok(())
    }

    /// Check subscope statement
    fn check_subscope(&mut self, subscope: &SubscopeStmt) -> NexusResult<()> {
        self.push_scope();
        self.subscope_depth += 1;
        self.check_block(&subscope.body)?;
        self.subscope_depth -= 1;
        self.pop_scope();
        Ok(())
    }

    /// Check an expression and return its type
    pub fn check_expression(&mut self, expr: &Expression) -> NexusResult<TypeInfo> {
        let (ty, span) = match expr {
            Expression::Literal(lit) => (self.check_literal(lit), lit.span),
            Expression::Variable(var) => (self.check_variable(var)?, var.span),
            Expression::Call(call) => (self.check_call(call)?, call.span),
            Expression::MethodCall(method_call) => {
                (self.check_method_call(method_call)?, method_call.span)
            }
            Expression::MacroCall(macro_call) => {
                // Macros are evaluated at compile time, treat as string for now
                (
                    NexusType::Array(ArrayType {
                        element_type: Box::new(NexusType::Primitive(PrimitiveType::Rune)),
                        size: ArraySize::Dynamic,
                        prealloc: None,
                    }),
                    macro_call.span,
                )
            }
            Expression::FieldAccess(field_access) => {
                (self.check_field_access(field_access)?, field_access.span)
            }
            Expression::Index(index) => (self.check_index(index)?, index.span),
            Expression::Array(array) => (self.check_array(array)?, array.span),
            Expression::StructInit(struct_init) => {
                (self.check_struct_init(struct_init)?, struct_init.span)
            }
            Expression::Lambda(lambda) => (self.check_lambda(lambda)?, lambda.span),
            Expression::Grouped(inner, _span) => return self.check_expression(inner),
        };

        Ok(TypeInfo { ty, span })
    }

    /// Check a literal expression
    fn check_literal(&self, lit: &Literal) -> NexusType {
        match &lit.kind {
            LiteralKind::Int(_) => NexusType::Primitive(PrimitiveType::I64),
            LiteralKind::Float(_) => NexusType::Primitive(PrimitiveType::F64),
            LiteralKind::String(_) => NexusType::Array(ArrayType {
                element_type: Box::new(NexusType::Primitive(PrimitiveType::Rune)),
                size: ArraySize::Dynamic,
                prealloc: None,
            }),
            LiteralKind::Char(_) => NexusType::Primitive(PrimitiveType::Rune),
            LiteralKind::Bool(_) => NexusType::Primitive(PrimitiveType::Bool),
            LiteralKind::None => NexusType::Named("None".to_string()),
        }
    }

    /// Check a variable reference
    fn check_variable(&self, var: &VariableRef) -> NexusResult<NexusType> {
        self.variables
            .get(&var.name)
            .cloned()
            .ok_or_else(|| NexusError::TypeError {
                message: format!("Undefined variable '{}'", var.name),
                span: var.span,
            })
    }

    /// Check a function call
    fn check_call(&mut self, call: &CallExpr) -> NexusResult<NexusType> {
        // Get function signature
        let func_sig = self
            .functions
            .get(&call.function)
            .ok_or_else(|| NexusError::TypeError {
                message: format!("Undefined function '{}'", call.function),
                span: call.span,
            })?;

        // Check color compatibility
        if let Some(current_color) = self.current_function_color
            && !current_color.can_call(func_sig.color)
        {
            return Err(NexusError::TypeError {
                message: format!(
                    "{} function cannot call {} function '{}'",
                    current_color, func_sig.color, call.function
                ),
                span: call.span,
            });
        }

        // Check argument count (skip for variadic functions with Error type params)
        let has_variadic = func_sig
            .params
            .iter()
            .any(|p| matches!(p, NexusType::Error));
        if !has_variadic && call.args.len() != func_sig.params.len() {
            return Err(NexusError::TypeError {
                message: format!(
                    "Function '{}' expects {} arguments, got {}",
                    call.function,
                    func_sig.params.len(),
                    call.args.len()
                ),
                span: call.span,
            });
        }

        // Check argument types
        let params_clone = func_sig.params.clone();
        let return_type = func_sig.return_type.clone();
        for (i, arg) in call.args.iter().enumerate() {
            // For variadic functions, stop checking after the variadic parameter
            if i >= params_clone.len() {
                break;
            }
            let expected_type = &params_clone[i];
            // Skip type checking for Error type (represents any/variadic)
            if matches!(expected_type, NexusType::Error) {
                continue;
            }
            let arg_type = self.check_expression(arg)?;

            // Resolve both types to handle Named types like "str"
            let resolved_expected = self.type_registry.resolve_type(expected_type);
            let resolved_arg = self.type_registry.resolve_type(&arg_type.ty);

            if !resolved_arg.is_assignable_to(&resolved_expected) {
                return Err(NexusError::TypeError {
                    message: format!(
                        "Argument {} to '{}': expected type '{}', got '{}'",
                        i + 1,
                        call.function,
                        expected_type,
                        arg_type.ty
                    ),
                    span: call.span,
                });
            }
        }

        Ok(return_type)
    }

    /// Check a method call
    fn check_method_call(&mut self, call: &MethodCallExpr) -> NexusResult<NexusType> {
        // Check if this is a module-qualified function call (e.g., mathlib.abs())
        if let Expression::Variable(var_ref) = call.receiver.as_ref() {
            let module_name = &var_ref.name;

            // Check if this is an imported module
            if self.imported_modules.contains(module_name) {
                // Look up the function by its qualified name
                let qualified_name = format!("{}.{}", module_name, call.method);

                // Try to find the function
                if let Some(func_sig) = self.functions.get(&qualified_name).cloned() {
                    // Check argument count
                    if call.args.len() != func_sig.params.len() {
                        return Err(NexusError::TypeError {
                            message: format!(
                                "Function '{}' expects {} arguments, got {}",
                                qualified_name,
                                func_sig.params.len(),
                                call.args.len()
                            ),
                            span: call.span,
                        });
                    }

                    // Check argument types
                    for (i, arg) in call.args.iter().enumerate() {
                        let expected_type = &func_sig.params[i];
                        let arg_type = self.check_expression(arg)?;

                        let resolved_expected = self.type_registry.resolve_type(expected_type);
                        let resolved_arg = self.type_registry.resolve_type(&arg_type.ty);

                        if !resolved_arg.is_assignable_to(&resolved_expected) {
                            return Err(NexusError::TypeError {
                                message: format!(
                                    "Argument {} to '{}': expected type '{}', got '{}'",
                                    i + 1,
                                    qualified_name,
                                    expected_type,
                                    arg_type.ty
                                ),
                                span: call.span,
                            });
                        }
                    }

                    return Ok(func_sig.return_type);
                }

                // Function not found in this module
                return Err(NexusError::TypeError {
                    message: format!("Undefined function '{}'", qualified_name),
                    span: call.span,
                });
            }
        }

        let receiver_type = self.check_expression(&call.receiver)?;

        // Find matching method
        let methods = self
            .methods
            .get(&call.method)
            .ok_or_else(|| NexusError::TypeError {
                message: format!("Undefined method '{}'", call.method),
                span: call.span,
            })?;

        // Find method that matches receiver type
        let method_sig = methods
            .iter()
            .find(|m| {
                let expected = NexusType::Named(m.receiver_type.clone());
                let expected = self.type_registry.resolve_type(&expected);
                receiver_type.ty.is_assignable_to(&expected)
            })
            .ok_or_else(|| NexusError::TypeError {
                message: format!(
                    "No method '{}' found for type '{}'",
                    call.method, receiver_type.ty
                ),
                span: call.span,
            })?;

        // Check argument count
        if call.args.len() != method_sig.params.len() {
            return Err(NexusError::TypeError {
                message: format!(
                    "Method '{}' expects {} arguments, got {}",
                    call.method,
                    method_sig.params.len(),
                    call.args.len()
                ),
                span: call.span,
            });
        }

        // Check argument types
        let params_clone = method_sig.params.clone();
        let return_type = method_sig.return_type.clone();
        for (i, arg) in call.args.iter().enumerate() {
            let expected_type = &params_clone[i];
            let arg_type = self.check_expression(arg)?;
            if !arg_type.ty.is_assignable_to(expected_type) {
                return Err(NexusError::TypeError {
                    message: format!(
                        "Argument {} to method '{}': expected type '{}', got '{}'",
                        i + 1,
                        call.method,
                        expected_type,
                        arg_type.ty
                    ),
                    span: call.span,
                });
            }
        }

        Ok(return_type)
    }

    /// Check field access
    fn check_field_access(&mut self, access: &FieldAccessExpr) -> NexusResult<NexusType> {
        let object_type = self.check_expression(&access.object)?;

        // Resolve the type in case it's a Named type
        let resolved_type = self.type_registry.resolve_type(&object_type.ty);

        match &resolved_type {
            NexusType::Struct(struct_def) => {
                let field = struct_def.get_field(&access.field);
                field
                    .map(|f| f.field_type.clone())
                    .ok_or_else(|| NexusError::TypeError {
                        message: format!(
                            "Struct '{}' has no field '{}'",
                            struct_def.name, access.field
                        ),
                        span: access.span,
                    })
            }
            _ => Err(NexusError::TypeError {
                message: format!(
                    "Cannot access field '{}' on non-struct type '{}'",
                    access.field, object_type.ty
                ),
                span: access.span,
            }),
        }
    }

    /// Check index expression
    fn check_index(&mut self, index: &IndexExpr) -> NexusResult<NexusType> {
        let array_type = self.check_expression(&index.array)?;
        let index_type = self.check_expression(&index.index)?;

        // Index must be an integer
        if !matches!(
            index_type.ty,
            NexusType::Primitive(PrimitiveType::I32 | PrimitiveType::I64)
        ) {
            return Err(NexusError::TypeError {
                message: format!("Array index must be an integer, got '{}'", index_type.ty),
                span: index.span,
            });
        }

        // Resolve the array type in case it contains Named element types
        let resolved_array_type = self.type_registry.resolve_type(&array_type.ty);

        // Get element type from array
        match &resolved_array_type {
            NexusType::Array(arr) => Ok((*arr.element_type).clone()),
            _ => {
                // Provide more context for debugging
                let array_expr_info = match &*index.array {
                    Expression::Variable(var) => format!(" (variable '{}')", var.name),
                    Expression::FieldAccess(fa) => format!(" (field access '.{}')", fa.field),
                    _ => String::new(),
                };
                Err(NexusError::TypeError {
                    message: format!(
                        "Cannot index non-array type '{}'{}",
                        array_type.ty, array_expr_info
                    ),
                    span: index.span,
                })
            }
        }
    }

    /// Check array literal
    fn check_array(&mut self, array: &ArrayExpr) -> NexusResult<NexusType> {
        if array.elements.is_empty() {
            // Empty array, we can't infer element type
            return Ok(NexusType::Array(ArrayType {
                element_type: Box::new(NexusType::Error),
                size: ArraySize::Fixed(0),
                prealloc: None,
            }));
        }

        // Infer element type from first element
        let first_type = self.check_expression(&array.elements[0])?.ty;

        // Check all elements have compatible type
        for elem in &array.elements[1..] {
            let elem_type = self.check_expression(elem)?;
            if !elem_type.ty.is_assignable_to(&first_type) {
                return Err(NexusError::TypeError {
                    message: format!(
                        "Array elements must have compatible types, got '{}' and '{}'",
                        first_type, elem_type.ty
                    ),
                    span: array.span,
                });
            }
        }

        Ok(NexusType::Array(ArrayType {
            element_type: Box::new(first_type),
            size: ArraySize::Fixed(array.elements.len()),
            prealloc: None,
        }))
    }

    /// Check struct initialization
    fn check_struct_init(&mut self, init: &StructInitExpr) -> NexusResult<NexusType> {
        let struct_def =
            self.type_registry
                .get_struct(&init.name)
                .ok_or_else(|| NexusError::TypeError {
                    message: format!("Undefined struct '{}'", init.name),
                    span: init.span,
                })?;

        // Check all initialized fields exist and have correct types
        for field_init in &init.fields {
            let field_def = struct_def
                .fields
                .iter()
                .find(|f| f.name == field_init.name)
                .ok_or_else(|| NexusError::TypeError {
                    message: format!("Struct '{}' has no field '{}'", init.name, field_init.name),
                    span: field_init.span,
                })?;

            let value_type = self.check_expression(&field_init.value)?;
            if !value_type.ty.is_assignable_to(&field_def.field_type) {
                return Err(NexusError::TypeError {
                    message: format!(
                        "Field '{}' of struct '{}': expected type '{}', got '{}'",
                        field_init.name, init.name, field_def.field_type, value_type.ty
                    ),
                    span: field_init.span,
                });
            }
        }

        Ok(NexusType::Struct(struct_def))
    }

    /// Check lambda expression
    fn check_lambda(&mut self, lambda: &LambdaExpr) -> NexusResult<NexusType> {
        self.push_scope();

        let param_types: Vec<NexusType> = lambda
            .params
            .iter()
            .map(|p| {
                let ty = self.resolve_type_expr(&p.ty);
                self.variables.insert(p.name.clone(), ty.clone());
                ty
            })
            .collect();

        let return_type = if let Some(ref ret_ty) = lambda.return_type {
            self.resolve_type_expr(ret_ty)
        } else {
            NexusType::Primitive(PrimitiveType::Void)
        };

        match &lambda.body {
            LambdaBody::Expression(expr) => {
                let expr_type = self.check_expression(expr)?;
                if !expr_type.ty.is_assignable_to(&return_type) {
                    return Err(NexusError::TypeError {
                        message: format!(
                            "Lambda body expression has type '{}', expected '{}'",
                            expr_type.ty, return_type
                        ),
                        span: lambda.span,
                    });
                }
            }
            LambdaBody::Block(block) => {
                let old_return_type = self.current_return_type.clone();
                self.current_return_type = Some(return_type.clone());
                self.check_block(block)?;
                self.current_return_type = old_return_type;
            }
        }

        self.pop_scope();

        Ok(NexusType::Function(FunctionType {
            params: param_types,
            return_type: Box::new(return_type),
            color: FunctionColor::Std,
        }))
    }

    /// Resolve a type expression to a concrete type
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
                NexusType::Array(ArrayType {
                    element_type,
                    size: array_size,
                    prealloc: None,
                })
            }
            TypeExpr::Unknown { variants, .. } => {
                let variant_types = variants.iter().map(|v| self.resolve_type_expr(v)).collect();
                NexusType::Unknown(UnknownType {
                    variants: variant_types,
                })
            }
            TypeExpr::Function {
                params,
                return_type,
                ..
            } => {
                let param_types = params.iter().map(|p| self.resolve_type_expr(p)).collect();
                let ret_type = Box::new(self.resolve_type_expr(return_type));
                NexusType::Function(FunctionType {
                    params: param_types,
                    return_type: ret_type,
                    color: FunctionColor::Std,
                })
            }
            TypeExpr::Void { .. } => NexusType::Primitive(PrimitiveType::Void),
        }
    }

    fn push_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn pop_scope(&mut self) {
        self.scope_depth = self.scope_depth.saturating_sub(1);
        // In a real implementation, we'd want to remove variables from this scope
    }
}

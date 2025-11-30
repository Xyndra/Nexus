//! Abstract Syntax Tree (AST) definitions for the Nexus programming language.
//!
//! This module defines all AST nodes that represent a parsed Nexus program.
//! Note: There are no binary or unary operators - all operations are function calls
//! like `addi64(a, b)`, `not(x)`, `eqi64(a, b)`, etc.

use nexus_core::{FunctionColor, Span, VarModifiers};

/// A complete Nexus program
#[derive(Debug, Clone)]
pub struct Program {
    /// Top-level items in the program
    pub items: Vec<Item>,
    /// Source span
    pub span: Span,
}

impl Program {
    pub fn new(items: Vec<Item>, span: Span) -> Self {
        Self { items, span }
    }

    /// Get all function definitions
    pub fn functions(&self) -> impl Iterator<Item = &FunctionDef> {
        self.items.iter().filter_map(|item| match item {
            Item::Function(f) => Some(f),
            _ => None,
        })
    }

    /// Get all struct definitions
    pub fn structs(&self) -> impl Iterator<Item = &StructDefAst> {
        self.items.iter().filter_map(|item| match item {
            Item::Struct(s) => Some(s),
            _ => None,
        })
    }

    /// Get all interface definitions
    pub fn interfaces(&self) -> impl Iterator<Item = &InterfaceDefAst> {
        self.items.iter().filter_map(|item| match item {
            Item::Interface(i) => Some(i),
            _ => None,
        })
    }
}

/// Top-level items in a program
#[derive(Debug, Clone)]
pub enum Item {
    /// Use statement (import)
    Use(UseStatement),
    /// Function definition
    Function(FunctionDef),
    /// Method definition (function on a struct)
    Method(MethodDef),
    /// Struct definition
    Struct(StructDefAst),
    /// Interface definition
    Interface(InterfaceDefAst),
    /// Macro definition
    Macro(MacroDef),
}

/// A use statement for importing symbols from modules
/// Example: `use { println } from compat.io`
#[derive(Debug, Clone)]
pub struct UseStatement {
    /// The symbols being imported
    pub symbols: Vec<String>,
    /// The module path (e.g., ["compat", "io"])
    pub module_path: Vec<String>,
    /// Source span
    pub span: Span,
}

impl Item {
    /// Get the span of this item
    pub fn span(&self) -> &Span {
        match self {
            Item::Use(u) => &u.span,
            Item::Function(f) => &f.span,
            Item::Method(m) => &m.span,
            Item::Struct(s) => &s.span,
            Item::Interface(i) => &i.span,
            Item::Macro(m) => &m.span,
        }
    }
}

/// Function definition
#[derive(Debug, Clone)]
pub struct FunctionDef {
    /// Function color (std, compat, plat)
    pub color: FunctionColor,
    /// Function name
    pub name: String,
    /// Span of just the function name
    pub name_span: Span,
    /// Parameters
    pub params: Vec<Parameter>,
    /// Return type
    pub return_type: TypeExpr,
    /// Return type contracts
    pub return_contracts: Vec<ContractExpr>,
    /// Function body
    pub body: Block,
    /// Source span
    pub span: Span,
}

/// Method definition (function on a struct)
#[derive(Debug, Clone)]
pub struct MethodDef {
    /// Function color (std, compat, plat)
    pub color: FunctionColor,
    /// Whether the receiver is mutable
    pub receiver_mutable: bool,
    /// Receiver type name
    pub receiver_type: String,
    /// Receiver parameter name
    pub receiver_name: String,
    /// Method name
    pub name: String,
    /// Span of just the method name
    pub name_span: Span,
    /// Parameters (not including receiver)
    pub params: Vec<Parameter>,
    /// Return type
    pub return_type: TypeExpr,
    /// Return type contracts
    pub return_contracts: Vec<ContractExpr>,
    /// Method body
    pub body: Block,
    /// Source span
    pub span: Span,
}

/// Function/method parameter
#[derive(Debug, Clone)]
pub struct Parameter {
    /// Parameter name
    pub name: String,
    /// Parameter type
    pub ty: TypeExpr,
    /// Contracts on this parameter
    pub contracts: Vec<ContractExpr>,
    /// Source span
    pub span: Span,
}

/// Struct definition (AST representation)
#[derive(Debug, Clone)]
pub struct StructDefAst {
    /// Struct name
    pub name: String,
    /// Span of just the struct name
    pub name_span: Span,
    /// Interfaces this struct implements
    pub implements: Vec<String>,
    /// Fields
    pub fields: Vec<FieldDef>,
    /// Source span
    pub span: Span,
}

/// Field definition in a struct
#[derive(Debug, Clone)]
pub struct FieldDef {
    /// Field name
    pub name: String,
    /// Field type
    pub ty: TypeExpr,
    /// Default value expression
    pub default: Option<Expression>,
    /// Annotations (like @prealloc)
    pub annotations: Vec<Annotation>,
    /// Source span
    pub span: Span,
}

/// Interface definition (AST representation)
#[derive(Debug, Clone)]
pub struct InterfaceDefAst {
    /// Interface name
    pub name: String,
    /// Span of just the interface name
    pub name_span: Span,
    /// Extended interfaces
    pub extends: Vec<String>,
    /// Method signatures
    pub methods: Vec<MethodSignature>,
    /// Source span
    pub span: Span,
}

/// Method signature in an interface
#[derive(Debug, Clone)]
pub struct MethodSignature {
    /// Function color requirement
    pub color: FunctionColor,
    /// Whether the receiver is mutable
    pub receiver_mutable: bool,
    /// Method name
    pub name: String,
    /// Parameters
    pub params: Vec<Parameter>,
    /// Return type
    pub return_type: TypeExpr,
    /// Source span
    pub span: Span,
}

/// Macro definition
#[derive(Debug, Clone)]
pub struct MacroDef {
    /// Macro name (without $)
    pub name: String,
    /// Parameters
    pub params: Vec<Parameter>,
    /// Macro body (must return macro type)
    pub body: Block,
    /// Source span
    pub span: Span,
}

/// Type expression (AST representation of types)
#[derive(Debug, Clone)]
pub enum TypeExpr {
    /// Named type (primitive, struct, or interface name)
    Named { name: String, span: Span },
    /// Array type: [N]T or [dyn]T
    Array {
        element: Box<TypeExpr>,
        size: ArraySizeExpr,
        span: Span,
    },
    /// Unknown sum type: unknown<A, B, C>
    Unknown { variants: Vec<TypeExpr>, span: Span },
    /// Function type (for lambdas)
    Function {
        params: Vec<TypeExpr>,
        return_type: Box<TypeExpr>,
        span: Span,
    },
    /// Void type
    Void { span: Span },
}

impl TypeExpr {
    /// Get the span of this type expression
    pub fn span(&self) -> &Span {
        match self {
            TypeExpr::Named { span, .. } => span,
            TypeExpr::Array { span, .. } => span,
            TypeExpr::Unknown { span, .. } => span,
            TypeExpr::Function { span, .. } => span,
            TypeExpr::Void { span } => span,
        }
    }

    /// Create a void type
    pub fn void(span: Span) -> Self {
        TypeExpr::Void { span }
    }

    /// Create a named type
    pub fn named(name: impl Into<String>, span: Span) -> Self {
        TypeExpr::Named {
            name: name.into(),
            span,
        }
    }
}

/// Array size expression
#[derive(Debug, Clone)]
pub enum ArraySizeExpr {
    /// Fixed size
    Fixed(usize),
    /// Dynamic size
    Dynamic,
}

/// Contract expression
#[derive(Debug, Clone)]
pub struct ContractExpr {
    /// Contract kind
    pub kind: ContractKindExpr,
    /// Source span
    pub span: Span,
}

/// Contract kinds (AST representation)
#[derive(Debug, Clone)]
pub enum ContractKindExpr {
    /// @range(min, max)
    Range {
        min: Box<Expression>,
        max: Box<Expression>,
    },
    /// @not_null
    NotNull,
    /// @predicate(func_name)
    Predicate(String),
    /// @custom("expression")
    Custom(String),
}

/// Annotation on fields or parameters
#[derive(Debug, Clone)]
pub struct Annotation {
    /// Annotation name (without @)
    pub name: String,
    /// Arguments
    pub args: Vec<Expression>,
    /// Source span
    pub span: Span,
}

/// A block of statements
#[derive(Debug, Clone)]
pub struct Block {
    /// Statements in the block
    pub statements: Vec<Statement>,
    /// Source span
    pub span: Span,
}

impl Block {
    pub fn new(statements: Vec<Statement>, span: Span) -> Self {
        Self { statements, span }
    }

    pub fn empty(span: Span) -> Self {
        Self {
            statements: Vec::new(),
            span,
        }
    }
}

/// Statements
#[derive(Debug, Clone)]
pub enum Statement {
    /// Variable declaration
    VarDecl(VarDecl),
    /// Assignment
    Assignment(Assignment),
    /// Expression statement
    Expression(Expression),
    /// Return statement
    Return(ReturnStmt),
    /// If statement
    If(IfStmt),
    /// Defer statement
    Defer(DeferStmt),
    /// Subscope declaration
    Subscope(SubscopeStmt),
    /// Goto statement
    Goto(GotoStmt),
    /// Block statement
    Block(Block),
}

impl Statement {
    pub fn span(&self) -> &Span {
        match self {
            Statement::VarDecl(v) => &v.span,
            Statement::Assignment(a) => &a.span,
            Statement::Expression(e) => e.span(),
            Statement::Return(r) => &r.span,
            Statement::If(i) => &i.span,
            Statement::Defer(d) => &d.span,
            Statement::Subscope(s) => &s.span,
            Statement::Goto(g) => &g.span,
            Statement::Block(b) => &b.span,
        }
    }
}

/// Variable declaration
#[derive(Debug, Clone)]
pub struct VarDecl {
    /// Variable modifiers (m, l, h, u, g)
    pub modifiers: VarModifiers,
    /// Variable name
    pub name: String,
    /// Optional type annotation
    pub ty: Option<TypeExpr>,
    /// Initializer expression
    pub init: Expression,
    /// Source span
    pub span: Span,
}

/// Assignment
#[derive(Debug, Clone)]
pub struct Assignment {
    /// Target expression
    pub target: Expression,
    /// Value expression
    pub value: Expression,
    /// Source span
    pub span: Span,
}

/// Return statement
#[derive(Debug, Clone)]
pub struct ReturnStmt {
    /// Return value (None for void return)
    pub value: Option<Expression>,
    /// Source span
    pub span: Span,
}

/// If statement
#[derive(Debug, Clone)]
pub struct IfStmt {
    /// Condition
    pub condition: IfCondition,
    /// Then block
    pub then_block: Block,
    /// Else block (optional)
    pub else_block: Option<ElseClause>,
    /// Source span
    pub span: Span,
}

/// If condition types
#[derive(Debug, Clone)]
pub enum IfCondition {
    /// Boolean expression: if (expr) { ... }
    Boolean(Expression),
    /// Pattern matching: if matcher(x) { 10 -> ...; 20 -> ... }
    Pattern {
        matcher: Expression,
        cases: Vec<PatternCase>,
    },
}

/// Pattern case in pattern-matching if
#[derive(Debug, Clone)]
pub struct PatternCase {
    /// Pattern to match
    pub pattern: Pattern,
    /// Body (expression or block)
    pub body: PatternBody,
    /// Source span
    pub span: Span,
}

/// Pattern for matching
#[derive(Debug, Clone)]
pub enum Pattern {
    /// Literal value
    Literal(Literal),
    /// Wildcard (_)
    Wildcard(Span),
    /// Binding (name)
    Binding { name: String, span: Span },
}

/// Body of a pattern case
#[derive(Debug, Clone)]
pub enum PatternBody {
    /// Single expression
    Expression(Expression),
    /// Block
    Block(Block),
}

/// Else clause
#[derive(Debug, Clone)]
pub enum ElseClause {
    /// else { ... }
    Block(Block),
    /// else if ...
    ElseIf(Box<IfStmt>),
}

/// Defer statement
#[derive(Debug, Clone)]
pub struct DeferStmt {
    /// Deferred block
    pub body: Block,
    /// Source span
    pub span: Span,
}

/// Subscope statement
#[derive(Debug, Clone)]
pub struct SubscopeStmt {
    /// Subscope name
    pub name: String,
    /// Subscope body
    pub body: Block,
    /// Source span
    pub span: Span,
}

/// Goto statement (jump to a previously defined subscope label)
#[derive(Debug, Clone)]
pub struct GotoStmt {
    /// Label to jump to
    pub label: String,
    /// Source span
    pub span: Span,
}

/// Expressions
///
/// Note: All operations are function calls. There are no binary or unary operators.
/// Examples:
/// - Addition: `addi64(a, b)`
/// - Subtraction: `subi64(a, b)`
/// - Negation: `negi64(x)`
/// - Logical not: `not(x)`
/// - Equality: `eqi64(a, b)`
/// - Less than: `lti64(a, b)`
#[derive(Debug, Clone)]
pub enum Expression {
    /// Literal value
    Literal(Literal),
    /// Variable reference
    Variable(VariableRef),
    /// Function call (includes all operations like addi64, not, eqi64, etc.)
    Call(CallExpr),
    /// Method call
    MethodCall(MethodCallExpr),
    /// Macro invocation
    MacroCall(MacroCallExpr),
    /// Field access
    FieldAccess(FieldAccessExpr),
    /// Index access
    Index(IndexExpr),
    /// Array literal
    Array(ArrayExpr),
    /// Struct instantiation
    StructInit(StructInitExpr),
    /// Lambda expression
    Lambda(LambdaExpr),
    /// Grouped expression (parenthesized)
    Grouped(Box<Expression>, Span),
}

impl Expression {
    /// Get the span of this expression
    pub fn span(&self) -> &Span {
        match self {
            Expression::Literal(l) => &l.span,
            Expression::Variable(v) => &v.span,
            Expression::Call(c) => &c.span,
            Expression::MethodCall(m) => &m.span,
            Expression::MacroCall(m) => &m.span,
            Expression::FieldAccess(f) => &f.span,
            Expression::Index(i) => &i.span,
            Expression::Array(a) => &a.span,
            Expression::StructInit(s) => &s.span,
            Expression::Lambda(l) => &l.span,
            Expression::Grouped(_, span) => span,
        }
    }
}

/// Literal values
#[derive(Debug, Clone)]
pub struct Literal {
    /// The literal kind
    pub kind: LiteralKind,
    /// Source span
    pub span: Span,
}

/// Literal kinds
#[derive(Debug, Clone)]
pub enum LiteralKind {
    /// Integer literal
    Int(i64),
    /// Float literal
    Float(f64),
    /// String literal
    String(String),
    /// Character literal
    Char(char),
    /// Boolean literal
    Bool(bool),
    /// None literal
    None,
}

/// Variable reference
#[derive(Debug, Clone)]
pub struct VariableRef {
    /// Variable name
    pub name: String,
    /// Source span
    pub span: Span,
}

/// Function call expression
/// This is used for ALL operations including arithmetic, logic, comparison, etc.
#[derive(Debug, Clone)]
pub struct CallExpr {
    /// Function name
    pub function: String,
    /// Arguments
    pub args: Vec<Expression>,
    /// Source span
    pub span: Span,
}

/// Method call expression
#[derive(Debug, Clone)]
pub struct MethodCallExpr {
    /// Receiver expression
    pub receiver: Box<Expression>,
    /// Method name
    pub method: String,
    /// Arguments
    pub args: Vec<Expression>,
    /// Source span
    pub span: Span,
}

/// Macro call expression
#[derive(Debug, Clone)]
pub struct MacroCallExpr {
    /// Macro name (without $)
    pub name: String,
    /// Arguments
    pub args: Vec<Expression>,
    /// Source span
    pub span: Span,
}

/// Field access expression
#[derive(Debug, Clone)]
pub struct FieldAccessExpr {
    /// Object expression
    pub object: Box<Expression>,
    /// Field name
    pub field: String,
    /// Source span
    pub span: Span,
}

/// Index expression
#[derive(Debug, Clone)]
pub struct IndexExpr {
    /// Array expression
    pub array: Box<Expression>,
    /// Index expression
    pub index: Box<Expression>,
    /// Whether bounds checking is disabled (@unchecked)
    pub unchecked: bool,
    /// Source span
    pub span: Span,
}

/// Array literal expression
#[derive(Debug, Clone)]
pub struct ArrayExpr {
    /// Elements
    pub elements: Vec<Expression>,
    /// Source span
    pub span: Span,
}

/// Struct instantiation expression
#[derive(Debug, Clone)]
pub struct StructInitExpr {
    /// Struct name
    pub name: String,
    /// Field initializers
    pub fields: Vec<FieldInit>,
    /// Source span
    pub span: Span,
}

/// Field initializer
#[derive(Debug, Clone)]
pub struct FieldInit {
    /// Field name
    pub name: String,
    /// Value expression
    pub value: Expression,
    /// Source span
    pub span: Span,
}

/// Lambda expression
#[derive(Debug, Clone)]
pub struct LambdaExpr {
    /// Captured variables (for closures)
    pub captures: Vec<String>,
    /// Parameters
    pub params: Vec<Parameter>,
    /// Return type (optional, can be inferred)
    pub return_type: Option<TypeExpr>,
    /// Lambda body
    pub body: LambdaBody,
    /// Source span
    pub span: Span,
}

/// Lambda body
#[derive(Debug, Clone)]
pub enum LambdaBody {
    /// Single expression
    Expression(Box<Expression>),
    /// Block
    Block(Block),
}

/// Helper to create expressions
impl Expression {
    /// Create an integer literal
    pub fn int(value: i64, span: Span) -> Self {
        Expression::Literal(Literal {
            kind: LiteralKind::Int(value),
            span,
        })
    }

    /// Create a float literal
    pub fn float(value: f64, span: Span) -> Self {
        Expression::Literal(Literal {
            kind: LiteralKind::Float(value),
            span,
        })
    }

    /// Create a string literal
    pub fn string(value: impl Into<String>, span: Span) -> Self {
        Expression::Literal(Literal {
            kind: LiteralKind::String(value.into()),
            span,
        })
    }

    /// Create a boolean literal
    pub fn bool(value: bool, span: Span) -> Self {
        Expression::Literal(Literal {
            kind: LiteralKind::Bool(value),
            span,
        })
    }

    /// Create a variable reference
    pub fn var(name: impl Into<String>, span: Span) -> Self {
        Expression::Variable(VariableRef {
            name: name.into(),
            span,
        })
    }

    /// Create a function call
    pub fn call(function: impl Into<String>, args: Vec<Expression>, span: Span) -> Self {
        Expression::Call(CallExpr {
            function: function.into(),
            args,
            span,
        })
    }

    /// Create a None literal
    pub fn none(span: Span) -> Self {
        Expression::Literal(Literal {
            kind: LiteralKind::None,
            span,
        })
    }
}

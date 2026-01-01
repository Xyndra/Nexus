//! Error types for the Nexus language.

use thiserror::Error;

use crate::Span;

/// The main error type for Nexus language operations.
#[derive(Debug, Error)]
pub enum NexusError {
    // === Lexer Errors ===
    #[error("Lexer error at line {line}, column {column}: {message}")]
    LexerError {
        message: String,
        line: u32,
        column: u32,
    },

    // === Parser Errors ===
    #[error("Parse error at {span}: {message}")]
    ParseError { message: String, span: Span },

    #[error("Unexpected token: expected {expected}, found {found}")]
    UnexpectedToken {
        expected: String,
        found: String,
        span: Span,
    },

    #[error("Unexpected end of input")]
    UnexpectedEof { span: Span },

    // === Type Errors ===
    #[error("Type error: {message}")]
    TypeError { message: String, span: Span },

    #[error("Type mismatch: expected {expected}, found {found}")]
    TypeMismatch {
        expected: String,
        found: String,
        span: Span,
    },

    #[error("Unknown type: {name}")]
    UnknownType { name: String, span: Span },

    #[error("Interface not implemented: {interface} is not implemented for {struct_name}")]
    InterfaceNotImplemented {
        interface: String,
        struct_name: String,
        span: Span,
    },

    // === Name Resolution Errors ===
    #[error("Undefined variable: {name}")]
    UndefinedVariable { name: String, span: Span },

    #[error("Undefined function: {name}")]
    UndefinedFunction { name: String, span: Span },

    #[error("Variable already defined: {name}")]
    VariableAlreadyDefined { name: String, span: Span },

    #[error("Function already defined: {name}")]
    FunctionAlreadyDefined { name: String, span: Span },

    #[error("Cannot mutate immutable variable: {name}")]
    ImmutableVariable { name: String, span: Span },

    #[error("Invalid modifiers: {message}")]
    InvalidModifiers { message: String, span: Span },

    // === Function Coloring Errors ===
    #[error(
        "Color violation: {caller_color} function cannot call {callee_color} function '{callee_name}'"
    )]
    ColorViolation {
        caller_color: String,
        callee_color: String,
        callee_name: String,
        span: Span,
    },

    // === Contract Errors ===
    #[error("Contract violation: {message}")]
    ContractViolation { message: String, span: Span },

    #[error("Precondition failed: {message}")]
    PreconditionFailed { message: String, span: Span },

    #[error("Postcondition failed: {message}")]
    PostconditionFailed { message: String, span: Span },

    // === Runtime Errors ===
    #[error("Runtime error: {message}")]
    RuntimeError { message: String, span: Option<Span> },

    #[error("Index out of bounds: index {index} is out of range for array of length {length}")]
    IndexOutOfBounds {
        index: i64,
        length: usize,
        span: Span,
    },

    #[error("Division by zero")]
    DivisionByZero { span: Span },

    #[error("Integer overflow")]
    IntegerOverflow { span: Span },

    #[error("Null pointer dereference")]
    NullPointer { span: Span },

    // === Permission Errors ===
    #[error("Permission denied: {permission} is not allowed for {context}")]
    PermissionDenied {
        permission: String,
        context: String,
        span: Span,
    },

    #[error("Macro requires approval for {color} call to '{function}'")]
    MacroApprovalRequired {
        color: String,
        function: String,
        span: Span,
    },

    // === Module Errors ===
    #[error("Module already defined: {name}")]
    ModuleAlreadyDefined { name: String },

    #[error("Module not found: {name}")]
    ModuleNotFound { name: String, span: Span },

    #[error("Circular dependency detected: {path}")]
    CircularDependency { path: String, span: Span },

    #[error("Warning: accessing underscore-prefixed item '{name}' from external module")]
    UnderscorePrefixWarning { name: String, span: Span },

    // === IO Errors ===
    #[error("IO error: {message}")]
    IoError { message: String },

    // === Transpile Errors ===
    #[error("Transpile error: {message}")]
    TranspileError { message: String },

    // === Sandbox Errors ===
    #[error("Sandbox violation: {message}")]
    SandboxViolation { message: String, span: Option<Span> },

    // === Internal Errors ===
    #[error("Internal error: {message}")]
    InternalError { message: String },

    /// Error wrapper that carries optional filename/source context. Display delegates to the
    /// inner error (so existing formatting is preserved).
    #[error("{inner}")]
    WithContext {
        inner: Box<NexusError>,
        filename: Option<String>,
        source_text: Option<String>,
    },
}

impl NexusError {
    /// Get the span associated with this error, if any.
    pub fn span(&self) -> Option<&Span> {
        match self {
            NexusError::LexerError { .. } => None,
            NexusError::ParseError { span, .. } => Some(span),
            NexusError::UnexpectedToken { span, .. } => Some(span),
            NexusError::UnexpectedEof { span } => Some(span),
            NexusError::TypeError { span, .. } => Some(span),
            NexusError::TypeMismatch { span, .. } => Some(span),
            NexusError::UnknownType { span, .. } => Some(span),
            NexusError::InterfaceNotImplemented { span, .. } => Some(span),
            NexusError::UndefinedVariable { span, .. } => Some(span),
            NexusError::UndefinedFunction { span, .. } => Some(span),
            NexusError::VariableAlreadyDefined { span, .. } => Some(span),
            NexusError::FunctionAlreadyDefined { span, .. } => Some(span),
            NexusError::ImmutableVariable { span, .. } => Some(span),
            NexusError::InvalidModifiers { span, .. } => Some(span),
            NexusError::ColorViolation { span, .. } => Some(span),
            NexusError::ContractViolation { span, .. } => Some(span),
            NexusError::PreconditionFailed { span, .. } => Some(span),
            NexusError::PostconditionFailed { span, .. } => Some(span),
            NexusError::RuntimeError { span, .. } => span.as_ref(),
            NexusError::IndexOutOfBounds { span, .. } => Some(span),
            NexusError::DivisionByZero { span } => Some(span),
            NexusError::IntegerOverflow { span } => Some(span),
            NexusError::NullPointer { span } => Some(span),
            NexusError::PermissionDenied { span, .. } => Some(span),
            NexusError::MacroApprovalRequired { span, .. } => Some(span),
            NexusError::ModuleAlreadyDefined { .. } => None,
            NexusError::ModuleNotFound { span, .. } => Some(span),
            NexusError::CircularDependency { span, .. } => Some(span),
            NexusError::UnderscorePrefixWarning { span, .. } => Some(span),
            NexusError::IoError { .. } => None,
            NexusError::TranspileError { .. } => None,
            NexusError::SandboxViolation { span, .. } => span.as_ref(),
            NexusError::InternalError { .. } => None,
            NexusError::WithContext { inner, .. } => inner.span(),
        }
    }

    /// Check if this error is a warning rather than a hard error.
    pub fn is_warning(&self) -> bool {
        match self {
            NexusError::WithContext { inner, .. } => inner.is_warning(),
            _ => matches!(self, NexusError::UnderscorePrefixWarning { .. }),
        }
    }

    /// Format this error with an optional source snippet and an optional filename.
    ///
    /// If this error is a `WithContext` wrapper, prefer the context embedded in the wrapper
    /// unless explicit `source` / `filename` arguments are provided. Otherwise fall back to
    /// rendering this error as a diagnostic.
    pub fn format_with_source(&self, source: Option<&str>, filename: Option<&str>) -> String {
        if let NexusError::WithContext {
            inner,
            filename: ctx_file,
            source_text: ctx_source_text,
        } = self
        {
            // Prefer explicit arguments, otherwise use the context stored in the wrapper.
            let src = source.or_else(|| ctx_source_text.as_deref());
            let fname = filename.or_else(|| ctx_file.as_deref());
            return inner.format_with_source(src, fname);
        }

        let severity = if self.is_warning() {
            DiagnosticSeverity::Warning
        } else {
            DiagnosticSeverity::Error
        };

        let diagnostic = Diagnostic {
            severity,
            message: self.to_string(),
            span: self.span().cloned(),
            hints: Vec::new(),
        };

        diagnostic.format(source)
    }

    /// Wrap this error with optional filename/source context so callers that have the source
    /// text available can automatically render a snippet.
    pub fn with_context(self, filename: Option<String>, source_text: Option<String>) -> NexusError {
        NexusError::WithContext {
            inner: Box::new(self),
            filename,
            source_text,
        }
    }
}

impl From<std::io::Error> for NexusError {
    fn from(err: std::io::Error) -> Self {
        NexusError::IoError {
            message: err.to_string(),
        }
    }
}

/// A diagnostic message with severity level.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: DiagnosticSeverity,
    pub message: String,
    pub span: Option<Span>,
    pub hints: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Info,
    Hint,
}

impl Diagnostic {
    pub fn error(message: impl Into<String>) -> Self {
        Self {
            severity: DiagnosticSeverity::Error,
            message: message.into(),
            span: None,
            hints: Vec::new(),
        }
    }

    pub fn warning(message: impl Into<String>) -> Self {
        Self {
            severity: DiagnosticSeverity::Warning,
            message: message.into(),
            span: None,
            hints: Vec::new(),
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_hint(mut self, hint: impl Into<String>) -> Self {
        self.hints.push(hint.into());
        self
    }

    /// Format this diagnostic optionally using the provided source text and filename.
    ///
    /// If `source` is provided and the diagnostic has an associated `Span`, the resulting string
    /// will include the source line containing the span and a caret marker highlighting the span.
    pub fn format(&self, source: Option<&str>) -> String {
        let sev_str = match self.severity {
            DiagnosticSeverity::Error => "error",
            DiagnosticSeverity::Warning => "warning",
            DiagnosticSeverity::Info => "info",
            DiagnosticSeverity::Hint => "hint",
        };

        let mut out = format!("{}: {}", sev_str, self.message);

        if let Some(span) = &self.span {
            if let Some(src) = source {
                // Try to render a snippet (line + marker) using Span helper.
                if let Some(snippet) = span.format_snippet(src) {
                    out.push('\n');
                    out.push_str(&snippet);
                } else {
                    out.push_str(&format!("\n  --> {}\n", span));
                }
            } else {
                // No source available; just show location.
                out.push_str(&format!("\n  --> {}\n", span));
            }
        }

        if !self.hints.is_empty() {
            out.push_str("\nHints:\n");
            for hint in &self.hints {
                out.push_str(&format!("  - {}\n", hint));
            }
        }

        out
    }
}

impl From<NexusError> for Diagnostic {
    fn from(err: NexusError) -> Self {
        let severity = if err.is_warning() {
            DiagnosticSeverity::Warning
        } else {
            DiagnosticSeverity::Error
        };

        Diagnostic {
            severity,
            message: err.to_string(),
            span: err.span().cloned(),
            hints: Vec::new(),
        }
    }
}

/// A collection of diagnostics for reporting multiple issues.
#[derive(Debug, Default)]
pub struct Diagnostics {
    items: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, diagnostic: Diagnostic) {
        self.items.push(diagnostic);
    }

    pub fn error(&mut self, message: impl Into<String>, span: Span) {
        self.push(Diagnostic::error(message).with_span(span));
    }

    pub fn warning(&mut self, message: impl Into<String>, span: Span) {
        self.push(Diagnostic::warning(message).with_span(span));
    }

    pub fn has_errors(&self) -> bool {
        self.items
            .iter()
            .any(|d| d.severity == DiagnosticSeverity::Error)
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Diagnostic> {
        self.items.iter()
    }

    pub fn into_vec(self) -> Vec<Diagnostic> {
        self.items
    }

    pub fn error_count(&self) -> usize {
        self.items
            .iter()
            .filter(|d| d.severity == DiagnosticSeverity::Error)
            .count()
    }

    pub fn warning_count(&self) -> usize {
        self.items
            .iter()
            .filter(|d| d.severity == DiagnosticSeverity::Warning)
            .count()
    }
}

impl IntoIterator for Diagnostics {
    type Item = Diagnostic;
    type IntoIter = std::vec::IntoIter<Diagnostic>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

impl<'a> IntoIterator for &'a Diagnostics {
    type Item = &'a Diagnostic;
    type IntoIter = std::slice::Iter<'a, Diagnostic>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.iter()
    }
}

impl Extend<Diagnostic> for Diagnostics {
    fn extend<T: IntoIterator<Item = Diagnostic>>(&mut self, iter: T) {
        self.items.extend(iter);
    }
}

/// Result type alias for Nexus operations.
pub type NexusResult<T> = Result<T, NexusError>;

//! Language Server Protocol implementation for the Nexus programming language.
//!
//! This crate provides the LSP server that enables IDE features like
//! code completion, diagnostics, hover information, and more.

use nexus_core::{Diagnostics, Span};
use nexus_parser::{Program, parse};
use nexus_types::TypeRegistry;
use std::collections::HashMap;

/// The Nexus Language Server
pub struct Lsp {
    /// Open documents (URI -> content)
    documents: HashMap<String, Document>,
    /// Type registry shared across documents
    #[allow(dead_code)]
    type_registry: TypeRegistry,
    /// Diagnostics per document
    diagnostics: HashMap<String, Diagnostics>,
    /// Configuration
    config: LspConfig,
}

/// A document being edited
#[derive(Debug, Clone)]
pub struct Document {
    /// Document URI
    pub uri: String,
    /// Document content
    pub content: String,
    /// Parsed AST (if successfully parsed)
    pub ast: Option<Program>,
    /// Version number
    pub version: i32,
}

/// LSP configuration
#[derive(Debug, Clone)]
pub struct LspConfig {
    /// Whether to show warnings for underscore-prefixed access from other modules
    pub warn_underscore_access: bool,
    /// Whether to enable all diagnostics
    pub enable_diagnostics: bool,
}

impl Default for LspConfig {
    fn default() -> Self {
        Self {
            warn_underscore_access: true,
            enable_diagnostics: true,
        }
    }
}

/// Position in a document
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    /// Line number (0-based)
    pub line: u32,
    /// Column number (0-based, in UTF-16 code units for LSP compatibility)
    pub character: u32,
}

/// A range in a document
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Range {
    /// Start position
    pub start: Position,
    /// End position
    pub end: Position,
}

/// Hover information
#[derive(Debug, Clone)]
pub struct HoverInfo {
    /// The content to display
    pub contents: String,
    /// The range this hover applies to
    pub range: Option<Range>,
}

/// A completion item
#[derive(Debug, Clone)]
pub struct CompletionItem {
    /// The label to display
    pub label: String,
    /// The kind of completion
    pub kind: CompletionKind,
    /// Detail information
    pub detail: Option<String>,
    /// Documentation
    pub documentation: Option<String>,
    /// Text to insert
    pub insert_text: Option<String>,
}

/// Kinds of completion items
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompletionKind {
    Function,
    Variable,
    Struct,
    Interface,
    Field,
    Keyword,
    Builtin,
    Macro,
}

/// A symbol in the document
#[derive(Debug, Clone)]
pub struct DocumentSymbol {
    /// Symbol name
    pub name: String,
    /// Symbol kind
    pub kind: SymbolKind,
    /// Range of the symbol
    pub range: Range,
    /// Range of the symbol's name
    pub selection_range: Range,
    /// Children symbols
    pub children: Vec<DocumentSymbol>,
}

/// Kinds of symbols
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Function,
    Method,
    Struct,
    Interface,
    Field,
    Variable,
    Macro,
}

/// Go to definition result
#[derive(Debug, Clone)]
pub struct Location {
    /// Document URI
    pub uri: String,
    /// Range in the document
    pub range: Range,
}

impl Lsp {
    /// Create a new LSP instance
    pub fn new() -> Self {
        Self {
            documents: HashMap::new(),
            type_registry: TypeRegistry::new(),
            diagnostics: HashMap::new(),
            config: LspConfig::default(),
        }
    }

    /// Create with custom configuration
    pub fn with_config(config: LspConfig) -> Self {
        Self {
            documents: HashMap::new(),
            type_registry: TypeRegistry::new(),
            diagnostics: HashMap::new(),
            config,
        }
    }

    /// Open a document
    pub fn open_document(&mut self, uri: String, content: String, version: i32) {
        let ast = self.parse_content(&content);
        let diagnostics = self.compute_diagnostics(&uri, &content, &ast);

        self.documents.insert(
            uri.clone(),
            Document {
                uri: uri.clone(),
                content,
                ast,
                version,
            },
        );

        self.diagnostics.insert(uri, diagnostics);
    }

    /// Update a document
    pub fn update_document(&mut self, uri: &str, content: String, version: i32) {
        let ast = self.parse_content(&content);
        let diagnostics = self.compute_diagnostics(uri, &content, &ast);

        if let Some(doc) = self.documents.get_mut(uri) {
            doc.content = content;
            doc.ast = ast;
            doc.version = version;
        }

        self.diagnostics.insert(uri.to_string(), diagnostics);
    }

    /// Close a document
    pub fn close_document(&mut self, uri: &str) {
        self.documents.remove(uri);
        self.diagnostics.remove(uri);
    }

    /// Get diagnostics for a document
    pub fn get_diagnostics(&self, uri: &str) -> Option<&Diagnostics> {
        self.diagnostics.get(uri)
    }

    /// Get hover information at a position
    pub fn hover(&self, uri: &str, position: Position) -> Option<HoverInfo> {
        let doc = self.documents.get(uri)?;
        let ast = doc.ast.as_ref()?;

        // Find the token/node at the position
        let offset = self.position_to_offset(&doc.content, position)?;

        // Look for function definitions
        for func in ast.functions() {
            if func.span.contains(offset) {
                let params: Vec<String> = func.params.iter().map(|p| p.name.to_string()).collect();

                let content = format!(
                    "```nexus\n{} {}({}): {}\n```\n\nFunction color: `{}`",
                    func.color,
                    func.name,
                    params.join(", "),
                    "...", // TODO: format return type
                    func.color
                );

                return Some(HoverInfo {
                    contents: content,
                    range: Some(self.span_to_range(&doc.content, &func.span)),
                });
            }
        }

        // Look for structs
        for struct_def in ast.structs() {
            if struct_def.span.contains(offset) {
                let content = format!(
                    "```nexus\nstruct {}\n```\n\nFields: {}",
                    struct_def.name,
                    struct_def.fields.len()
                );

                return Some(HoverInfo {
                    contents: content,
                    range: Some(self.span_to_range(&doc.content, &struct_def.span)),
                });
            }
        }

        None
    }

    /// Get completions at a position
    pub fn completions(&self, uri: &str, _position: Position) -> Vec<CompletionItem> {
        let mut items = Vec::new();

        // Add keywords
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
            "break",
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

        // Add builtin functions
        let builtins = [
            ("not", "Logical NOT"),
            ("and", "Logical AND"),
            ("or", "Logical OR"),
            ("addi64", "Add two i64 values"),
            ("subi64", "Subtract two i64 values"),
            ("muli64", "Multiply two i64 values"),
            ("divi64", "Divide two i64 values"),
            ("eqi64", "Check equality of two i64 values"),
            ("lti64", "Check if first i64 is less than second"),
            ("gti64", "Check if first i64 is greater than second"),
            ("len", "Get length of array/string"),
            ("push", "Push element to array"),
            ("pop", "Pop element from array"),
            ("concat", "Concatenate two arrays/strings"),
            ("println", "Print with newline"),
            ("print", "Print without newline"),
            ("typeof", "Get type name of value"),
            ("is_none", "Check if value is None"),
            ("unwrap", "Unwrap optional value"),
            ("assert", "Assert condition is true"),
            ("assert_eq", "Assert two values are equal"),
        ];

        for (name, doc) in builtins {
            items.push(CompletionItem {
                label: name.to_string(),
                kind: CompletionKind::Builtin,
                detail: Some("builtin function".to_string()),
                documentation: Some(doc.to_string()),
                insert_text: Some(format!("{}()", name)),
            });
        }

        // Add variable modifiers as snippets
        let modifiers = [
            ("m", "mutable variable"),
            ("mh", "mutable heap variable"),
            ("g", "global variable"),
            ("l", "locked variable"),
            ("u", "undetermined type variable"),
        ];

        for (mod_str, doc) in modifiers {
            items.push(CompletionItem {
                label: mod_str.to_string(),
                kind: CompletionKind::Keyword,
                detail: Some("variable modifier".to_string()),
                documentation: Some(doc.to_string()),
                insert_text: Some(format!("{} ", mod_str)),
            });
        }

        // Add document-specific completions
        if let Some(doc) = self.documents.get(uri)
            && let Some(ast) = &doc.ast
        {
            // Add functions from this document
            for func in ast.functions() {
                items.push(CompletionItem {
                    label: func.name.clone(),
                    kind: CompletionKind::Function,
                    detail: Some(format!("{} function", func.color)),
                    documentation: None,
                    insert_text: Some(format!("{}()", func.name)),
                });
            }

            // Add structs from this document
            for struct_def in ast.structs() {
                items.push(CompletionItem {
                    label: struct_def.name.clone(),
                    kind: CompletionKind::Struct,
                    detail: Some("struct".to_string()),
                    documentation: None,
                    insert_text: Some(format!("{} {{}}", struct_def.name)),
                });
            }

            // Add interfaces from this document
            for iface in ast.interfaces() {
                items.push(CompletionItem {
                    label: iface.name.clone(),
                    kind: CompletionKind::Interface,
                    detail: Some("interface".to_string()),
                    documentation: None,
                    insert_text: None,
                });
            }
        }

        items
    }

    /// Get document symbols
    pub fn document_symbols(&self, uri: &str) -> Vec<DocumentSymbol> {
        let mut symbols = Vec::new();

        let doc = match self.documents.get(uri) {
            Some(d) => d,
            None => return symbols,
        };

        let ast = match &doc.ast {
            Some(a) => a,
            None => return symbols,
        };

        // Add functions
        for func in ast.functions() {
            symbols.push(DocumentSymbol {
                name: func.name.clone(),
                kind: SymbolKind::Function,
                range: self.span_to_range(&doc.content, &func.span),
                selection_range: self.span_to_range(&doc.content, &func.span),
                children: Vec::new(),
            });
        }

        // Add structs with their fields
        for struct_def in ast.structs() {
            let mut children = Vec::new();

            for field in &struct_def.fields {
                children.push(DocumentSymbol {
                    name: field.name.clone(),
                    kind: SymbolKind::Field,
                    range: self.span_to_range(&doc.content, &field.span),
                    selection_range: self.span_to_range(&doc.content, &field.span),
                    children: Vec::new(),
                });
            }

            symbols.push(DocumentSymbol {
                name: struct_def.name.clone(),
                kind: SymbolKind::Struct,
                range: self.span_to_range(&doc.content, &struct_def.span),
                selection_range: self.span_to_range(&doc.content, &struct_def.span),
                children,
            });
        }

        // Add interfaces
        for iface in ast.interfaces() {
            let mut children = Vec::new();

            for method in &iface.methods {
                children.push(DocumentSymbol {
                    name: method.name.clone(),
                    kind: SymbolKind::Method,
                    range: self.span_to_range(&doc.content, &method.span),
                    selection_range: self.span_to_range(&doc.content, &method.span),
                    children: Vec::new(),
                });
            }

            symbols.push(DocumentSymbol {
                name: iface.name.clone(),
                kind: SymbolKind::Interface,
                range: self.span_to_range(&doc.content, &iface.span),
                selection_range: self.span_to_range(&doc.content, &iface.span),
                children,
            });
        }

        symbols
    }

    /// Parse content and return AST if successful
    fn parse_content(&self, content: &str) -> Option<Program> {
        parse(content).ok()
    }

    /// Compute diagnostics for a document
    fn compute_diagnostics(&self, _uri: &str, content: &str, ast: &Option<Program>) -> Diagnostics {
        let mut diagnostics = Diagnostics::new();

        if !self.config.enable_diagnostics {
            return diagnostics;
        }

        // Try to parse and collect errors
        if ast.is_none()
            && let Err(e) = parse(content)
        {
            diagnostics.push(e.into());
        }

        // Additional semantic checks would go here
        // - Type checking
        // - Color violation detection
        // - Underscore prefix warnings
        // etc.

        diagnostics
    }

    /// Convert a position to byte offset
    fn position_to_offset(&self, content: &str, position: Position) -> Option<usize> {
        let mut offset = 0;
        let mut line = 0;

        for (i, ch) in content.char_indices() {
            if line == position.line as usize {
                for (col, (j, c)) in content[i..].char_indices().enumerate() {
                    if col == position.character as usize {
                        return Some(i + j);
                    }
                    if c == '\n' {
                        break;
                    }
                }
                return Some(i + (position.character as usize).min(content[i..].len()));
            }
            if ch == '\n' {
                line += 1;
            }
            offset = i + ch.len_utf8();
        }

        if line == position.line as usize {
            Some(offset)
        } else {
            None
        }
    }

    /// Convert a span to a range
    fn span_to_range(&self, content: &str, span: &Span) -> Range {
        let start = self.offset_to_position(content, span.start);
        let end = self.offset_to_position(content, span.end);
        Range { start, end }
    }

    /// Convert byte offset to position
    fn offset_to_position(&self, content: &str, offset: usize) -> Position {
        let mut line = 0;
        let mut col = 0;

        for (i, ch) in content.char_indices() {
            if i >= offset {
                break;
            }
            if ch == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }

        Position {
            line,
            character: col,
        }
    }
}

impl Default for Lsp {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lsp_creation() {
        let lsp = Lsp::new();
        assert!(lsp.documents.is_empty());
    }

    #[test]
    fn test_open_document() {
        let mut lsp = Lsp::new();
        lsp.open_document(
            "file:///test.nx".to_string(),
            "std main(): void {}".to_string(),
            1,
        );

        assert!(lsp.documents.contains_key("file:///test.nx"));
        let doc = lsp.documents.get("file:///test.nx").unwrap();
        assert!(doc.ast.is_some());
    }

    #[test]
    fn test_completions() {
        let lsp = Lsp::new();
        let completions = lsp.completions(
            "file:///test.nx",
            Position {
                line: 0,
                character: 0,
            },
        );

        // Should have at least keywords and builtins
        assert!(!completions.is_empty());
        assert!(completions.iter().any(|c| c.label == "std"));
        assert!(completions.iter().any(|c| c.label == "addi64"));
    }

    #[test]
    fn test_document_symbols() {
        let mut lsp = Lsp::new();
        lsp.open_document(
            "file:///test.nx".to_string(),
            r#"
                std main(): void {}
                struct Player {
                    i32 health = 100
                }
            "#
            .to_string(),
            1,
        );

        let symbols = lsp.document_symbols("file:///test.nx");
        assert!(symbols.iter().any(|s| s.name == "main"));
        assert!(symbols.iter().any(|s| s.name == "Player"));
    }
}

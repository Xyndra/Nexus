//! Core types used throughout the LSP server.

/// A position in a document, using 0-based line and character offsets.
#[derive(Debug, Clone, Copy, Default)]
pub struct Position {
    /// Line number (0-based)
    pub line: u32,
    /// Character offset within the line (0-based)
    pub character: u32,
}

/// A range in a document, defined by start and end positions.
#[derive(Debug, Clone, Default)]
pub struct Range {
    /// Start position (inclusive)
    pub start: Position,
    /// End position (exclusive)
    pub end: Position,
}

/// Information returned on hover.
#[derive(Debug, Clone)]
pub struct HoverInfo {
    /// The hover contents (markdown formatted)
    pub contents: String,
    /// Optional range that the hover applies to
    pub range: Option<Range>,
}

/// A completion item returned by the completion handler.
#[derive(Debug, Clone)]
pub struct CompletionItem {
    /// The label shown in the completion list
    pub label: String,
    /// The kind of completion item
    pub kind: CompletionKind,
    /// Additional detail shown next to the label
    pub detail: Option<String>,
    /// Documentation for the item
    pub documentation: Option<String>,
    /// Text to insert when completing
    pub insert_text: Option<String>,
}

/// The kind of a completion item.
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

/// A symbol in a document (function, struct, etc.).
#[derive(Debug, Clone)]
pub struct DocumentSymbol {
    /// The name of the symbol
    pub name: String,
    /// The kind of symbol
    pub kind: SymbolKind,
    /// The full range of the symbol
    pub range: Range,
    /// The range of the symbol's name
    pub selection_range: Range,
    /// Child symbols (e.g., methods in a struct)
    pub children: Vec<DocumentSymbol>,
}

/// The kind of a document symbol.
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

/// A location in a document.
#[derive(Debug, Clone)]
pub struct Location {
    /// The document URI
    pub uri: String,
    /// The range within the document
    pub range: Range,
}

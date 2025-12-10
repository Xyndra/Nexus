//! Language Server Protocol implementation for the Nexus programming language.
//!
//! This crate provides the LSP server that enables IDE features like
//! code completion, diagnostics, hover information, and more.

mod builtins;
mod completions;
mod diagnostics;
mod document_symbols;
mod goto_definition;
mod hover;
mod macro_expansion;
mod types;
mod utils;

use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

use nexus_core::Diagnostics;
use nexus_parser::{Program, parse};
use nexus_types::TypeRegistry;

// Re-export public types
pub use types::{
    CompletionItem, CompletionKind, DocumentSymbol, HoverInfo, Location, Position, Range,
    SymbolKind,
};

use diagnostics::{DiagnosticsConfig, FunctionContext, MacroContext};

/// Configuration for the LSP server.
#[derive(Debug, Clone)]
pub struct LspConfig {
    /// Whether to warn about underscore access (e.g., _variable)
    pub warn_underscore_access: bool,
    /// Whether diagnostics are enabled
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

/// A document open in the LSP server.
#[derive(Debug, Clone)]
pub struct Document {
    /// The document URI
    pub uri: String,
    /// The document content
    pub content: String,
    /// The parsed AST (if parsing succeeded)
    pub ast: Option<Program>,
    /// Document version
    pub version: i32,
}

/// The Nexus Language Server.
pub struct Lsp {
    /// Open documents (URI -> Document)
    documents: HashMap<String, Document>,
    /// Type registry shared across documents
    #[allow(dead_code)]
    type_registry: TypeRegistry,
    /// Diagnostics per document
    diagnostics: HashMap<String, Diagnostics>,
    /// Configuration
    config: LspConfig,
}

impl Lsp {
    /// Create a new LSP server instance.
    pub fn new() -> Self {
        Self {
            documents: HashMap::new(),
            type_registry: TypeRegistry::new(),
            diagnostics: HashMap::new(),
            config: LspConfig::default(),
        }
    }

    /// Create a new LSP server instance with custom configuration.
    pub fn with_config(config: LspConfig) -> Self {
        Self {
            documents: HashMap::new(),
            type_registry: TypeRegistry::new(),
            diagnostics: HashMap::new(),
            config,
        }
    }

    /// Load all .nx files from the same directory as the given URI
    fn load_module_files(&self, uri: &str) -> HashMap<String, (String, Option<Program>)> {
        let mut files = HashMap::new();

        // Extract directory path from URI
        let path = uri
            .strip_prefix("file:///")
            .or_else(|| uri.strip_prefix("file://"))
            .unwrap_or(uri);

        // Remove Windows extended path prefix if present
        let path = if let Some(path) = path.strip_prefix(r"\\?\") {
            path
        } else {
            path
        };

        let path_buf = PathBuf::from(path);
        let dir = match path_buf.parent() {
            Some(d) => d,
            None => return files,
        };

        // Read all .nx files from the directory
        let entries = match fs::read_dir(dir) {
            Ok(entries) => entries,
            Err(_) => return files,
        };

        for entry in entries.flatten() {
            let entry_path = entry.path();

            // Only process .nx files
            if entry_path.is_file()
                && entry_path.extension().is_some_and(|ext| ext == "nx")
                && let Ok(content) = fs::read_to_string(&entry_path)
            {
                let ast = parse(&content).ok();

                // Convert path to URI
                let mut path_str = entry_path.display().to_string();

                // Remove Windows extended path prefix if present
                if path_str.starts_with(r"\\?\") {
                    path_str = path_str[4..].to_string();
                }

                // Normalize path separators
                path_str = path_str.replace('\\', "/");

                // Create proper file URI
                let file_uri = format!("file:///{}", path_str);

                files.insert(file_uri, (content, ast));
            }
        }

        files
    }

    /// Open a document in the LSP server.
    pub fn open_document(&mut self, uri: &str, content: &str, version: i32) {
        let ast = parse(content).ok();
        let doc = Document {
            uri: uri.to_string(),
            content: content.to_string(),
            ast: ast.clone(),
            version,
        };

        self.documents.insert(uri.to_string(), doc);

        // Compute diagnostics with macro context from all documents
        self.recompute_diagnostics(uri);
    }

    /// Update a document in the LSP server.
    pub fn update_document(&mut self, uri: &str, content: &str, version: i32) {
        let ast = parse(content).ok();

        if let Some(doc) = self.documents.get_mut(uri) {
            doc.content = content.to_string();
            doc.ast = ast.clone();
            doc.version = version;
        }

        // Recompute diagnostics with macro context from all documents
        self.recompute_diagnostics(uri);
    }

    /// Recompute diagnostics for a document with access to all other documents for macro/function resolution
    fn recompute_diagnostics(&mut self, uri: &str) {
        let diag_config = DiagnosticsConfig {
            enabled: self.config.enable_diagnostics,
        };

        // Build documents map for macro and function context
        // Include all opened documents
        let mut documents_map: HashMap<String, (String, Option<Program>)> = self
            .documents
            .iter()
            .map(|(u, doc)| (u.clone(), (doc.content.clone(), doc.ast.clone())))
            .collect();

        // Also load all .nx files from the same directory (same module)
        let module_files = self.load_module_files(uri);
        for (file_uri, (content, ast)) in module_files {
            // Don't overwrite already opened documents
            documents_map.entry(file_uri).or_insert((content, ast));
        }

        // Build macro context from all documents
        let macro_context = MacroContext::from_documents(&documents_map);

        // Build function context from all documents
        let function_context = FunctionContext::from_documents(&documents_map);

        // Get the document content and ast
        if let Some(doc) = self.documents.get(uri) {
            let diags = diagnostics::compute_diagnostics_with_context(
                &doc.content,
                &doc.ast,
                &diag_config,
                Some(&macro_context),
                Some(&function_context),
                Some(uri),
            );
            self.diagnostics.insert(uri.to_string(), diags);
        }
    }

    /// Close a document in the LSP server.
    pub fn close_document(&mut self, uri: &str) {
        self.documents.remove(uri);
        self.diagnostics.remove(uri);
    }

    /// Get diagnostics for a document.
    pub fn get_diagnostics(&self, uri: &str) -> Option<&Diagnostics> {
        self.diagnostics.get(uri)
    }

    /// Get hover information at a position.
    pub fn hover(&self, uri: &str, position: Position) -> Option<HoverInfo> {
        let doc = self.documents.get(uri)?;
        let ast = doc.ast.as_ref()?;
        let offset = utils::position_to_offset(&doc.content, position)?;

        // Build a map of documents for cross-file resolution
        // Include all opened documents
        let mut documents: HashMap<String, (String, Option<Program>)> = self
            .documents
            .iter()
            .map(|(uri, doc)| (uri.clone(), (doc.content.clone(), doc.ast.clone())))
            .collect();

        // Also load all .nx files from the same directory (same module)
        let module_files = self.load_module_files(uri);
        for (file_uri, (content, ast)) in module_files {
            // Don't overwrite already opened documents
            documents.entry(file_uri).or_insert((content, ast));
        }

        hover::find_hover(&doc.content, ast, offset, &documents)
    }

    /// Go to definition at a position.
    pub fn goto_definition(&self, uri: &str, position: Position) -> Option<Location> {
        let doc = self.documents.get(uri)?;
        let ast = doc.ast.as_ref()?;
        let offset = utils::position_to_offset(&doc.content, position)?;

        // Build a map of documents for cross-file resolution
        // Include all opened documents
        let mut documents: HashMap<String, (String, Option<Program>)> = self
            .documents
            .iter()
            .map(|(uri, doc)| (uri.clone(), (doc.content.clone(), doc.ast.clone())))
            .collect();

        // Also load all .nx files from the same directory (same module)
        let module_files = self.load_module_files(uri);
        for (file_uri, (content, ast)) in module_files {
            // Don't overwrite already opened documents
            documents.entry(file_uri).or_insert((content, ast));
        }

        goto_definition::find_definition(uri, &doc.content, ast, offset, &documents)
    }

    /// Get completions at a position.
    pub fn completions(&self, uri: &str, _position: Position) -> Vec<CompletionItem> {
        let ast = self.documents.get(uri).and_then(|doc| doc.ast.as_ref());

        completions::get_completions(ast)
    }

    /// Get document symbols.
    pub fn document_symbols(&self, uri: &str) -> Vec<DocumentSymbol> {
        let doc = match self.documents.get(uri) {
            Some(d) => d,
            None => return Vec::new(),
        };

        let ast = match &doc.ast {
            Some(a) => a,
            None => return Vec::new(),
        };

        document_symbols::get_document_symbols(&doc.content, ast)
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
        lsp.open_document("file:///test.nx", "std main(): void {}", 1);

        assert!(lsp.documents.contains_key("file:///test.nx"));
        let doc = lsp.documents.get("file:///test.nx").unwrap();
        assert!(doc.ast.is_some());
    }

    #[test]
    fn test_completions() {
        let mut lsp = Lsp::new();
        lsp.open_document("file:///test.nx", "std main(): void {}", 1);

        let completions = lsp.completions(
            "file:///test.nx",
            Position {
                line: 0,
                character: 18,
            },
        );

        // Should have keywords and builtins
        assert!(!completions.is_empty());
        assert!(completions.iter().any(|c| c.label == "if"));
        assert!(completions.iter().any(|c| c.label == "len"));
    }

    #[test]
    fn test_document_symbols() {
        let mut lsp = Lsp::new();
        lsp.open_document(
            "file:///test.nx",
            "std main(): void {} std helper(): i64 { return 42 }",
            1,
        );

        let symbols = lsp.document_symbols("file:///test.nx");

        assert!(symbols.len() >= 2);
        assert!(symbols.iter().any(|s| s.name == "main"));
        assert!(symbols.iter().any(|s| s.name == "helper"));
    }

    #[test]
    fn test_return_with_value_in_subscope_error() {
        let mut lsp = Lsp::new();
        let content = r#"
            std main(): i64 {
                subscope loop {
                    return 42
                    goto loop
                }
            }
        "#;
        lsp.open_document("file:///test.nx", content, 1);

        let diagnostics = lsp.get_diagnostics("file:///test.nx");
        assert!(diagnostics.is_some());
        assert!(!diagnostics.unwrap().is_empty());
    }

    #[test]
    fn test_return_without_value_in_subscope_ok() {
        let mut lsp = Lsp::new();
        let content = r#"
            std main(): void {
                subscope loop {
                    m x = 1
                    goto loop
                }
            }
        "#;
        lsp.open_document("file:///test.nx", content, 1);

        let diagnostics = lsp.get_diagnostics("file:///test.nx");
        assert!(diagnostics.is_some());
        // Subscope without return with value should be ok
        assert!(diagnostics.unwrap().is_empty());
    }

    #[test]
    fn test_goto_definition_non_recursive() {
        let mut lsp = Lsp::new();
        // Simple case: helper function defined before main
        let content = "std helper(): i64 { return 42 } std main(): void { m x = helper() }";
        lsp.open_document("file:///test.nx", content, 1);

        // Position on "helper()" call in main - the call starts at position 57
        // Content: "std helper(): i64 { return 42 } std main(): void { m x = helper() }"
        //                                                                   ^ position 57
        let loc = lsp.goto_definition(
            "file:///test.nx",
            Position {
                line: 0,
                character: 57,
            },
        );
        // Just check that we found something - exact position testing is fragile
        assert!(loc.is_some(), "Expected to find helper function definition");
    }

    #[test]
    fn test_goto_definition_recursive() {
        // Test that goto definition works for recursive calls
        // This is a simpler version that just verifies the functionality
        let mut lsp = Lsp::new();
        let content = "std fact(i64 n): i64 { return muli64(n, fact(subi64(n, 1))) }";
        lsp.open_document("file:///test.nx", content, 1);

        // Position on recursive "fact" call - around position 45
        let loc = lsp.goto_definition(
            "file:///test.nx",
            Position {
                line: 0,
                character: 45,
            },
        );
        // Just check that we found something
        assert!(
            loc.is_some(),
            "Expected to find recursive function definition"
        );
    }

    #[test]
    fn test_hover_with_doc_comment() {
        let mut lsp = Lsp::new();
        let content = r#"
            // This function greets someone
            // It takes a name parameter
            std greet(string name): void {
                m x = 1
            }
        "#;
        lsp.open_document("file:///test.nx", content, 1);

        // Hover on function name "greet" (line 3, around position 16)
        let hover = lsp.hover(
            "file:///test.nx",
            Position {
                line: 3,
                character: 16,
            },
        );
        assert!(
            hover.is_some(),
            "Expected hover info for function with doc comment"
        );
        let hover = hover.unwrap();
        assert!(hover.contents.contains("greet"));
        assert!(hover.contents.contains("name"));
    }

    #[test]
    fn test_hover_on_function_call() {
        let mut lsp = Lsp::new();
        let content = r#"
            // Adds two numbers
            std add(i64 a, i64 b): i64 {
                return addi64(a, b)
            }

            std main(): void {
                m x = add(1, 2)
            }
        "#;
        lsp.open_document("file:///test.nx", content, 1);

        // Hover on add() call in main (line 7, around position 22)
        let hover = lsp.hover(
            "file:///test.nx",
            Position {
                line: 7,
                character: 22,
            },
        );
        assert!(hover.is_some(), "Expected hover info for function call");
        let hover = hover.unwrap();
        assert!(hover.contents.contains("add"));
        assert!(hover.contents.contains("a"));
        assert!(hover.contents.contains("i64"));
    }

    #[test]
    fn test_hover_on_builtin() {
        let mut lsp = Lsp::new();
        let content = r#"
            std main(): void {
                m x = len([1, 2, 3])
            }
        "#;
        lsp.open_document("file:///test.nx", content, 1);

        // Hover on len() call (line 2, around position 22)
        let hover = lsp.hover(
            "file:///test.nx",
            Position {
                line: 2,
                character: 22,
            },
        );
        assert!(hover.is_some(), "Expected hover info for builtin function");
        let hover = hover.unwrap();
        assert!(hover.contents.contains("len"));
        assert!(hover.contents.contains("collection"));
        assert!(hover.contents.contains("i64"));
    }

    #[test]
    fn test_goto_definition_function_defined_after() {
        let mut lsp = Lsp::new();
        let content = r#"
            std main(): void {
                m x = helper()
            }

            std helper(): i64 {
                return 1
            }
        "#;
        lsp.open_document("file:///test.nx", content, 1);

        // Position on "helper()" call in main (line 2, around position 22)
        let loc = lsp.goto_definition(
            "file:///test.nx",
            Position {
                line: 2,
                character: 22,
            },
        );
        assert!(loc.is_some());
        let loc = loc.unwrap();
        assert_eq!(loc.uri, "file:///test.nx");
    }
}

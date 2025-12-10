//! Nexus Language Server Protocol implementation.
//!
//! This binary provides a full LSP server for the Nexus programming language,
//! enabling IDE features like syntax highlighting, code completion,
//! hover information, diagnostics, and more.

use std::error::Error;

use crossbeam_channel::Sender;
use log::{debug, error, info, warn};
use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionOptions, CompletionParams, CompletionResponse,
    Diagnostic as LspDiagnostic, DiagnosticSeverity as LspDiagnosticSeverity,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, DocumentSymbol as LspDocumentSymbol, DocumentSymbolParams,
    DocumentSymbolResponse, GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents,
    HoverParams, HoverProviderCapability, InitializeParams, MarkupContent, MarkupKind, OneOf,
    Position as LspPosition, PublishDiagnosticsParams, Range as LspRange, SemanticTokenModifier,
    SemanticTokenType, SemanticTokens, SemanticTokensFullOptions, SemanticTokensLegend,
    SemanticTokensOptions, SemanticTokensParams, SemanticTokensResult,
    SemanticTokensServerCapabilities, ServerCapabilities, SymbolKind as LspSymbolKind,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url, WorkDoneProgressOptions,
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, DidSaveTextDocument,
        Notification as _, PublishDiagnostics,
    },
    request::{
        Completion, DocumentSymbolRequest, GotoDefinition, HoverRequest, Request as _,
        SemanticTokensFullRequest,
    },
};
use nexus_lsp_server::{CompletionKind, DocumentSymbol, Lsp, Position, SymbolKind};
use serde_json::Value;

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info")).init();

    info!("Starting Nexus Language Server");

    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".into(), ":".into(), "@".into(), "$".into()]),
            resolve_provider: Some(false),
            work_done_progress_options: WorkDoneProgressOptions::default(),
            all_commit_characters: None,
            completion_item: None,
        }),
        definition_provider: Some(OneOf::Left(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: vec![
                        SemanticTokenType::KEYWORD,
                        SemanticTokenType::TYPE,
                        SemanticTokenType::FUNCTION,
                        SemanticTokenType::VARIABLE,
                        SemanticTokenType::PARAMETER,
                        SemanticTokenType::PROPERTY,
                        SemanticTokenType::STRING,
                        SemanticTokenType::NUMBER,
                        SemanticTokenType::COMMENT,
                        SemanticTokenType::OPERATOR,
                        SemanticTokenType::MACRO,
                        SemanticTokenType::STRUCT,
                        SemanticTokenType::INTERFACE,
                        SemanticTokenType::NAMESPACE,
                    ],
                    token_modifiers: vec![
                        SemanticTokenModifier::DECLARATION,
                        SemanticTokenModifier::DEFINITION,
                        SemanticTokenModifier::READONLY,
                        SemanticTokenModifier::STATIC,
                        SemanticTokenModifier::DEPRECATED,
                        SemanticTokenModifier::MODIFICATION,
                        SemanticTokenModifier::DOCUMENTATION,
                        SemanticTokenModifier::DEFAULT_LIBRARY,
                    ],
                },
                full: Some(SemanticTokensFullOptions::Bool(true)),
                range: Some(false),
                work_done_progress_options: WorkDoneProgressOptions::default(),
            },
        )),
        ..Default::default()
    })?;

    let init_params = connection.initialize(server_capabilities)?;
    let init_params: InitializeParams = serde_json::from_value(init_params)?;

    info!("Nexus LSP initialized");

    main_loop(connection, init_params)?;

    io_threads.join()?;

    info!("Nexus LSP shutting down");
    Ok(())
}

fn main_loop(
    connection: Connection,
    _init_params: InitializeParams,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let mut lsp = Lsp::new();

    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }

                let result = handle_request(&mut lsp, &req);

                let response = match result {
                    Ok(value) => Response::new_ok(req.id, value),
                    Err(e) => {
                        error!("Error handling request: {}", e);
                        Response::new_err(
                            req.id,
                            lsp_server::ErrorCode::InternalError as i32,
                            e.to_string(),
                        )
                    }
                };

                connection.sender.send(Message::Response(response))?;
            }
            Message::Response(resp) => {
                debug!("Received response: {:?}", resp);
            }
            Message::Notification(not) => {
                handle_notification(&mut lsp, &not, &connection.sender)?;
            }
        }
    }

    Ok(())
}

fn handle_request(lsp: &mut Lsp, req: &Request) -> Result<Value, Box<dyn Error + Sync + Send>> {
    match req.method.as_str() {
        HoverRequest::METHOD => {
            let params: HoverParams = serde_json::from_value(req.params.clone())?;
            let result = handle_hover(lsp, params);
            Ok(serde_json::to_value(result)?)
        }
        Completion::METHOD => {
            let params: CompletionParams = serde_json::from_value(req.params.clone())?;
            let result = handle_completion(lsp, params);
            Ok(serde_json::to_value(result)?)
        }
        GotoDefinition::METHOD => {
            let params: GotoDefinitionParams = serde_json::from_value(req.params.clone())?;
            let result = handle_goto_definition(lsp, params);
            Ok(serde_json::to_value(result)?)
        }
        DocumentSymbolRequest::METHOD => {
            let params: DocumentSymbolParams = serde_json::from_value(req.params.clone())?;
            let result = handle_document_symbols(lsp, params);
            Ok(serde_json::to_value(result)?)
        }
        SemanticTokensFullRequest::METHOD => {
            let params: SemanticTokensParams = serde_json::from_value(req.params.clone())?;
            let result = handle_semantic_tokens(lsp, params);
            Ok(serde_json::to_value(result)?)
        }
        _ => {
            warn!("Unhandled request method: {}", req.method);
            Ok(Value::Null)
        }
    }
}

fn handle_notification(
    lsp: &mut Lsp,
    not: &Notification,
    sender: &Sender<Message>,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    match not.method.as_str() {
        DidOpenTextDocument::METHOD => {
            let params: DidOpenTextDocumentParams = serde_json::from_value(not.params.clone())?;
            let uri = params.text_document.uri.to_string();
            let content = params.text_document.text;
            let version = params.text_document.version;

            lsp.open_document(&uri, &content, version);
            publish_diagnostics(lsp, &uri, sender)?;
        }
        DidChangeTextDocument::METHOD => {
            let params: DidChangeTextDocumentParams = serde_json::from_value(not.params.clone())?;
            let uri = params.text_document.uri.to_string();

            if let Some(change) = params.content_changes.into_iter().last() {
                lsp.update_document(&uri, &change.text, params.text_document.version);
                publish_diagnostics(lsp, &uri, sender)?;
            }
        }
        DidSaveTextDocument::METHOD => {
            let params: DidSaveTextDocumentParams = serde_json::from_value(not.params.clone())?;
            let uri = params.text_document.uri.to_string();
            publish_diagnostics(lsp, &uri, sender)?;
        }
        DidCloseTextDocument::METHOD => {
            let params: DidCloseTextDocumentParams = serde_json::from_value(not.params.clone())?;
            let uri = params.text_document.uri.to_string();
            lsp.close_document(&uri);
        }
        _ => {
            debug!("Unhandled notification: {}", not.method);
        }
    }

    Ok(())
}

fn handle_hover(lsp: &mut Lsp, params: HoverParams) -> Option<Hover> {
    let uri = params
        .text_document_position_params
        .text_document
        .uri
        .to_string();
    let position = params.text_document_position_params.position;

    let hover_info = lsp.hover(
        &uri,
        Position {
            line: position.line,
            character: position.character,
        },
    )?;

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: hover_info.contents,
        }),
        range: hover_info.range.map(|r| LspRange {
            start: LspPosition {
                line: r.start.line,
                character: r.start.character,
            },
            end: LspPosition {
                line: r.end.line,
                character: r.end.character,
            },
        }),
    })
}

fn handle_completion(lsp: &Lsp, params: CompletionParams) -> Option<CompletionResponse> {
    let uri = params.text_document_position.text_document.uri.to_string();
    let position = params.text_document_position.position;

    let completions = lsp.completions(
        &uri,
        Position {
            line: position.line,
            character: position.character,
        },
    );

    let items: Vec<CompletionItem> = completions
        .into_iter()
        .map(|item| CompletionItem {
            label: item.label,
            kind: Some(match item.kind {
                CompletionKind::Function => CompletionItemKind::FUNCTION,
                CompletionKind::Variable => CompletionItemKind::VARIABLE,
                CompletionKind::Struct => CompletionItemKind::STRUCT,
                CompletionKind::Interface => CompletionItemKind::INTERFACE,
                CompletionKind::Field => CompletionItemKind::FIELD,
                CompletionKind::Keyword => CompletionItemKind::KEYWORD,
                CompletionKind::Builtin => CompletionItemKind::FUNCTION,
                CompletionKind::Macro => CompletionItemKind::FUNCTION,
            }),
            detail: item.detail,
            documentation: item.documentation.map(|d| {
                lsp_types::Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: d,
                })
            }),
            insert_text: item.insert_text,
            ..Default::default()
        })
        .collect();

    Some(CompletionResponse::Array(items))
}

fn handle_goto_definition(
    lsp: &mut Lsp,
    params: GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let uri = params
        .text_document_position_params
        .text_document
        .uri
        .to_string();
    let position = params.text_document_position_params.position;

    let location = lsp.goto_definition(
        &uri,
        nexus_lsp_server::Position {
            line: position.line,
            character: position.character,
        },
    )?;

    Some(GotoDefinitionResponse::Scalar(lsp_types::Location {
        uri: Url::parse(&location.uri).ok()?,
        range: LspRange {
            start: LspPosition {
                line: location.range.start.line,
                character: location.range.start.character,
            },
            end: LspPosition {
                line: location.range.end.line,
                character: location.range.end.character,
            },
        },
    }))
}

fn handle_document_symbols(
    lsp: &Lsp,
    params: DocumentSymbolParams,
) -> Option<DocumentSymbolResponse> {
    let uri = params.text_document.uri.to_string();

    let symbols = lsp.document_symbols(&uri);

    fn convert_symbol(sym: DocumentSymbol) -> LspDocumentSymbol {
        #[allow(deprecated)]
        LspDocumentSymbol {
            name: sym.name,
            kind: match sym.kind {
                SymbolKind::Function => LspSymbolKind::FUNCTION,
                SymbolKind::Method => LspSymbolKind::METHOD,
                SymbolKind::Struct => LspSymbolKind::STRUCT,
                SymbolKind::Interface => LspSymbolKind::INTERFACE,
                SymbolKind::Field => LspSymbolKind::FIELD,
                SymbolKind::Variable => LspSymbolKind::VARIABLE,
                SymbolKind::Macro => LspSymbolKind::FUNCTION,
            },
            range: LspRange {
                start: LspPosition {
                    line: sym.range.start.line,
                    character: sym.range.start.character,
                },
                end: LspPosition {
                    line: sym.range.end.line,
                    character: sym.range.end.character,
                },
            },
            selection_range: LspRange {
                start: LspPosition {
                    line: sym.selection_range.start.line,
                    character: sym.selection_range.start.character,
                },
                end: LspPosition {
                    line: sym.selection_range.end.line,
                    character: sym.selection_range.end.character,
                },
            },
            detail: None,
            tags: None,
            deprecated: None,
            children: if sym.children.is_empty() {
                None
            } else {
                Some(sym.children.into_iter().map(convert_symbol).collect())
            },
        }
    }

    let document_symbols: Vec<LspDocumentSymbol> =
        symbols.into_iter().map(convert_symbol).collect();

    Some(DocumentSymbolResponse::Nested(document_symbols))
}

fn handle_semantic_tokens(
    _lsp: &Lsp,
    params: SemanticTokensParams,
) -> Option<SemanticTokensResult> {
    // TODO: Implement semantic tokens using nexus-lexer
    debug!("Semantic tokens requested for {:?}", params.text_document);
    Some(SemanticTokensResult::Tokens(SemanticTokens {
        result_id: None,
        data: vec![],
    }))
}

fn publish_diagnostics(
    lsp: &Lsp,
    uri: &str,
    sender: &Sender<Message>,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let empty_diagnostics = nexus_core::Diagnostics::new();
    let diagnostics_result = lsp.get_diagnostics(uri).unwrap_or(&empty_diagnostics);

    let diagnostics: Vec<LspDiagnostic> = diagnostics_result
        .iter()
        .filter_map(|diag| {
            let span = diag.span.as_ref()?;
            let severity = match diag.severity {
                nexus_core::DiagnosticSeverity::Error => LspDiagnosticSeverity::ERROR,
                nexus_core::DiagnosticSeverity::Warning => LspDiagnosticSeverity::WARNING,
                nexus_core::DiagnosticSeverity::Info => LspDiagnosticSeverity::INFORMATION,
                nexus_core::DiagnosticSeverity::Hint => LspDiagnosticSeverity::HINT,
            };
            // Span has line/column for start position, and byte offsets for start/end
            // We use the line/column from the span (1-based, convert to 0-based)
            // For the end, we estimate based on span length
            let start_line = span.line.saturating_sub(1);
            let start_col = span.column.saturating_sub(1);
            let span_len = span.end.saturating_sub(span.start);
            Some(LspDiagnostic {
                range: LspRange {
                    start: LspPosition {
                        line: start_line,
                        character: start_col,
                    },
                    end: LspPosition {
                        line: start_line,
                        character: start_col + span_len as u32,
                    },
                },
                severity: Some(severity),
                message: diag.message.clone(),
                source: Some("nexus".to_string()),
                ..Default::default()
            })
        })
        .collect();

    let uri = Url::parse(uri).unwrap_or_else(|_| Url::parse("file:///unknown").unwrap());

    let params = PublishDiagnosticsParams {
        uri,
        diagnostics,
        version: None,
    };

    let notification = Notification::new(PublishDiagnostics::METHOD.to_string(), params);
    sender.send(Message::Notification(notification))?;

    Ok(())
}

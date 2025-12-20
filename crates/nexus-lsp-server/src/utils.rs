//! Utility functions for LSP operations.
//!
//! This module contains helper functions for converting between
//! different position and span representations.

use crate::types::{Position, Range};
use nexus_core::Span;

/// Convert a Position (line/character) to a byte offset in the content.
pub fn position_to_offset(content: &str, position: Position) -> Option<usize> {
    let mut current_line = 0u32;
    let mut current_offset = 0usize;

    for (i, c) in content.char_indices() {
        if current_line == position.line {
            for (char_count, (j, ch)) in content[i..].char_indices().enumerate() {
                if char_count as u32 == position.character {
                    return Some(i + j);
                }
                if ch == '\n' {
                    // Position is past end of line
                    return Some(i + j);
                }
            }
            // Position is at end of content
            return Some(content.len());
        }
        if c == '\n' {
            current_line += 1;
        }
        current_offset = i + c.len_utf8();
    }

    // If we're looking for a position past the content
    if current_line == position.line && position.character == 0 {
        return Some(current_offset);
    }

    None
}

/// Convert a byte offset to a Position (line/character).
pub fn offset_to_position(content: &str, offset: usize) -> Position {
    let mut line = 0u32;
    let mut line_start = 0usize;

    for (i, c) in content.char_indices() {
        if i >= offset {
            return Position {
                line,
                character: (offset - line_start) as u32,
            };
        }
        if c == '\n' {
            line += 1;
            line_start = i + 1;
        }
    }

    // Offset is at or past end of content
    Position {
        line,
        character: (offset.saturating_sub(line_start)) as u32,
    }
}

/// Convert a Span to a Range.
pub fn span_to_range(content: &str, span: &Span) -> Range {
    Range {
        start: offset_to_position(content, span.start),
        end: offset_to_position(content, span.end),
    }
}

/// Extract doc comments from source content above a given span.
///
/// This looks for consecutive `//` comment lines immediately preceding
/// the span (allowing blank lines between comments).
pub fn extract_doc_comment(content: &str, span: &Span) -> String {
    let before_span = &content[..span.start];
    let lines: Vec<&str> = before_span.lines().collect();

    let mut comment_lines = Vec::new();
    let mut found_non_blank = false;

    // Walk backwards from the end
    for line in lines.iter().rev() {
        let trimmed = line.trim();

        if trimmed.starts_with("//") {
            // Extract comment content (remove // prefix)
            let comment_content = trimmed.strip_prefix("//").unwrap_or("").trim();
            comment_lines.push(comment_content.to_string());
            found_non_blank = true;
        } else if trimmed.is_empty() {
            // Allow blank lines within comments, but not before finding any comment
            if found_non_blank {
                // Keep blank line in comments
                comment_lines.push(String::new());
            }
            // Continue looking
        } else {
            // Non-comment, non-blank line - stop
            break;
        }
    }

    // Reverse to get correct order and trim trailing empty lines
    comment_lines.reverse();

    // Remove leading/trailing empty lines
    while comment_lines.first().is_some_and(|s| s.is_empty()) {
        comment_lines.remove(0);
    }
    while comment_lines.last().is_some_and(|s| s.is_empty()) {
        comment_lines.pop();
    }

    comment_lines.join("\n")
}

/// Format a type expression for display.
pub fn format_type_expr(ty: &nexus_parser::TypeExpr) -> String {
    match ty {
        nexus_parser::TypeExpr::Named { name, .. } => name.clone(),
        nexus_parser::TypeExpr::Array { element, .. } => {
            format!("[{}]", format_type_expr(element))
        }
        nexus_parser::TypeExpr::Unknown { variants, .. } => {
            let variants_str: Vec<String> = variants.iter().map(format_type_expr).collect();
            format!("unknown<{}>", variants_str.join(", "))
        }
        nexus_parser::TypeExpr::Function {
            params,
            return_type,
            ..
        } => {
            let params_str: Vec<String> = params.iter().map(format_type_expr).collect();
            format!(
                "({}) -> {}",
                params_str.join(", "),
                format_type_expr(return_type)
            )
        }
        nexus_parser::TypeExpr::Void { .. } => "void".to_string(),
    }
}

/// Format a function parameter for display, including name and type.
pub fn format_parameter(param: &nexus_parser::Parameter) -> String {
    if param.mutable {
        format!("m {}: {}", param.name, format_type_expr(&param.ty))
    } else {
        format!("{}: {}", param.name, format_type_expr(&param.ty))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position_to_offset_first_line() {
        let content = "hello world";
        let pos = Position {
            line: 0,
            character: 6,
        };
        assert_eq!(position_to_offset(content, pos), Some(6));
    }

    #[test]
    fn test_position_to_offset_second_line() {
        let content = "hello\nworld";
        let pos = Position {
            line: 1,
            character: 2,
        };
        assert_eq!(position_to_offset(content, pos), Some(8));
    }

    #[test]
    fn test_offset_to_position_first_line() {
        let content = "hello world";
        let pos = offset_to_position(content, 6);
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 6);
    }

    #[test]
    fn test_offset_to_position_second_line() {
        let content = "hello\nworld";
        let pos = offset_to_position(content, 8);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 2);
    }

    #[test]
    fn test_extract_doc_comment() {
        // Content: "// This is a doc comment\n// Second line\nstd main(): void { m x = 1 }"
        // Positions: 0-23 is first comment (24 chars), 24 is \n, 25-38 is second comment (14 chars), 39 is \n, 40+ is function
        let content = "// This is a doc comment\n// Second line\nstd main(): void { m x = 1 }";
        let span = Span {
            start: 40,
            end: 68,
            line: 3,
            column: 1,
        };
        let comment = extract_doc_comment(content, &span);
        assert!(
            comment.contains("This is a doc comment"),
            "Expected comment to contain 'This is a doc comment', got: {}",
            comment
        );
        assert!(
            comment.contains("Second line"),
            "Expected comment to contain 'Second line', got: {}",
            comment
        );
    }

    #[test]
    fn test_extract_doc_comment_with_blank_lines() {
        let content = "// First\n//\n// Third\nstd main() {}";
        // "std main() {}" starts at position 21 (after "// Third\n")
        let span = Span {
            start: 21,
            end: 34,
            line: 4,
            column: 1,
        };
        let comment = extract_doc_comment(content, &span);
        assert!(comment.contains("First"));
        assert!(comment.contains("Third"));
    }
}

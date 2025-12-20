//! Nexus Language Lexer
//!
//! Tokenizes Nexus source code into a stream of tokens for parsing.
//! Note: There are no operator tokens. All operations are function calls like
//! `addi64(a, b)`, `not(x)`, `eqi64(a, b)`, etc.

mod token;

pub use token::*;

use nexus_core::{NexusError, Span};

/// The lexer for the Nexus programming language.
#[derive(Debug)]
pub struct Lexer<'src> {
    source: &'src str,
    chars: std::iter::Peekable<std::str::CharIndices<'src>>,
    current_pos: usize,
    line: u32,
    column: u32,
    tokens: Vec<Token>,
}

impl<'src> Lexer<'src> {
    /// Create a new lexer for the given source code.
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
            current_pos: 0,
            line: 1,
            column: 1,
            tokens: Vec::new(),
        }
    }

    /// Tokenize the entire source and return all tokens.
    pub fn tokenize(mut self) -> Result<Vec<Token>, NexusError> {
        while !self.is_at_end() {
            self.skip_whitespace_and_comments()?;
            if !self.is_at_end() {
                let token = self.scan_token()?;
                self.tokens.push(token);
            }
        }

        self.tokens.push(Token {
            kind: TokenKind::Eof,
            span: Span::new(self.current_pos, self.current_pos, self.line, self.column),
            lexeme: String::new(),
        });

        Ok(self.tokens)
    }

    fn is_at_end(&mut self) -> bool {
        self.chars.peek().is_none()
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        let result = self.chars.next();
        if let Some((pos, ch)) = result {
            self.current_pos = pos + ch.len_utf8();
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
        result
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, ch)| *ch)
    }

    fn peek_next(&self) -> Option<char> {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().map(|(_, ch)| ch)
    }

    fn skip_whitespace_and_comments(&mut self) -> Result<(), NexusError> {
        while let Some(ch) = self.peek() {
            match ch {
                ' ' | '\t' | '\r' => {
                    self.advance();
                }
                '\n' => {
                    // Newlines are significant (same as semicolons) - emit a token
                    return Ok(());
                }
                '/' => {
                    if self.peek_next() == Some('/') {
                        // Single-line comment
                        while let Some(ch) = self.peek() {
                            if ch == '\n' {
                                break;
                            }
                            self.advance();
                        }
                    } else if self.peek_next() == Some('*') {
                        // Multi-line comment
                        self.advance(); // consume '/'
                        self.advance(); // consume '*'
                        let mut depth = 1;
                        while depth > 0 {
                            match self.peek() {
                                Some('*') if self.peek_next() == Some('/') => {
                                    self.advance();
                                    self.advance();
                                    depth -= 1;
                                }
                                Some('/') if self.peek_next() == Some('*') => {
                                    self.advance();
                                    self.advance();
                                    depth += 1;
                                }
                                Some(_) => {
                                    self.advance();
                                }
                                None => {
                                    return Err(NexusError::LexerError {
                                        message: "Unterminated multi-line comment".to_string(),
                                        line: self.line,
                                        column: self.column,
                                    });
                                }
                            }
                        }
                    } else {
                        return Ok(());
                    }
                }
                _ => return Ok(()),
            }
        }
        Ok(())
    }

    fn scan_token(&mut self) -> Result<Token, NexusError> {
        let start_pos = self.current_pos;
        let start_line = self.line;
        let start_column = self.column;

        let (_, ch) = self.advance().unwrap();

        let kind = match ch {
            // Newline (statement terminator, same as semicolon)
            '\n' => TokenKind::Newline,

            // Delimiters and punctuation
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            '[' => TokenKind::LeftBracket,
            ']' => TokenKind::RightBracket,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            ';' => TokenKind::Semicolon,
            ':' => TokenKind::Colon,
            '@' => TokenKind::At,
            '$' => TokenKind::Dollar,
            '=' => TokenKind::Equal,
            '<' => TokenKind::Less,
            '>' => TokenKind::Greater,

            // Arrow for pattern matching cases
            '-' => {
                if self.peek() == Some('>') {
                    self.advance();
                    TokenKind::Arrow
                } else {
                    return Err(NexusError::LexerError {
                        message: "Unexpected '-'. Did you mean '->' for pattern matching, or a function like 'subi64(a, b)'?".to_string(),
                        line: start_line,
                        column: start_column,
                    });
                }
            }

            // String literal
            '"' => {
                return self.scan_string(start_pos, start_line, start_column);
            }

            // Character literal
            '\'' => return self.scan_char(start_pos, start_line, start_column),

            // Number literal
            c if c.is_ascii_digit() => {
                return self.scan_number(start_pos, start_line, start_column);
            }

            // Identifier or keyword
            c if c.is_alphabetic() || c == '_' => {
                return self.scan_identifier(start_pos, start_line, start_column);
            }

            _ => {
                return Err(NexusError::LexerError {
                    message: format!(
                        "Unexpected character: '{}'. Note: Nexus uses function calls for operations (e.g., addi64(a, b) instead of a + b)",
                        ch
                    ),
                    line: start_line,
                    column: start_column,
                });
            }
        };

        let lexeme = self.source[start_pos..self.current_pos].to_string();
        Ok(Token {
            kind,
            span: Span::new(start_pos, self.current_pos, start_line, start_column),
            lexeme,
        })
    }

    fn scan_string(
        &mut self,
        start_pos: usize,
        start_line: u32,
        start_column: u32,
    ) -> Result<Token, NexusError> {
        let mut value = String::new();

        while let Some(ch) = self.peek() {
            if ch == '"' {
                break;
            }
            if ch == '\\' {
                self.advance();
                match self.peek() {
                    Some('n') => {
                        value.push('\n');
                        self.advance();
                    }
                    Some('r') => {
                        value.push('\r');
                        self.advance();
                    }
                    Some('t') => {
                        value.push('\t');
                        self.advance();
                    }
                    Some('\\') => {
                        value.push('\\');
                        self.advance();
                    }
                    Some('"') => {
                        value.push('"');
                        self.advance();
                    }
                    Some('0') => {
                        value.push('\0');
                        self.advance();
                    }
                    Some(c) => {
                        return Err(NexusError::LexerError {
                            message: format!("Invalid escape sequence: \\{}", c),
                            line: self.line,
                            column: self.column,
                        });
                    }
                    None => {
                        return Err(NexusError::LexerError {
                            message: "Unterminated escape sequence".to_string(),
                            line: self.line,
                            column: self.column,
                        });
                    }
                }
            } else {
                value.push(ch);
                self.advance();
            }
        }

        if self.peek() != Some('"') {
            return Err(NexusError::LexerError {
                message: "Unterminated string literal".to_string(),
                line: start_line,
                column: start_column,
            });
        }
        self.advance(); // consume closing quote

        let lexeme = self.source[start_pos..self.current_pos].to_string();
        Ok(Token {
            kind: TokenKind::StringLiteral(value),
            span: Span::new(start_pos, self.current_pos, start_line, start_column),
            lexeme,
        })
    }

    fn scan_char(
        &mut self,
        start_pos: usize,
        start_line: u32,
        start_column: u32,
    ) -> Result<Token, NexusError> {
        let ch = if self.peek() == Some('\\') {
            self.advance();
            match self.peek() {
                Some('n') => {
                    self.advance();
                    '\n'
                }
                Some('r') => {
                    self.advance();
                    '\r'
                }
                Some('t') => {
                    self.advance();
                    '\t'
                }
                Some('\\') => {
                    self.advance();
                    '\\'
                }
                Some('\'') => {
                    self.advance();
                    '\''
                }
                Some('0') => {
                    self.advance();
                    '\0'
                }
                Some(c) => {
                    return Err(NexusError::LexerError {
                        message: format!("Invalid escape sequence: \\{}", c),
                        line: self.line,
                        column: self.column,
                    });
                }
                None => {
                    return Err(NexusError::LexerError {
                        message: "Unterminated escape sequence".to_string(),
                        line: self.line,
                        column: self.column,
                    });
                }
            }
        } else {
            match self.advance() {
                Some((_, c)) => c,
                None => {
                    return Err(NexusError::LexerError {
                        message: "Unterminated character literal".to_string(),
                        line: start_line,
                        column: start_column,
                    });
                }
            }
        };

        if self.peek() != Some('\'') {
            return Err(NexusError::LexerError {
                message: "Unterminated character literal".to_string(),
                line: start_line,
                column: start_column,
            });
        }
        self.advance(); // consume closing quote

        let lexeme = self.source[start_pos..self.current_pos].to_string();
        Ok(Token {
            kind: TokenKind::CharLiteral(ch),
            span: Span::new(start_pos, self.current_pos, start_line, start_column),
            lexeme,
        })
    }

    fn scan_number(
        &mut self,
        start_pos: usize,
        start_line: u32,
        start_column: u32,
    ) -> Result<Token, NexusError> {
        // Consume integer part
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() || ch == '_' {
                self.advance();
            } else {
                break;
            }
        }

        // Check for float
        let is_float =
            if self.peek() == Some('.') && self.peek_next().is_some_and(|c| c.is_ascii_digit()) {
                self.advance(); // consume '.'
                while let Some(ch) = self.peek() {
                    if ch.is_ascii_digit() || ch == '_' {
                        self.advance();
                    } else {
                        break;
                    }
                }
                true
            } else {
                false
            };

        // Check for exponent
        let has_exponent = if let Some('e' | 'E') = self.peek() {
            self.advance();
            if let Some('+' | '-') = self.peek() {
                self.advance();
            }
            while let Some(ch) = self.peek() {
                if ch.is_ascii_digit() {
                    self.advance();
                } else {
                    break;
                }
            }
            true
        } else {
            false
        };

        let lexeme = self.source[start_pos..self.current_pos].to_string();
        let clean_lexeme: String = lexeme.chars().filter(|&c| c != '_').collect();

        let kind = if is_float || has_exponent {
            match clean_lexeme.parse::<f64>() {
                Ok(value) => TokenKind::FloatLiteral(value),
                Err(_) => {
                    return Err(NexusError::LexerError {
                        message: format!("Invalid float literal: {}", lexeme),
                        line: start_line,
                        column: start_column,
                    });
                }
            }
        } else {
            match clean_lexeme.parse::<i64>() {
                Ok(value) => TokenKind::IntLiteral(value),
                Err(_) => {
                    return Err(NexusError::LexerError {
                        message: format!("Invalid integer literal: {}", lexeme),
                        line: start_line,
                        column: start_column,
                    });
                }
            }
        };

        Ok(Token {
            kind,
            span: Span::new(start_pos, self.current_pos, start_line, start_column),
            lexeme,
        })
    }

    fn scan_identifier(
        &mut self,
        start_pos: usize,
        start_line: u32,
        start_column: u32,
    ) -> Result<Token, NexusError> {
        while let Some(ch) = self.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let lexeme = self.source[start_pos..self.current_pos].to_string();
        let kind = match lexeme.as_str() {
            // Function coloring keywords
            "std" => TokenKind::Std,
            "compat" => TokenKind::Compat,
            "plat" => TokenKind::Plat,

            // Type keywords
            "struct" => TokenKind::Struct,
            "interface" => TokenKind::Interface,
            "impl" => TokenKind::Impl,
            "unknown" => TokenKind::Unknown,

            // Control flow
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "match" => TokenKind::Match,
            "return" => TokenKind::Return,
            "defer" => TokenKind::Defer,
            "subscope" => TokenKind::Subscope,
            "goto" => TokenKind::Goto,

            // Primitive types
            "void" => TokenKind::Void,
            "bool" => TokenKind::Bool,
            "i8" => TokenKind::I8,
            "i16" => TokenKind::I16,
            "i32" => TokenKind::I32,
            "i64" => TokenKind::I64,
            "u8" => TokenKind::U8,
            "u16" => TokenKind::U16,
            "u32" => TokenKind::U32,
            "u64" => TokenKind::U64,
            "f32" => TokenKind::F32,
            "f64" => TokenKind::F64,
            "rune" => TokenKind::Rune,
            "macro" => TokenKind::Macro,
            "dyn" => TokenKind::Dyn,

            // Boolean literals
            "true" => TokenKind::True,
            "false" => TokenKind::False,

            // Import keywords
            "use" => TokenKind::Use,
            "from" => TokenKind::From,

            // Other
            "None" => TokenKind::None,
            "Error" => TokenKind::Error,

            // Default: identifier (could be variable modifier like 'm', 'mh', etc.)
            _ => TokenKind::Identifier(lexeme.clone()),
        };

        Ok(Token {
            kind,
            span: Span::new(start_pos, self.current_pos, start_line, start_column),
            lexeme,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let source = "mh arr = 123";
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();

        assert!(matches!(tokens[0].kind, TokenKind::Identifier(_)));
        assert!(matches!(tokens[1].kind, TokenKind::Identifier(_)));
        assert!(matches!(tokens[2].kind, TokenKind::Equal));
        assert!(matches!(tokens[3].kind, TokenKind::IntLiteral(123)));
    }

    #[test]
    fn test_function_coloring() {
        let source = "std main(): void {}";
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();

        assert!(matches!(tokens[0].kind, TokenKind::Std));
    }

    #[test]
    fn test_newline_as_terminator() {
        let source = "m x = 1\nm y = 2";
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();

        // Should have newline tokens
        let newlines: Vec<_> = tokens
            .iter()
            .filter(|t| matches!(t.kind, TokenKind::Newline))
            .collect();
        assert!(!newlines.is_empty());
    }

    #[test]
    fn test_function_call_style() {
        let source = "addi64(x, y)";
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();

        assert!(matches!(&tokens[0].kind, TokenKind::Identifier(s) if s == "addi64"));
        assert!(matches!(tokens[1].kind, TokenKind::LeftParen));
        assert!(matches!(&tokens[2].kind, TokenKind::Identifier(s) if s == "x"));
        assert!(matches!(tokens[3].kind, TokenKind::Comma));
        assert!(matches!(&tokens[4].kind, TokenKind::Identifier(s) if s == "y"));
        assert!(matches!(tokens[5].kind, TokenKind::RightParen));
    }

    #[test]
    fn test_arrow_for_pattern_matching() {
        let source = "10 -> result";
        let lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();

        assert!(matches!(tokens[0].kind, TokenKind::IntLiteral(10)));
        assert!(matches!(tokens[1].kind, TokenKind::Arrow));
        assert!(matches!(&tokens[2].kind, TokenKind::Identifier(s) if s == "result"));
    }
}

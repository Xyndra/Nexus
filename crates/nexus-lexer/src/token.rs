//! Token definitions for the Nexus lexer.

use nexus_core::Span;

/// A token produced by the lexer.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// The kind of token
    pub kind: TokenKind,
    /// Source location
    pub span: Span,
    /// The original lexeme (raw text from source)
    pub lexeme: String,
}

impl Token {
    /// Check if this token is a statement terminator (newline or semicolon)
    pub fn is_terminator(&self) -> bool {
        matches!(self.kind, TokenKind::Newline | TokenKind::Semicolon)
    }

    /// Check if this token is a variable modifier (m, l, h, u, g, or combinations)
    pub fn is_var_modifier(&self) -> bool {
        if let TokenKind::Identifier(ref s) = self.kind {
            s.chars().all(|c| matches!(c, 'm' | 'l' | 'h' | 'u' | 'g'))
        } else {
            false
        }
    }
}

/// The kind of token.
///
/// Note: There are no operator tokens. All operations are function calls like
/// `addi64(a, b)`, `not(x)`, `eqi64(a, b)`, etc.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Literals
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    CharLiteral(char),

    // Identifiers and keywords
    Identifier(String),

    // Function coloring keywords
    Std,
    Compat,
    Plat,

    // Type keywords
    Struct,
    Interface,
    Impl,
    Unknown,

    // Control flow keywords
    If,
    Else,
    Match,
    Return,
    Defer,
    Subscope,
    Goto,
    Exit,

    // Import keywords
    Use,
    From,

    // Primitive type keywords
    Void,
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Rune,
    Macro,
    Dyn,

    // Boolean literals
    True,
    False,

    // Special values
    None,
    Error,

    // Delimiters and punctuation
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Comma,        // ,
    Dot,          // .
    Semicolon,    // ;
    Colon,        // :
    At,           // @
    Dollar,       // $
    Equal,        // = (assignment only)
    Arrow,        // -> (for pattern matching cases)
    Less,         // < (for generics like unknown<A, B>)
    Greater,      // > (for generics like unknown<A, B>)

    // Statement terminators
    Newline,

    // End of file
    Eof,
}

impl TokenKind {
    /// Get the name of this token kind for error messages
    pub fn name(&self) -> &'static str {
        match self {
            TokenKind::IntLiteral(_) => "integer literal",
            TokenKind::FloatLiteral(_) => "float literal",
            TokenKind::StringLiteral(_) => "string literal",
            TokenKind::CharLiteral(_) => "character literal",
            TokenKind::Identifier(_) => "identifier",
            TokenKind::Std => "'std'",
            TokenKind::Compat => "'compat'",
            TokenKind::Plat => "'plat'",
            TokenKind::Struct => "'struct'",
            TokenKind::Interface => "'interface'",
            TokenKind::Impl => "'impl'",
            TokenKind::Unknown => "'unknown'",
            TokenKind::If => "'if'",
            TokenKind::Else => "'else'",
            TokenKind::Match => "'match'",
            TokenKind::Return => "'return'",
            TokenKind::Defer => "'defer'",
            TokenKind::Subscope => "'subscope'",
            TokenKind::Goto => "'goto'",
            TokenKind::Exit => "'exit'",
            TokenKind::Use => "'use'",
            TokenKind::From => "'from'",
            TokenKind::Void => "'void'",
            TokenKind::Bool => "'bool'",
            TokenKind::I8 => "'i8'",
            TokenKind::I16 => "'i16'",
            TokenKind::I32 => "'i32'",
            TokenKind::I64 => "'i64'",
            TokenKind::U8 => "'u8'",
            TokenKind::U16 => "'u16'",
            TokenKind::U32 => "'u32'",
            TokenKind::U64 => "'u64'",
            TokenKind::F32 => "'f32'",
            TokenKind::F64 => "'f64'",
            TokenKind::Rune => "'rune'",
            TokenKind::Macro => "'macro'",
            TokenKind::Dyn => "'dyn'",
            TokenKind::True => "'true'",
            TokenKind::False => "'false'",
            TokenKind::None => "'None'",
            TokenKind::Error => "'Error'",
            TokenKind::LeftParen => "'('",
            TokenKind::RightParen => "')'",
            TokenKind::LeftBrace => "'{'",
            TokenKind::RightBrace => "'}'",
            TokenKind::LeftBracket => "'['",
            TokenKind::RightBracket => "']'",
            TokenKind::Comma => "','",
            TokenKind::Dot => "'.'",
            TokenKind::Semicolon => "';'",
            TokenKind::Colon => "':'",
            TokenKind::At => "'@'",
            TokenKind::Dollar => "'$'",
            TokenKind::Equal => "'='",
            TokenKind::Arrow => "'->'",
            TokenKind::Less => "'<'",
            TokenKind::Greater => "'>'",
            TokenKind::Newline => "newline",
            TokenKind::Eof => "end of file",
        }
    }

    /// Check if this is a primitive type keyword
    pub fn is_primitive_type(&self) -> bool {
        matches!(
            self,
            TokenKind::Void
                | TokenKind::Bool
                | TokenKind::I8
                | TokenKind::I16
                | TokenKind::I32
                | TokenKind::I64
                | TokenKind::U8
                | TokenKind::U16
                | TokenKind::U32
                | TokenKind::U64
                | TokenKind::F32
                | TokenKind::F64
                | TokenKind::Rune
        )
    }

    /// Check if this is a function color keyword
    pub fn is_function_color(&self) -> bool {
        matches!(self, TokenKind::Std | TokenKind::Compat | TokenKind::Plat)
    }

    /// Check if this is a keyword (not an identifier)
    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            TokenKind::Std
                | TokenKind::Compat
                | TokenKind::Plat
                | TokenKind::Struct
                | TokenKind::Interface
                | TokenKind::Impl
                | TokenKind::Unknown
                | TokenKind::If
                | TokenKind::Else
                | TokenKind::Match
                | TokenKind::Return
                | TokenKind::Defer
                | TokenKind::Subscope
                | TokenKind::Goto
                | TokenKind::Exit
                | TokenKind::Use
                | TokenKind::From
                | TokenKind::Void
                | TokenKind::Bool
                | TokenKind::I8
                | TokenKind::I16
                | TokenKind::I32
                | TokenKind::I64
                | TokenKind::U8
                | TokenKind::U16
                | TokenKind::U32
                | TokenKind::U64
                | TokenKind::F32
                | TokenKind::F64
                | TokenKind::Rune
                | TokenKind::Macro
                | TokenKind::Dyn
                | TokenKind::True
                | TokenKind::False
                | TokenKind::None
                | TokenKind::Error
        )
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::IntLiteral(v) => write!(f, "{}", v),
            TokenKind::FloatLiteral(v) => write!(f, "{}", v),
            TokenKind::StringLiteral(s) => write!(f, "\"{}\"", s),
            TokenKind::CharLiteral(c) => write!(f, "'{}'", c),
            TokenKind::Identifier(s) => write!(f, "{}", s),
            _ => write!(f, "{}", self.name()),
        }
    }
}

//! Core types shared across all Nexus crates.
//!
//! This crate provides fundamental types like source spans, error types,
//! and common utilities used throughout the Nexus language implementation.

mod error;
mod span;

pub use error::*;
pub use span::*;

/// Source file identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceId(pub u32);

impl SourceId {
    pub const REPL: SourceId = SourceId(0);
    pub const BUFFER: SourceId = SourceId(1);
}

/// A source file with its content
#[derive(Debug, Clone)]
pub struct SourceFile {
    pub id: SourceId,
    pub name: String,
    pub content: String,
    /// Byte offsets of line starts for fast line lookup
    line_starts: Vec<usize>,
}

impl SourceFile {
    pub fn new(id: SourceId, name: impl Into<String>, content: impl Into<String>) -> Self {
        let content = content.into();
        let line_starts = std::iter::once(0)
            .chain(content.match_indices('\n').map(|(i, _)| i + 1))
            .collect();

        Self {
            id,
            name: name.into(),
            content,
            line_starts,
        }
    }

    /// Get line and column (1-based) from byte offset
    pub fn line_col(&self, offset: usize) -> (usize, usize) {
        let line = self
            .line_starts
            .partition_point(|&start| start <= offset)
            .saturating_sub(1);
        let col = offset - self.line_starts.get(line).copied().unwrap_or(0);
        (line + 1, col + 1)
    }

    /// Get the content of a specific line (0-based)
    pub fn line_content(&self, line: usize) -> Option<&str> {
        let start = *self.line_starts.get(line)?;
        let end = self
            .line_starts
            .get(line + 1)
            .map(|&e| e.saturating_sub(1))
            .unwrap_or(self.content.len());
        Some(&self.content[start..end])
    }

    /// Get total number of lines
    pub fn line_count(&self) -> usize {
        self.line_starts.len()
    }
}

/// Variable modifier flags
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct VarModifiers {
    /// `c` - const (immutable, read-only, incompatible with `l`)
    pub const_: bool,
    /// `m` - mutable
    pub mutable: bool,
    /// `l` - locked (read-write lock for thread safety)
    pub locked: bool,
    /// `h` - heap allocated
    pub heap: bool,
    /// `u` - undetermined type (type + pointer)
    pub undetermined: bool,
    /// `g` - global (persists beyond scope)
    pub global: bool,
}

impl VarModifiers {
    pub fn parse(s: &str) -> Option<Self> {
        let mut modifiers = Self::default();
        for ch in s.chars() {
            match ch {
                'c' => modifiers.const_ = true,
                'm' => modifiers.mutable = true,
                'l' => modifiers.locked = true,
                'h' => modifiers.heap = true,
                'u' => modifiers.undetermined = true,
                'g' => modifiers.global = true,
                _ => return None,
            }
        }
        Some(modifiers)
    }

    pub fn is_empty(&self) -> bool {
        !self.const_
            && !self.mutable
            && !self.locked
            && !self.heap
            && !self.undetermined
            && !self.global
    }

    /// Check if this modifier combination is valid
    pub fn is_valid(&self) -> bool {
        // const is incompatible with mutable and locked
        if self.const_ && (self.mutable || self.locked) {
            return false;
        }
        true
    }
}

impl std::fmt::Display for VarModifiers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.const_ {
            write!(f, "c")?;
        }
        if self.mutable {
            write!(f, "m")?;
        }
        if self.locked {
            write!(f, "l")?;
        }
        if self.heap {
            write!(f, "h")?;
        }
        if self.undetermined {
            write!(f, "u")?;
        }
        if self.global {
            write!(f, "g")?;
        }
        Ok(())
    }
}

/// Function color indicating capability level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FunctionColor {
    /// Pure logic only, no side effects
    Std,
    /// Uses base system compatibility APIs (cross-platform)
    Compat,
    /// Uses platform-specific code
    Plat,
}

impl FunctionColor {
    /// Check if this color can call functions of another color
    pub fn can_call(&self, other: FunctionColor) -> bool {
        match (self, other) {
            (FunctionColor::Std, FunctionColor::Std) => true,
            (FunctionColor::Std, _) => false,
            (FunctionColor::Compat, FunctionColor::Plat) => false,
            (FunctionColor::Compat, _) => true,
            (FunctionColor::Plat, _) => true,
        }
    }
}

impl std::fmt::Display for FunctionColor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionColor::Std => write!(f, "std"),
            FunctionColor::Compat => write!(f, "compat"),
            FunctionColor::Plat => write!(f, "plat"),
        }
    }
}

impl std::str::FromStr for FunctionColor {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "std" => Ok(FunctionColor::Std),
            "compat" => Ok(FunctionColor::Compat),
            "plat" => Ok(FunctionColor::Plat),
            _ => Err(()),
        }
    }
}

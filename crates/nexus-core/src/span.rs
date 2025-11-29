//! Source span types for tracking locations in source code.

/// A span represents a range in source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    /// Byte offset of the start of the span
    pub start: usize,
    /// Byte offset of the end of the span (exclusive)
    pub end: usize,
    /// Line number (1-based)
    pub line: u32,
    /// Column number (1-based)
    pub column: u32,
}

impl Span {
    /// Create a new span.
    pub fn new(start: usize, end: usize, line: u32, column: u32) -> Self {
        Self {
            start,
            end,
            line,
            column,
        }
    }

    /// Create a dummy span (used for generated code or builtins).
    pub fn dummy() -> Self {
        Self {
            start: 0,
            end: 0,
            line: 0,
            column: 0,
        }
    }

    /// Check if this is a dummy span.
    pub fn is_dummy(&self) -> bool {
        self.line == 0 && self.column == 0
    }

    /// Get the length of the span in bytes.
    pub fn len(&self) -> usize {
        self.end.saturating_sub(self.start)
    }

    /// Check if the span is empty.
    pub fn is_empty(&self) -> bool {
        self.start >= self.end
    }

    /// Merge two spans into one that covers both.
    pub fn merge(&self, other: &Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            // Use the earlier position for line/column
            line: if self.start <= other.start {
                self.line
            } else {
                other.line
            },
            column: if self.start <= other.start {
                self.column
            } else {
                other.column
            },
        }
    }

    /// Create a span that starts at this span and ends at another.
    pub fn to(&self, other: &Span) -> Span {
        Span {
            start: self.start,
            end: other.end,
            line: self.line,
            column: self.column,
        }
    }

    /// Check if this span contains a byte offset.
    pub fn contains(&self, offset: usize) -> bool {
        offset >= self.start && offset < self.end
    }

    /// Check if this span overlaps with another.
    pub fn overlaps(&self, other: &Span) -> bool {
        self.start < other.end && other.start < self.end
    }
}

impl Default for Span {
    fn default() -> Self {
        Self::dummy()
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// A value with an associated span.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    /// Create a new spanned value.
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }

    /// Map the inner value while preserving the span.
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U> {
        Spanned {
            value: f(self.value),
            span: self.span,
        }
    }

    /// Get a reference to the inner value.
    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            value: &self.value,
            span: self.span,
        }
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> std::ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_merge() {
        let span1 = Span::new(0, 5, 1, 1);
        let span2 = Span::new(10, 15, 2, 1);
        let merged = span1.merge(&span2);

        assert_eq!(merged.start, 0);
        assert_eq!(merged.end, 15);
        assert_eq!(merged.line, 1);
    }

    #[test]
    fn test_span_contains() {
        let span = Span::new(5, 10, 1, 1);

        assert!(!span.contains(4));
        assert!(span.contains(5));
        assert!(span.contains(7));
        assert!(span.contains(9));
        assert!(!span.contains(10));
    }

    #[test]
    fn test_span_overlaps() {
        let span1 = Span::new(0, 10, 1, 1);
        let span2 = Span::new(5, 15, 1, 6);
        let span3 = Span::new(10, 20, 2, 1);

        assert!(span1.overlaps(&span2));
        assert!(!span1.overlaps(&span3));
        assert!(span2.overlaps(&span3));
    }
}

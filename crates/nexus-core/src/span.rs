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

    /// Return the byte start and end indices of the line containing this span's start.
    ///
    /// The returned bounds are [start, end) (end is exclusive) and will never include a trailing
    /// newline character. Returns `None` if the source is empty.
    pub fn line_bounds(&self, source: &str) -> Option<(usize, usize)> {
        if source.is_empty() {
            return None;
        }

        // Clamp the span start to the source length to avoid out-of-bounds.
        let start = self.start.min(source.len());

        // Find the previous newline (if any) before start; line starts after it.
        let line_start = match source[..start].rfind('\n') {
            Some(idx) => idx + 1,
            None => 0,
        };

        // Find the next newline (if any) after start; line ends at that newline or the end of source.
        let line_end = match source[start..].find('\n') {
            Some(idx) => start + idx,
            None => source.len(),
        };

        Some((line_start, line_end))
    }

    /// Return the text of the whole line containing this span's start (without trailing newline).
    pub fn line_text<'a>(&self, source: &'a str) -> Option<&'a str> {
        self.line_bounds(source)
            .map(|(s, e)| source[s..e].trim_end_matches('\r'))
    }

    /// Build a marker line (spaces + carets) that highlights this span within its source line.
    ///
    /// The marker aligns roughly with character positions in the line; it clamps the span to the
    /// containing line if the span extends across lines.
    pub fn marker(&self, source: &str) -> Option<String> {
        let (line_start, line_end) = self.line_bounds(source)?;
        // Clamp absolute span to the line bounds.
        let sabs = self.start.max(line_start).min(line_end);
        let eabs = self.end.max(sabs).min(line_end);

        let line = &source[line_start..line_end];

        // Compute character offsets within the line for start and end.
        let start_char_idx = line[..(sabs - line_start)].chars().count();
        let end_char_idx = line[..(eabs - line_start)].chars().count();

        let mut width = end_char_idx.saturating_sub(start_char_idx);
        if width == 0 {
            // Ensure at least one caret for empty spans (e.g. insertion points).
            width = 1;
        }

        let mut marker = String::new();
        marker.push_str(&" ".repeat(start_char_idx));
        marker.push_str(&"^".repeat(width));
        Some(marker)
    }

    /// Format a short snippet showing the file/line/column and a caret marker under the line.
    ///
    /// Example output:
    ///
    ///   m x = foo + 1
    ///         ^^^
    ///
    /// This is intended to be used by CLI error reporting code that has access to the source
    /// text and optionally the filename.
    pub fn format_snippet(&self, source: &str) -> Option<String> {
        let line = self.line_text(source)?;
        let marker = self.marker(source)?;
        let mut buf = String::new();

        buf.push_str(&format!("{}\n{}\n", line, marker));
        Some(buf)
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

    #[test]
    fn test_line_text_and_marker() {
        let src = "first line\n  foo bar\nthird line\n";
        let start = src.find("foo").unwrap();
        let end = start + "foo".len();
        let span = Span::new(start, end, 2, 3);

        // The full line containing "foo"
        assert_eq!(span.line_text(src).unwrap(), "  foo bar");

        // The marker should have two leading spaces (for the two spaces before "foo")
        // followed by three carets for "foo".
        assert_eq!(span.marker(src).unwrap(), "  ^^^");

        let snippet = span.format_snippet(src).unwrap();
        assert!(snippet.contains("  foo bar"));
        assert!(snippet.contains("^^^"));
    }

    #[test]
    fn test_zero_length_span_marker() {
        let src = "a\nb\n";
        let pos = src.find('b').unwrap();
        let span = Span::new(pos, pos, 2, 1);
        let marker = span.marker(src).unwrap();
        // Zero-length span should still render a single caret.
        assert_eq!(marker, "^");
    }

    #[test]
    fn test_multibyte_characters() {
        // Ensure that character-counting (not byte counting) is used for marker alignment.
        let src = "λλλ\n  λλ abc\n";
        let pos = src.find("abc").unwrap();
        let span = Span::new(pos, pos + 3, 2, 6);
        let line = span.line_text(src).unwrap();
        assert_eq!(line, "  λλ abc");

        let marker = span.marker(src).unwrap();
        // Marker should end with three carets for "abc".
        assert!(marker.ends_with("^^^"));
    }
}

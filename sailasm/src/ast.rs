//! Contains structures to represent the abstract syntax tree.

use std::cmp::{Ord, Ordering, PartialOrd};
use std::fmt::{Debug, Display, Formatter};

/// Represents a line or column number.
pub type LocationNumber = std::num::NonZeroUsize;

pub(crate) const LOCATION_NUMBER_START: LocationNumber = unsafe {
    // Safety: Constant is guaranteed to be non-zero.
    LocationNumber::new_unchecked(1)
};

/// Represents a pair of line and column numbers.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Location {
    pub line: LocationNumber,
    pub column: LocationNumber,
}

impl Location {
    pub fn new(line: LocationNumber, column: LocationNumber) -> Self {
        Self { line, column }
    }
}

impl Ord for Location {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.line.cmp(&other.line) {
            Ordering::Equal => self.column.cmp(&other.column),
            c => c,
        }
    }
}

impl PartialOrd for Location {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "({}, {})", self.line, self.column)
    }
}

impl From<Location> for (usize, usize) {
    fn from(location: Location) -> (usize, usize) {
        (location.line.get(), location.column.get())
    }
}

/// An inclusive range of locations in the input.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct LocationRange {
    start: Location,
    end: Location,
}

impl LocationRange {
    pub fn new(start: Location, end: Location) -> Self {
        if start > end {
            panic!("end location {} must not come before start location {}", end, start);
        }

        Self { start, end }
    }

    #[inline]
    pub fn start(&self) -> &Location {
        &self.start
    }

    #[inline]
    pub fn end(&self) -> &Location {
        &self.end
    }
}

impl From<&Location> for LocationRange {
    fn from(location: &Location) -> Self {
        Self::new(location.clone(), location.clone())
    }
}

impl From<Location> for LocationRange {
    fn from(location: Location) -> Self {
        Self::new(location.clone(), location)
    }
}

impl From<&LocationRange> for std::ops::RangeInclusive<Location> {
    fn from(range: &LocationRange) -> Self {
        Self::new(range.start.clone(), range.end.clone())
    }
}

impl Ord for LocationRange {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.start.cmp(&other.start) {
            Ordering::Equal => self.end.cmp(&other.end),
            c => c,
        }
    }
}

impl PartialOrd for LocationRange {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Display for LocationRange {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Display::fmt(&self.start, f)?;
        if self.end > self.start {
            f.write_str(" - ")?;
            Display::fmt(&self.end, f)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Located<N> {
    location: LocationRange,
    node: N,
}

impl<N> Located<N> {
    pub fn new(node: N, start: Location, end: Location) -> Self {
        Self {
            node,
            location: LocationRange::new(start, end),
        }
    }

    #[inline]
    pub fn location(&self) -> &LocationRange {
        &self.location
    }

    #[inline]
    pub fn node(&self) -> &N {
        &self.node
    }
}

pub type Symbol<'s> = Located<&'s sailar::Id>;

#[derive(Debug, thiserror::Error)]
#[error("\"\\{sequence}\" is not a valid escape sequence")]
pub struct InvalidEscapeSequenceError {
    byte_offset: usize,
    sequence: Box<str>,
}

impl InvalidEscapeSequenceError {
    /// The byte offset into the original string where the invalid escape sequence is, where 0 is the first byte in the string
    /// literal.
    #[inline]
    pub fn byte_offset(&self) -> usize {
        self.byte_offset
    }

    /// The invalid escape sequence, without the leading backslash.
    #[inline]
    pub fn sequence(&self) -> &str {
        &self.sequence
    }

    pub(crate) fn take_sequence(self) -> Box<str> {
        self.sequence
    }
}

#[derive(Clone, PartialEq)]
pub struct LiteralString<'s> {
    original_contents: &'s str,
    actual_contents: Option<Box<str>>,
}

impl<'s> LiteralString<'s> {
    /// Creates a string literal from its contents, which can pontentially contain escape sequences.
    pub fn with_escape_sequences(contents: &'s str, buffer: &mut String) -> Result<Self, InvalidEscapeSequenceError> {
        buffer.clear();

        let mut characters = contents.char_indices();
        while let Some((byte_offset, c)) = characters.next() {
            if c == '\\' {
                if buffer.is_empty() {
                    buffer.reserve(contents.len());
                    buffer.push_str(&contents[0..byte_offset]);
                }

                match characters.next() {
                    Some((_, 't')) => buffer.push('\t'),
                    Some((_, 'n')) => buffer.push('\n'),
                    Some((_, 'r')) => buffer.push('\r'),
                    Some((_, '"')) => buffer.push('"'),
                    Some((_, '\'')) => buffer.push('\''),
                    Some((_, '\\')) => buffer.push('\\'),
                    _ => {
                        return Err(InvalidEscapeSequenceError {
                            byte_offset: byte_offset + 1,
                            sequence: Box::from(characters.as_str()),
                        })
                    }
                }
            } else if !buffer.is_empty() {
                buffer.push(c);
            }
        }

        Ok(Self {
            original_contents: contents,
            actual_contents: if !buffer.is_empty() {
                Some(buffer.clone().into_boxed_str())
            } else {
                None
            },
        })
    }

    pub fn as_str(&self) -> &str {
        self.actual_contents
            .as_ref()
            .map(|contents| contents.as_ref())
            .unwrap_or(&self.original_contents)
    }
}

impl<'s> TryFrom<&'s str> for LiteralString<'s> {
    type Error = InvalidEscapeSequenceError;

    fn try_from(literal: &'s str) -> Result<Self, Self::Error> {
        LiteralString::with_escape_sequences(literal, &mut String::default())
    }
}

impl Debug for LiteralString<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Debug::fmt(self.original_contents, f)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FormatVersionKind {
    Major,
    Minor,
}

impl Display for FormatVersionKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_str(match self {
            Self::Major => "major",
            Self::Minor => "minor",
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Directive<'s> {
    /// ```text
    /// .array
    /// ```
    /// Indicates that records that follow the next record after this directive should be merged into a single array record.
    ///
    /// # Example
    ///
    /// ```text
    /// .array ; Merges the following 3 identifier records into one record.
    /// .identifier "abc"
    /// .identifier "def"
    /// .identifier "testing"
    /// ```
    Array,
    /// ```text
    /// .format major 0
    /// .format minor 12
    /// ```
    /// Sets the major or minor format version of the module.
    Format(FormatVersionKind, u8),
    /// ```text
    /// .identifier "no symbol" ; Referred to by numeric index
    /// .identifier @my_symbol "with symbol" ; Referred to by numeric index or by name.
    /// ```
    /// Defines record containing a reusable identifier string.
    Identifier(Option<Symbol<'s>>, LiteralString<'s>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_literal_string_with_escape_sequences_is_valid() {
        assert_eq!(
            "test\nfor\tthings\r\nthat work\\",
            LiteralString::try_from("test\\nfor\\tthings\\r\\nthat work\\\\")
                .unwrap()
                .as_str()
        );
    }
}

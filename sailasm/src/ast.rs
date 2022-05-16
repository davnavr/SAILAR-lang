//! Contains structures to represent the abstract syntax tree.

use std::cmp::{Ord, Ordering, PartialOrd};
use std::fmt::{Display, Formatter};

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
}

pub type Symbol<'s> = Located<&'s sailar::Id>;

#[derive(Clone, Debug, PartialEq)]
#[repr(transparent)]
pub struct LiteralString(Box<str>);

impl LiteralString {
    /// Creates a string literal from its contents, which can pontentially contain escape sequences.
    pub fn with_escape_sequences(contents: &str, buffer: &mut String) -> Self {
        buffer.clear();
        buffer.reserve(contents.len());
        for c in contents.chars() {
            if c == '\\' {
                todo!("escape sequences are not yet supported");
            }

            buffer.push(c);
        }

        Self(buffer.clone().into_boxed_str())
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
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
    Identifier(Option<Symbol<'s>>, LiteralString),
}

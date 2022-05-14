//! Contains structures to represent the abstract syntax tree.

/// Represents a line or column number.
pub type LocationNumber = std::num::NonZeroUsize;

pub(crate) const LOCATION_NUMBER_START: LocationNumber = unsafe {
    // Safety: Constant is guaranteed to be non-zero.
    LocationNumber::new_unchecked(1)
};

/// Represents a pair of line and column numbers.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Location {
    pub line: LocationNumber,
    pub column: LocationNumber,
}

impl Location {
    pub fn new(line: LocationNumber, column: LocationNumber) -> Self {
        Self { line, column }
    }
}

impl From<Location> for (usize, usize) {
    fn from(location: Location) -> (usize, usize) {
        (location.line.get(), location.column.get())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Located<N> {
    location: std::ops::Range<Location>,
    node: N,
}

impl<N> Located<N> {
    pub fn new(node: N, start: Location, end: Location) -> Self {
        Self {
            node,
            location: std::ops::Range { start, end },
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

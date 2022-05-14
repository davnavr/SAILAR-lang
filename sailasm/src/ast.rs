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

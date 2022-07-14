//! Code for interacting with SAILAR identifiers.

use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;

/// Represents a SAILAR identifier, which is a UTF-8 string that cannot be empty or contain any `null` bytes.
///
/// [`Id`] is to [`Identifier`] as [`str`] is to [`String`].
#[derive(Clone, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Identifier(String);

/// Borrowed form of a SAILAR identifier.
#[derive(Eq, Hash, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Id(str);

macro_rules! format_impls {
    ($implementor: ident) => {
        impl Debug for $implementor {
            fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
                Debug::fmt(&self.0, f)
            }
        }

        impl Display for $implementor {
            fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
                Display::fmt(&self.0, f)
            }
        }
    };
}

format_impls!(Identifier);
format_impls!(Id);

macro_rules! deref_impl {
    ($implementor: ident, $target: ty) => {
        impl Deref for $implementor {
            type Target = $target;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
    };
}

deref_impl!(Identifier, String);
deref_impl!(Id, str);

impl Id {
    #[inline]
    pub fn to_identifier(&self) -> Identifier {
        Identifier(self.0.to_string())
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Creates a reference to an identfier from a string, without any validation checks.
    ///
    /// # Safety
    ///
    /// Callers should ensure that the string does not contain any interior null bytes and must not be empty.
    pub unsafe fn from_str_unchecked(identifier: &str) -> &Id {
        // Safety: Representation of Id allows safe transmute here.
        std::mem::transmute::<&str, &Id>(identifier)
    }

    /// Attempts to create a reference to an identifier string.
    ///
    /// # Errors
    ///
    /// If the string is empty or contains a `NUL` character, then an error is returned.
    pub fn try_from_str(identifier: &str) -> Result<&Id, InvalidError> {
        if identifier.is_empty() {
            Err(InvalidError::Empty)
        } else if identifier.chars().any(|c| c == '\0') {
            Err(InvalidError::ContainsNull)
        } else {
            // Safety: Validation is performed above.
            Ok(unsafe { Self::from_str_unchecked(identifier) })
        }
    }

    pub fn from_byte_slice(bytes: &[u8]) -> Result<&Id, ParseError> {
        Ok(Id::try_from_str(std::str::from_utf8(bytes)?)?)
    }
}

impl Identifier {
    pub fn from_id(identifier: &Id) -> Self {
        identifier.to_identifier()
    }

    pub fn from_string(identifier: String) -> Result<Self, InvalidError> {
        Id::try_from_str(&identifier)?;
        Ok(Self(identifier))
    }

    pub fn from_boxed_str(identifier: Box<str>) -> Result<Self, InvalidError> {
        Self::from_string(identifier.into_string())
    }

    pub fn try_from_str(identifier: &str) -> Result<Self, InvalidError> {
        Id::try_from_str(identifier)?;
        Ok(Self(identifier.to_owned()))
    }

    pub fn from_byte_slice(bytes: &[u8]) -> Result<Self, ParseError> {
        Id::from_byte_slice(bytes).map(Id::to_identifier)
    }

    pub fn from_utf8(bytes: Vec<u8>) -> Result<Self, ParseError> {
        Ok(Self::from_string(String::from_utf8(bytes).map_err(|e| e.utf8_error())?)?)
    }

    pub fn as_id(&self) -> &Id {
        unsafe {
            // Safety: Constructors for Identifier delegates to same validation check as Id, and representations for str and Id
            // are matching.
            std::mem::transmute::<&str, &Id>(self.0.as_str())
        }
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn into_boxed_id(self) -> Box<Id> {
        unsafe {
            // Safety: Box<Id> should have same layout as Box<str>.
            std::mem::transmute::<Box<str>, Box<Id>>(self.0.into_boxed_str())
        }
    }
}

impl From<&Id> for Identifier {
    fn from(identifier: &Id) -> Self {
        Self::from_id(identifier)
    }
}

impl From<Box<Id>> for Identifier {
    fn from(identifier: Box<Id>) -> Self {
        unsafe {
            // Safety: Box<Id> should have same layout as Box<str>.
            Self(std::mem::transmute::<Box<Id>, Box<str>>(identifier).into_string())
        }
    }
}

impl From<&Id> for Box<Id> {
    fn from(identifier: &Id) -> Self {
        Identifier::from(identifier).into_boxed_id()
    }
}

#[derive(Clone, Debug, thiserror::Error, PartialEq)]
#[non_exhaustive]
pub enum InvalidError {
    #[error("identifiers cannot be empty")]
    Empty,
    #[error("identifiers cannot contain null bytes")]
    ContainsNull,
}

impl<'a> TryFrom<&'a str> for &'a Id {
    type Error = InvalidError;

    #[inline]
    fn try_from(identifier: &'a str) -> Result<&'a Id, InvalidError> {
        Id::try_from_str(identifier)
    }
}

impl TryFrom<String> for Identifier {
    type Error = InvalidError;

    #[inline]
    fn try_from(identifier: String) -> Result<Self, InvalidError> {
        Self::from_string(identifier)
    }
}

impl TryFrom<Box<str>> for Identifier {
    type Error = InvalidError;

    #[inline]
    fn try_from(identifier: Box<str>) -> Result<Self, InvalidError> {
        Self::from_boxed_str(identifier)
    }
}

impl TryFrom<&str> for Identifier {
    type Error = InvalidError;

    #[inline]
    fn try_from(identifier: &str) -> Result<Self, InvalidError> {
        Self::try_from_str(identifier)
    }
}

#[derive(Clone, Debug, thiserror::Error)]
pub enum ParseError {
    #[error(transparent)]
    InvalidIdentifier(#[from] InvalidError),
    #[error(transparent)]
    InvalidSequence(#[from] std::str::Utf8Error),
}

impl<'a> TryFrom<&'a [u8]> for &'a Id {
    type Error = ParseError;

    #[inline]
    fn try_from(bytes: &'a [u8]) -> Result<&'a Id, ParseError> {
        Id::from_byte_slice(bytes)
    }
}

impl TryFrom<&[u8]> for Identifier {
    type Error = ParseError;

    #[inline]
    fn try_from(bytes: &[u8]) -> Result<Self, ParseError> {
        Self::from_byte_slice(bytes)
    }
}

impl TryFrom<Vec<u8>> for Identifier {
    type Error = ParseError;

    #[inline]
    fn try_from(bytes: Vec<u8>) -> Result<Self, ParseError> {
        Self::from_utf8(bytes)
    }
}

impl std::borrow::Borrow<Id> for Identifier {
    #[inline]
    fn borrow(&self) -> &Id {
        self.as_id()
    }
}

impl<'a> ToOwned for Id {
    type Owned = Identifier;

    #[inline]
    fn to_owned(&self) -> Identifier {
        self.to_identifier()
    }
}

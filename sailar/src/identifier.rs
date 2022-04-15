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

    pub fn from_str(identifier: &str) -> Result<&Id, InvalidError> {
        if identifier.is_empty() {
            Err(InvalidError::Empty)
        } else if identifier.chars().any(|c| c == '\0') {
            Err(InvalidError::ContainsNull)
        } else {
            // Safety: Representation of Id allows safe transmute here.
            Ok(unsafe { std::mem::transmute::<&str, &Id>(identifier) })
        }
    }

    pub fn from_byte_slice(bytes: &[u8]) -> Result<&Id, ParseError> {
        Ok(Id::from_str(std::str::from_utf8(bytes)?)?)
    }
}

impl Identifier {
    #[inline]
    pub fn from_id(identifier: &Id) -> Self {
        identifier.to_identifier()
    }

    #[inline]
    pub fn from_string(identifier: String) -> Result<Self, InvalidError> {
        Id::from_str(&identifier)?;
        Ok(Self(identifier))
    }

    #[inline]
    fn from_str(identifier: &str) -> Result<Self, InvalidError> {
        Id::from_str(&identifier)?;
        Ok(Self(identifier.to_owned()))
    }

    #[inline]
    pub fn from_byte_slice(bytes: &[u8]) -> Result<Self, ParseError> {
        Id::from_byte_slice(bytes).map(Id::to_identifier)
    }

    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl From<&Id> for Identifier {
    #[inline]
    fn from(identifier: &Id) -> Self {
        Self::from_id(identifier)
    }
}

#[derive(Clone, Debug, thiserror::Error)]
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
        Id::from_str(identifier)
    }
}

impl TryFrom<String> for Identifier {
    type Error = InvalidError;

    #[inline]
    fn try_from(identifier: String) -> Result<Self, InvalidError> {
        Self::from_string(identifier)
    }
}

impl TryFrom<&str> for Identifier {
    type Error = InvalidError;

    #[inline]
    fn try_from(identifier: &str) -> Result<Self, InvalidError> {
        Self::from_str(identifier)
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

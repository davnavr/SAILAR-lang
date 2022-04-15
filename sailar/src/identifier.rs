//! Code for interacting with SAILAR identifiers.

use std::fmt::{Debug, Display, Formatter};
use std::ops::{Deref, DerefMut};

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

macro_rules! deref_impls {
    ($implementor: ident, $target: ty) => {
        impl Deref for $implementor {
            type Target = $target;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl DerefMut for $implementor {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }
    };
}

deref_impls!(Identifier, String);
deref_impls!(Id, str);

impl Identifier {
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
    fn from(identifier: &Id) -> Self {
        Self(identifier.0.to_string())
    }
}

#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum InvalidIdentifier {
    #[error("identifiers cannot be empty")]
    Empty,
    #[error("identifiers cannot contain null bytes")]
    ContainsNull,
}

impl<'a> TryFrom<&'a str> for &'a Id {
    type Error = InvalidIdentifier;

    fn try_from(identifier: &'a str) -> Result<&'a Id, InvalidIdentifier> {
        if identifier.is_empty() {
            Err(InvalidIdentifier::Empty)
        } else if identifier.chars().any(|c| c == '\0') {
            Err(InvalidIdentifier::ContainsNull)
        } else {
            Ok(unsafe { std::mem::transmute::<&'a str, &'a Id>(identifier) })
        }
    }
}

impl TryFrom<String> for Identifier {
    type Error = InvalidIdentifier;

    fn try_from(identifier: String) -> Result<Self, InvalidIdentifier> {
        <&Id>::try_from(identifier.as_str())?;
        Ok(Self(identifier))
    }
}

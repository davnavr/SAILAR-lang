//! Types that represent indices used to refer to records in a SAILAR module.

use std::fmt::{Debug, Formatter};

macro_rules! index_implementations {
    ($index_type: ty) => {
        impl From<usize> for $index_type {
            fn from(index: usize) -> Self {
                Self(index)
            }
        }

        impl From<$index_type> for usize {
            fn from(index: $index_type) -> usize {
                index.0
            }
        }

        impl Debug for $index_type {
            fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
                Debug::fmt(&self.0, f)
            }
        }
    };
}

/// Represents an index to an identifier in a module.
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Identifier(usize);

index_implementations!(Identifier);

/// Represents an index to a function signature in a module.
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct FunctionSignature(usize);

index_implementations!(FunctionSignature);

/// Represents an index to a type signature in a module.
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct TypeSignature(usize);

index_implementations!(TypeSignature);

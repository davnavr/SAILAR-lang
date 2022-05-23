//! Types that represent indices used to refer to records in a SAILAR module.

use std::fmt::{Debug, Formatter};
use std::num::TryFromIntError;

macro_rules! index_type {
    ($(#[$meta:meta])* $name:ident) => {
        #[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
        #[repr(transparent)]
        pub struct $name(usize);

        impl From<usize> for $name {
            #[inline]
            fn from(index: usize) -> Self {
                Self(index)
            }
        }

        impl From<$name> for usize {
            #[inline]
            fn from(index: $name) -> usize {
                index.0
            }
        }

        impl TryFrom<u32> for $name {
            type Error = TryFromIntError;

            #[inline]
            fn try_from(index: u32) -> Result<Self, TryFromIntError> {
                usize::try_from(index).map(Self)
            }
        }

        impl TryFrom<$name> for u32 {
            type Error = TryFromIntError;

            #[inline]
            fn try_from(index: $name) -> Result<u32, TryFromIntError> {
                u32::try_from(index.0)
            }
        }

        impl Debug for $name {
            fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
                Debug::fmt(&self.0, f)
            }
        }

        impl std::hash::Hash for $name {
            #[inline]
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                state.write_usize(self.0)
            }
        }
    };
}

index_type!(
    #[doc("Represents an index to an identifier in a module.")]
    Identifier
);

index_type!(
    #[doc("Represents an index to a function signature in a module.")]
    FunctionSignature
);

index_type!(
    #[doc("Represents an index to a type signature in a module.")]
    TypeSignature
);

index_type!(
    #[doc("Represents an index to a code block in a module.")]
    CodeBlock
);

//! Types that represent indices used to refer to records in a SAILAR module.

use std::fmt::{Debug, Formatter};
use std::num::TryFromIntError;

macro_rules! index_type {
    ($(#[$meta:meta])* $name:ident) => {
        #[derive(Copy, Clone, Ord, PartialOrd)]
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

        impl std::cmp::PartialEq for $name {
            #[inline]
            fn eq(&self, other: &Self) -> bool {
                self.0 == other.0
            }
        }

        impl std::cmp::Eq for $name {}
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

index_type!(
    #[doc("Represents an index to a function import or definition in a module in that order.")]
    FunctionTemplate
);

index_type!(
    #[doc("Represents an index to a function instantiation in a module.")]
    FunctionInstantiation
);

index_type!(
    #[doc("Represents an index referring to a register in a code block.")]
    #[doc("Indices start first with the input registers followed by temporary registers.")]
    Register
);

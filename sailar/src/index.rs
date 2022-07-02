//! Types that represent indices used to refer to records in a SAILAR module.

use crate::num::{IntegerEncodingError, VarU28};
use std::fmt::{Debug, Formatter};
use std::num::TryFromIntError;

macro_rules! index_type {
    ($(#[$meta:meta])* $name:ident) => {
        #[derive(Copy, Clone, Eq, Hash, Ord, PartialEq, PartialOrd)]
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

        impl TryFrom<VarU28> for $name {
            type Error = TryFromIntError;

            #[inline]
            fn try_from(index: VarU28) -> Result<Self, Self::Error> {
                Ok(Self(usize::try_from(index)?))
            }
        }

        impl TryFrom<$name> for VarU28 {
            type Error = IntegerEncodingError;

            #[inline]
            fn try_from(index: $name) -> Result<VarU28, Self::Error> {
                VarU28::try_from(index.0)
            }
        }

        impl Debug for $name {
            fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
                Debug::fmt(&self.0, f)
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

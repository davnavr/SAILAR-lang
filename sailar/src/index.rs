//! Types that represent indices used to refer to records in a SAILAR module.

use crate::num::{IntegerEncodingError, VarU28};
use std::fmt::Formatter;
use std::num::TryFromIntError;

pub(crate) trait Index: Into<usize> + Copy {
    fn name() -> &'static str;
}

macro_rules! index_type {
    ($(#[$meta:meta])* $name:ident { name = $description:literal }) => {
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

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
                std::fmt::Debug::fmt(&self.0, f)
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
                write!(f, "#{}", self.0)
            }
        }

        impl Index for $name {
            fn name() -> &'static str {
                $description
            }
        }
    };
}

index_type!(
    /// Represents an index to an identifier in a module.
    Identifier { name = "identifier string" }
);

index_type!(
    /// Represents an index to a function signature in a module.
    FunctionSignature { name = "function signature" }
);

index_type!(
    /// Represents an index to a type signature in a module.
    TypeSignature { name = "type signature" }
);

index_type!(
    /// Represents an index to a code block in a module.
    CodeBlock { name = "code block" }
);

index_type!(
    /// Represents an index to an imported function template, or a function template in the current module, in that order.
    FunctionTemplate { name = "function template" }
);

index_type!(
    /// Represents an index to a function (an instantiation of a function template) in the current module.
    Function { name = "function" }
);

index_type!(
    /// Represents an index referring to a register in a code block.
    ///
    /// Indices start first with the input registers followed by temporary registers.
    Register { name = "register" }
);

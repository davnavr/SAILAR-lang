//! Contains types for reading and writing SAILAR modules.
//!
//! For a summary of the instruction set, see [`instruction::Instruction`].

pub mod binary;
pub mod helper;
pub mod identifier;
pub mod loader;
pub mod versioning;

pub use identifier::{Id, Identifier};

#[macro_export]
#[doc(hidden)]
macro_rules! enum_case_from_impl {
    ($implementor: ty, $case_name: ident, $case_type: ty) => {
        impl std::convert::From<$case_type> for $implementor {
            #[inline]
            fn from(value: $case_type) -> Self {
                Self::$case_name(value)
            }
        }
    };
}

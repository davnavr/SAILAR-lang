//! Contains types for reading and writing SAILAR modules.
//!
//! For a summary of the instruction set, see [`instruction::Instruction`].

pub mod binary;
pub mod builder;
pub mod helper;
pub mod identifier;
pub mod index;
pub mod instruction;
pub mod num;
pub mod reader;
pub mod record;
pub mod signature;
pub mod validation;
pub mod versioning;
pub mod writer;

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

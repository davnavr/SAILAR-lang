//! Contains types for reading and writing SAILAR modules.

pub mod binary;
pub mod identifier;
pub mod instruction;
pub mod type_system;
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

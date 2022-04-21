//! Contains types for reading and writing SAILAR modules.

pub mod binary;
pub mod block;
pub mod data;
pub mod function;
pub mod identifier;
pub mod instruction_set;
pub mod module;
pub mod type_system;

pub use identifier::{Id, Identifier};
pub use module::Definition as ModuleDefinition;

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

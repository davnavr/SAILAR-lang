//! The C API for SAILAR.

#![allow(non_snake_case, improper_ctypes_definitions, clippy::missing_safety_doc)]

pub mod error;
pub mod identifier;
pub mod module;

#[macro_export]
#[doc(hidden)]
macro_rules! box_wrapper {
    ($name: ident, $wrapped: ty, $null_message: literal) => {
        pub struct $name(*mut $wrapped);

        impl $name {
            pub unsafe fn new(value: $wrapped) -> Self {
                Self(Box::into_raw(Box::new(value)))
            }

            pub unsafe fn as_ref<'a>(self) -> &'a $wrapped {
                self.0.as_ref().expect($null_message)
            }

            #[inline]
            pub unsafe fn null() -> Self {
                Self(std::ptr::null_mut())
            }

            pub unsafe fn into_box(self) -> Box<$wrapped> {
                Box::from_raw(self.0)
            }
        }
    };
}

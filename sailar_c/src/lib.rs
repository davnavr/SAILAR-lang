//! The C API for SAILAR.

#![allow(non_snake_case, clippy::missing_safety_doc)]

pub mod error;
pub mod identifier;
pub mod module;

#[macro_export]
#[doc(hidden)]
macro_rules! box_wrapper {
    ($name: ident, $wrapped: ty, $null_message: literal) => {
        #[repr(transparent)]
        pub struct $name(*mut ());

        impl $name {
            pub unsafe fn new(value: $wrapped) -> Self {
                Self(Box::into_raw(Box::new(value)) as *mut ())
            }

            #[inline]
            pub unsafe fn as_mut(self) -> *mut $wrapped {
                self.0 as *mut $wrapped
            }

            #[inline]
            pub unsafe fn as_ref<'a>(self) -> &'a $wrapped {
                self.as_mut().as_ref().expect($null_message)
            }

            #[inline]
            pub unsafe fn null() -> Self {
                Self(std::ptr::null_mut())
            }

            pub unsafe fn into_box(self) -> Box<$wrapped> {
                Box::from_raw(self.as_mut())
            }
        }
    };
}

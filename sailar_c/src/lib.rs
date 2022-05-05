//! The C API for SAILAR.

#![allow(non_snake_case, clippy::missing_safety_doc)]

pub mod error;
pub mod identifier;
pub mod reader;

#[macro_export]
#[doc(hidden)]
macro_rules! box_wrapper {
    ($name: ident, $wrapped: ty, $getter_visibility: vis) => {
        #[repr(transparent)]
        pub struct $name(*mut std::ffi::c_void);

        #[allow(dead_code)]
        impl $name {
            pub(crate) unsafe fn new(value: $wrapped) -> Self {
                Self(Box::into_raw(Box::new(value)) as *mut std::ffi::c_void)
            }

            #[inline]
            $getter_visibility unsafe fn into_mut(self) -> *mut $wrapped {
                self.0 as *mut $wrapped
            }

            #[inline]
            $getter_visibility unsafe fn into_ref<'a>(self) -> &'a $wrapped {
                self.into_mut().as_ref().expect("reference must not be null")
            }

            #[inline]
            pub(crate) unsafe fn null() -> Self {
                Self(std::ptr::null_mut())
            }

            #[inline]
            $getter_visibility unsafe fn into_box(self) -> Box<$wrapped> {
                Box::from_raw(self.into_mut())
            }
        }
    };
}

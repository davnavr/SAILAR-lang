//! The C API for SAILAR.

#![allow(non_snake_case, clippy::missing_safety_doc)]

pub mod buffer;
pub mod error;
pub mod identifier;
pub mod reader;
pub mod signature;

#[macro_export]
#[doc(hidden)]
macro_rules! handle_error {
    ($result: expr, $error: ident) => {
        match crate::error::handle_error($result, $error) {
            Some(value) => value,
            None => return Default::default(),
        }
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! box_wrapper {
    ($name: ident($visibility: vis $wrapped: ty)) => {
        #[repr(transparent)]
        pub struct $name(*mut std::ffi::c_void);

        #[allow(dead_code)]
        impl $name {
            $visibility unsafe fn new(value: $wrapped) -> Self {
                Self(Box::into_raw(Box::new(value)) as *mut std::ffi::c_void)
            }

            #[inline]
            $visibility unsafe fn into_mut_ptr(self) -> *mut $wrapped {
                self.0 as *mut $wrapped
            }

            #[inline]
            $visibility unsafe fn into_ref<'a>(self) -> &'a $wrapped {
                self.into_mut_ptr().as_ref().expect("reference must not be null")
            }

            #[inline]
            $visibility unsafe fn into_mut<'a>(self) -> &'a mut $wrapped {
                self.into_mut_ptr().as_mut().expect("reference must not be null")
            }

            #[inline]
            pub const fn null() -> Self {
                Self(std::ptr::null_mut())
            }

            #[inline]
            pub fn is_null(&self) -> bool {
                self.0.is_null()
            }

            #[inline]
            $visibility unsafe fn into_box(self) -> Box<$wrapped> {
                Box::from_raw(self.into_mut())
            }
        }

        impl std::default::Default for $name {
            fn default() -> Self {
                Self::null()
            }
        }
    };
}

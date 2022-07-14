//! Provides a C API for the low-level SAILAR module builder API.

#![warn(non_snake_case)]

use crate::error::{self, Error};

pub type Builder = sailar::builder::Builder<'static>;

#[no_mangle]
pub unsafe extern "C" fn sailar_create_builder() -> *const Builder {
    Box::into_raw(Box::new(Builder::new())) as *const _
}

//! Provides a C API for the low-level SAILAR module builder API.

#![warn(non_snake_case)]

use crate::error::{self, Error};
use crate::path::FilePath;

pub type Builder = sailar::builder::Builder<'static>;

/// Creates an empty module builder.
///
/// # Safety
///
/// See the [`crate#safety`] documentation.
#[no_mangle]
pub unsafe extern "C" fn sailar_builder_create() -> *const Builder {
    Box::into_raw(Box::new(Builder::new())) as *const _
}

/// Disposes a module builder.
///
/// # Safety
///
/// Callers must ensure that the `builder` has not already been disposed.
#[no_mangle]
pub unsafe extern "C" fn sailar_builder_dispose(builder: *mut Builder) {
    if !builder.is_null() {
        Box::from_raw(builder);
    }
}

/// Writes the contents of a SAILAR module to the specified file.
///
/// # Safety
///
/// Callers must ensure that the `builder` has not already been disposed.
#[no_mangle]
pub unsafe extern "C" fn sailar_builder_write_to_path(builder: *mut Builder, path: *const FilePath, error: *mut *const Error) {
    error::handle_or_default(
        || match (builder.as_ref(), path.as_ref()) {
            (Some(builder), Some(path)) => {
                let destination = std::fs::OpenOptions::new().write(true).truncate(true).open(path)?;
                Ok(builder.write_to(destination)?)
            }
            (None, _) => Err("cannot write contents of null builder")?,
            (_, None) => Err("cannot write builder contents to null path")?,
        },
        error,
    )
}

//! Provides a C API for the low-level SAILAR module builder API.

#![warn(non_snake_case)]

use crate::buffer::Buffer;
use crate::error::{self, Error};
use crate::path::FilePath;
use sailar::record;
use sailar::identifier::Id;

pub type Builder = sailar::builder::Builder<'static>;

/// Creates an empty module builder.
///
/// # Safety
///
/// See the [`crate#safety`] documentation.
#[no_mangle]
pub unsafe extern "C" fn sailar_builder_create() -> *mut Builder {
    Box::into_raw(Box::new(Builder::new()))
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
pub unsafe extern "C" fn sailar_builder_write_to_path(builder: *const Builder, path: *const FilePath, error: *mut *const Error) {
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

/// Writes the contents of a SAILAR module to a byte buffer.
///
/// # Safety
///
/// Callers must ensure that the `builder` has not already been disposed.
#[no_mangle]
pub unsafe extern "C" fn sailar_builder_write_to_buffer(builder: *const Builder, error: *mut *const Error) -> *mut Buffer {
    error::handle_or(
        || {
            if let Some(builder) = builder.as_ref() {
                let mut buffer = Buffer::with_capacity(128);
                builder.write_to(&mut buffer)?;
                Ok(Box::into_raw(Box::new(buffer)))
            } else {
                Err("unable to write null builder to buffer")?
            }
        },
        std::ptr::null_mut(),
        error,
    )
}

/// Appends a module identifier metadata record to the SAILAR module.
///
/// # Safety
///
/// Callers must ensure that the `builder` has not already been disposed.
#[no_mangle]
pub unsafe extern "C" fn sailar_builder_add_module_identifier(builder: *mut Builder, name: *const Box<Id>, version_numbers: *const u16, version_numbers_count: usize) {
    let builder = builder.as_mut().unwrap();
    let name = name.as_ref().unwrap();
    // TODO: How to ensure version numbers are not null?
    let version_numbers = std::slice::from_raw_parts(version_numbers, version_numbers_count);
    let version: Box<[_]> = version_numbers.iter().copied().map(sailar::num::VarU28::from_u16).collect();
    builder.add_record(record::MetadataField::ModuleIdentifier(record::ModuleIdentifier::new_owned(name.as_ref().into(), version)))
}

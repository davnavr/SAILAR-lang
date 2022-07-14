//! Functions for manipulating identifiers.

#![warn(non_snake_case)]

use crate::error::{self, Error};
use sailar::identifier::Id;

/// Creates a SAILAR identifier string by copying from a sequence of bytes with the specified byte [`length`]. If the bytes are
/// not valid UTF-8, returns `null` and an error that can be disposed with [`sailar_dispose_error`].
///
/// The identifier can be disposed later with [`sailar_dispose_identifier`].
///
/// # Safety
///
/// The identifier should not have been disposed.
///
/// [`sailar_dispose_error`]: error::sailar_dispose_error
#[no_mangle]
pub unsafe extern "C" fn sailar_identifier_from_utf8(
    contents: *const u8,
    length: usize,
    error: *mut *const Error,
) -> *const Box<Id> {
    let bytes = std::slice::from_raw_parts(contents, length);
    error::handle_or(
        || {
            let s = std::str::from_utf8(bytes)?;
            let id = sailar::identifier::Identifier::try_from_str(s)?.into_boxed_id();
            Ok(Box::into_raw(Box::new(id)) as *const _)
        },
        std::ptr::null(),
        error,
    )
}

/// Creates a SAILAR identifier string from a sequence of UTF-16 code points with the specified `count`.
///
/// See the documentation for [`sailar_identifier_from_utf8`] for more information.
///
/// # Safety
///
/// See [`sailar_identifier_from_utf8`].
#[no_mangle]
pub unsafe extern "C" fn sailar_identifier_from_utf16(
    contents: *const u16,
    count: usize,
    error: *mut *const Error,
) -> *const Box<Id> {
    let code_points = std::slice::from_raw_parts(contents, count);
    error::handle_or(
        || {
            let s = String::from_utf16(code_points)?;
            let id = sailar::identifier::Identifier::try_from(s)?.into_boxed_id();
            Ok(Box::into_raw(Box::new(id)) as *const _)
        },
        std::ptr::null(),
        error,
    )
}

/// Disposes a SAILAR identifier string.
///
/// # Safety
///
/// Callers must ensure that the `identifier` has not already been disposed.
///
/// This function is **not thread safe**.
#[no_mangle]
pub unsafe extern "C" fn sailar_dispose_identifier(identifier: *mut Box<Id>) {
    if !identifier.is_null() {
        Box::from_raw(identifier);
    }
}

/// Returns a pointer to the contents of a SAILAR identifier, as well as the length in bytes.
///
/// # Safety
///
/// The identifier should not have been disposed.
#[no_mangle]
pub unsafe extern "C" fn sailar_identifier_contents(identifier: *mut Box<Id>, length: *mut usize) -> *const u8 {
    if let Some(id) = identifier.as_ref() {
        let bytes = id.as_bytes();
        *length = bytes.len();
        bytes.as_ptr()
    } else {
        std::ptr::null()
    }
}

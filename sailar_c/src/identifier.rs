//! Functions for manipulating identifiers.

#![warn(non_snake_case)]

use crate::error::{self, Error};
use sailar::identifier::Id;

/// Creates a SAILAR identifier string by copying from a sequence of bytes. If the bytes are not valid UTF-8, returns `null`
/// and an error that can be disposed with `sailar_dispose_error`.
///
/// The identifier can be disposed later with `sailar_dispose_identifier`.
///
/// # Safety
///
/// See the [`crate`] documentation.
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

// #[no_mangle]
// pub unsafe extern "C" fn sailar_dispose_identifier(identifier: Identifier) {
//     identifier.into_box();
// }

// /// Returns a pointer to the contents of a SAILAR identifier, as well as the length in bytes.
// #[no_mangle]
// pub unsafe extern "C" fn sailar_get_identifier_contents(identifier: Identifier, length: *mut usize) -> *const u8 {
//     let bytes = identifier.into_ref().as_bytes();
//     *length = bytes.len();
//     bytes.as_ptr()
// }

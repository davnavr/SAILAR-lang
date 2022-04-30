//! Functions for creating identifiers.

use crate::error::{self, SAILErrorRef};
use sailar::identifier::Identifier;

crate::box_wrapper!(SAILIdentifierRef, Identifier, "identifier must not be null");

/// Creates a SAILAR identifier string from a sequence of bytes. If the bytes are not valid UTF-8, returns `null`
/// and an error that can be disposed with `SAILDisposeError`.
///
/// The identifier can be disposed later with `SAILDisposeIdentifier`.
#[no_mangle]
pub unsafe extern "C" fn SAILCreateIdentifier(contents: *const u8, length: usize, error: *mut SAILErrorRef) -> SAILIdentifierRef {
    let bytes = std::slice::from_raw_parts(contents, length);
    match Identifier::try_from(bytes) {
        Ok(identifier) => SAILIdentifierRef::new(identifier),
        Err(e) => {
            *error = error::from_error(e);
            SAILIdentifierRef::null()
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn SAILDisposeIdentifier(identifier: SAILIdentifierRef) {
    identifier.into_box();
}

/// Returns a pointer to the contents of a SAILAR identifier, as well as the length in bytes.
#[no_mangle]
pub unsafe extern "C" fn SAILGetIdentifierContents(identifier: SAILIdentifierRef, length: *mut usize) -> *const u8 {
    let bytes = identifier.as_ref().as_bytes();
    *length = bytes.len();
    bytes.as_ptr()
}

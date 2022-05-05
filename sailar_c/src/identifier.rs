//! Functions for creating identifiers.

use crate::error::Error;
use sailar::identifier::Identifier;

crate::box_wrapper!(IdentifierRef, Identifier, pub);

/// Creates a SAILAR identifier string from a sequence of bytes. If the bytes are not valid UTF-8, returns `null`
/// and an error that can be disposed with `sailar_dispose_error`.
///
/// The identifier can be disposed later with `sailar_dispose_identifier`.
#[no_mangle]
pub unsafe extern "C" fn sailar_create_identifier(contents: *const u8, length: usize, error: *mut Error) -> IdentifierRef {
    let bytes = std::slice::from_raw_parts(contents, length);
    match Identifier::try_from(bytes) {
        Ok(identifier) => IdentifierRef::new(identifier),
        Err(e) => {
            *error = Error::from_error(e);
            IdentifierRef::null()
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn sailar_dispose_identifier(identifier: IdentifierRef) {
    identifier.into_box();
}

/// Returns a pointer to the contents of a SAILAR identifier, as well as the length in bytes.
#[no_mangle]
pub unsafe extern "C" fn sailar_get_identifier_contents(identifier: IdentifierRef, length: *mut usize) -> *const u8 {
    let bytes = identifier.into_ref().as_bytes();
    *length = bytes.len();
    bytes.as_ptr()
}

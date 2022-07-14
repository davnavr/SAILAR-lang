//! Functions for manipulating identifiers.

use crate::error::Error;
use sailar::identifier::Id;

pub type Identifier = Box<Id>;

/// Creates a SAILAR identifier string by copying from a sequence of bytes. If the bytes are not valid UTF-8, returns `null`
/// and an error that can be disposed with `sailar_dispose_error`.
///
/// The identifier can be disposed later with `sailar_dispose_identifier`.
/// 
/// # Safety
/// 
/// See the [`crate`] documentation.
#[no_mangle]
pub unsafe extern "C" fn sailar_create_identifier(contents: *const u8, length: usize, error: *mut Error) -> *const Identifier {
    let bytes = std::slice::from_raw_parts(contents, length);
    //Identifier::new(crate::handle_error!(identifier::Identifier::try_from(bytes), error))
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

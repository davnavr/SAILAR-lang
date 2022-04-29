//! Functions for creating identifiers.

use crate::error::{self, SAILErrorRef};
use sailar::identifier::Identifier;

pub struct SAILIdentifierRef(*mut Identifier);

/// Creates a SAILAR identifier string from a null-terminated sequence of bytes.
#[no_mangle]
pub unsafe extern "C" fn SAILCreateIdentifier(contents: *const u8, error: *mut SAILErrorRef) -> SAILIdentifierRef {
    let length = {
        let mut start = contents;
        let mut index = 0usize;
        loop {
            if *start == 0u8 {
                break index;
            }

            index += 1;
            start = start.add(1);
        }
    };

    let bytes = std::slice::from_raw_parts(contents, length);
    match Identifier::try_from(bytes) {
        Ok(identifier) => SAILIdentifierRef(Box::into_raw(Box::new(identifier))),
        Err(e) => {
            *error = error::from_error(e);
            SAILIdentifierRef(std::ptr::null_mut())
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn SAILDisposeIdentifier(identifier: SAILIdentifierRef) {
    Box::from_raw(identifier.0);
}

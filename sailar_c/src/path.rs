//! Helper module for the manipulation of file paths.

use crate::error::{self, Error};

pub type FilePath = std::path::PathBuf;

/// Creates a SAILAR file path from a UTF-8 string with the specified byte `length`.
///
/// # Safety
///
/// See the [`crate#safety`] documentation.
#[no_mangle]
pub unsafe extern "C" fn sailar_path_from_utf8(contents: *const u8, length: usize, error: *mut *const Error) -> *const FilePath {
    let bytes = if contents.is_null() {
        Default::default()
    } else {
        std::slice::from_raw_parts(contents, length)
    };

    error::handle_or(
        || Ok(Box::into_raw(Box::new(FilePath::from(std::str::from_utf8(bytes)?))) as *const _),
        std::ptr::null(),
        error,
    )
}

/// Creates a SAILAR file path from a sequence of UTF-16 code points.
///
/// # Safety
///
/// See [`sailar_path_from_utf8`].
#[no_mangle]
pub unsafe extern "C" fn sailar_path_from_utf16(contents: *const u16, count: usize, error: *mut *const Error) -> *const FilePath {
    let bytes = if contents.is_null() {
        Default::default()
    } else {
        std::slice::from_raw_parts(contents, count)
    };

    error::handle_or(
        || Ok(Box::into_raw(Box::new(FilePath::from(std::string::String::from_utf16(bytes)?))) as *const _),
        std::ptr::null(),
        error,
    )
}

/// Disposes a SAILAR file path.
///
/// # Safety
///
/// Callers must ensure that the `path` has not already been disposed.
///
/// This function is **not thread safe**.
#[no_mangle]
pub unsafe extern "C" fn sailar_dispose_path(path: *mut FilePath) {
    if !path.is_null() {
        Box::from_raw(path);
    }
}

//! Helper module for the manipulation of byte buffers.

pub type Buffer = Vec<u8>;

/// Disposes a byte buffer.
///
/// # Safety
///
/// Callers must ensure that the `buffer` has not already been disposed.
#[no_mangle]
pub unsafe extern "C" fn sailar_buffer_dispose(buffer: *mut Buffer) {
    if !buffer.is_null() {
        Box::from_raw(buffer);
    }
}

/// Returns a pointer to the contents of the byte buffer, as well as the length in bytes.
///
/// # Safety
///
/// Callers must ensure that the `buffer` has not already been disposed.
#[no_mangle]
pub unsafe extern "C" fn sailar_buffer_contents(buffer: *const Buffer, length: *mut usize) -> *const u8 {
    if let Some(buffer) = buffer.as_ref() {
        *length = buffer.len();
        buffer.as_ptr()
    } else {
        *length = 0;
        std::ptr::null()
    }
}

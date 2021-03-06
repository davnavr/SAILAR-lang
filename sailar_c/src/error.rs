//! Provides error handling.

#![warn(non_snake_case)]

pub type Error = Box<dyn std::error::Error + Send + Sync + 'static>;

/// Handles any `error` that is produced by a closure that returns a [`Result`].
///
/// # Safety
///
/// The `error` must be [valid](std::ptr#safety).
///
/// [`Result`]: std::result::Result
pub unsafe fn handle<T, F: FnOnce() -> Result<T, Error>>(expression: F, error: *mut *const Error) -> Option<T> {
    match expression() {
        Ok(value) => Some(value),
        Err(e) => {
            *error = Box::into_raw(Box::new(e)) as *const Error;
            None
        }
    }
}

/// Handles any `error` that is produced by a closure that returns a [`Result`], calling `or_else` if an error occurs.
///
/// # Safety
///
/// The `error` must be [valid](std::ptr#safety).
pub unsafe fn handle_or_else<T, F, E>(expression: F, or_else: E, error: *mut *const Error) -> T
where
    F: FnOnce() -> Result<T, Error>,
    E: FnOnce() -> T,
{
    handle(expression, error).unwrap_or_else(or_else)
}

/// Handles any `error` that is produced by a closure that returns a [`Result`], returning a `default` value if an error occurs.
///
/// # Safety
///
/// The `error` must be [valid](std::ptr#safety).
pub unsafe fn handle_or<T, F: FnOnce() -> Result<T, Error>>(expression: F, default: T, error: *mut *const Error) -> T {
    handle_or_else(expression, || default, error)
}
/// Handles any `error` that is produced by a closure that returns a [`Result`], returning [`Default::default`] if an error
/// occurs.
///
/// # Safety
///
/// The `error` must be [valid](std::ptr#safety).
pub unsafe fn handle_or_default<T: Default, F: FnOnce() -> Result<T, Error>>(expression: F, error: *mut *const Error) -> T {
    handle_or(expression, Default::default(), error)
}

/// Disposes the specified `error`.
///
/// # Safety
///
/// Callers must ensure that the `error` has not already been disposed.
#[no_mangle]
pub unsafe extern "C" fn sailar_error_dispose(error: *mut Error) {
    if !error.is_null() {
        Box::from_raw(error);
    }
}

/// Allocates a string describing an `error`.
///
/// # Safety
///
/// Callers must ensure that the `error` has not already been disposed.
#[no_mangle]
pub unsafe extern "C" fn sailar_error_message(error: *mut Error) -> *const String {
    if let Some(error) = error.as_ref() {
        Box::into_raw(Box::new(error.to_string())) as *const _
    } else {
        std::ptr::null()
    }
}

/// Returns a pointer to the UTF-8 contents of an error message, as well as the length in bytes.
///
/// # Safety
///
/// The `message` should not have been disposed.
#[no_mangle]
pub unsafe extern "C" fn sailar_error_message_contents(message: *mut String, length: *mut usize) -> *const u8 {
    if let Some(message) = message.as_ref() {
        *length = message.len();
        message.as_bytes().as_ptr()
    } else {
        *length = 0;
        std::ptr::null()
    }
}

/// Disposes a string containing an error message.
///
/// # Safety
///
/// Callers must ensure that the `message` has not already been disposed.
#[no_mangle]
pub unsafe extern "C" fn sailar_error_message_dispose(message: *mut String) {
    if !message.is_null() {
        Box::from_raw(message);
    }
}

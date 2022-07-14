//! Provides error handling.

pub type Error = Box<dyn std::error::Error + Send + 'static>;

/// Wraps a [`Result`], storing any `error` that occurs.
/// 
/// # Safety
/// 
/// The `error` must be [valid](std::ptr#safety).
pub unsafe fn wrap<T, E, F>(expression: F, error: *mut *const Error) -> Option<T>
where
    E: std::error::Error + Send + 'static,
    F: FnOnce() -> Result<T, E>,
{
    match expression() {
        Ok(value) => Some(value),
        Err(e) => {
            *error = Box::into_raw(Box::new(Box::new(e))) as *const Error;
            None
        }
    }
}

/// Disposes the specified `error`.
/// 
/// # Safety
/// 
/// Callers must ensure that the `error` has not already been disposed.
/// 
/// This function is **not thread safe**.
#[no_mangle]
pub unsafe extern "C" fn sailar_dispose_error(error: *mut Error) {
    if !error.is_null() {
        Box::from_raw(error);
    }
}

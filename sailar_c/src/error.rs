//! Error handling functions.

// Workaround since dyn makes a fat pointer
crate::box_wrapper!(Error(pub Box<dyn std::error::Error>));

impl Error {
    pub(crate) unsafe fn from_error<E: Into<Box<dyn std::error::Error>>>(error: E) -> Self {
        Self::new(error.into())
    }
}

pub(crate) unsafe fn handle_result<T, U: Default, E: Into<Box<dyn std::error::Error>>, F: FnOnce(T) -> U>(result: Result<T, E>, f: F, error: *mut Error) -> U {
    match result {
        Ok(value) => f(value),
        Err(e) => {
            *error = Error::from_error(e);
            Default::default()
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn sailar_dispose_error(error: Error) {
    Box::from_raw(error.0);
}

crate::box_wrapper!(ErrorMessage(pub String));

/// Allocates an object containing the `error`'s message. The `error` object should be disposed later using
/// `sailar_dispose_error_message`.
#[no_mangle]
pub unsafe extern "C" fn sailar_get_error_message(error: Error) -> ErrorMessage {
    ErrorMessage::new(error.into_ref().to_string())
}

#[no_mangle]
pub unsafe extern "C" fn sailar_get_error_message_contents(message: ErrorMessage, length: *mut usize) -> *const u8 {
    let message = message.into_ref();
    *length = message.len();
    message.as_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn sailar_dispose_error_message(message: ErrorMessage) {
    message.into_box();
}

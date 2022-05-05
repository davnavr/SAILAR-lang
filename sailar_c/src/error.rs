//! Error handling functions.

// Workaround since dyn makes a fat pointer
crate::box_wrapper!(Error, Box<Box<dyn std::error::Error>>);

impl Error {
    pub(crate) unsafe fn from_error<E: std::error::Error + 'static>(error: E) -> Self {
        Self::new(Box::new(Box::new(error)))
    }
}

#[no_mangle]
pub unsafe extern "C" fn sailar_dispose_error(error: Error) {
    Box::from_raw(error.0);
}

crate::box_wrapper!(ErrorMessage, String);

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

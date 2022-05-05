//! Error handling functions.

#[repr(transparent)]
pub struct Error(*mut std::ffi::c_void);

impl Error {
    pub unsafe fn new<E: std::error::Error + 'static>(error: E) -> Error {
        Error(Box::into_raw(Box::new(error)) as *mut _)
    }

    pub unsafe fn as_ref<'a>(self) -> &'a (dyn std::error::Error + 'static) {
        std::mem::transmute::<_, *mut (dyn std::error::Error + 'static)>(self.0).as_ref().expect("error must not be null")
    }
}

#[no_mangle]
pub unsafe extern "C" fn SAILARDisposeError(error: Error) {
    Box::from_raw(error.0);
}

crate::box_wrapper!(ErrorMessage, String);

#[no_mangle]
pub unsafe extern "C" fn SAILARGetErrorMessage(error: Error) -> ErrorMessage {
    ErrorMessage::new(error.as_ref().to_string())
}

#[no_mangle]
pub unsafe extern "C" fn SAILARGetErrorMessageContents(message: ErrorMessage, length: *mut usize) -> *const u8 {
    let message = message.as_ref();
    *length = message.len();
    message.as_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn SAILARDisposeErrorMessage(message: ErrorMessage) {
    message.into_box();
}

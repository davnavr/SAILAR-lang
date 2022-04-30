//! Error handling functions.

pub struct SAILErrorRef(*mut dyn std::error::Error);

impl SAILErrorRef {
    pub unsafe fn as_ref<'a>(self) -> &'a dyn std::error::Error {
        self.0.as_ref().expect("error must not be null")
    }
}

pub unsafe fn from_error<E: std::error::Error + 'static>(error: E) -> SAILErrorRef {
    SAILErrorRef(Box::into_raw(Box::new(error)))
}

#[no_mangle]
pub unsafe extern "C" fn SAILDisposeError(error: SAILErrorRef) {
    Box::from_raw(error.0);
}

crate::box_wrapper!(SAILErrorMessageRef, String, "error message must not be null");

#[no_mangle]
pub unsafe extern "C" fn SAILGetErrorMessage(error: SAILErrorRef) -> SAILErrorMessageRef {
    SAILErrorMessageRef::new(error.as_ref().to_string())
}

#[no_mangle]
pub unsafe extern "C" fn SAILGetErrorMessageContents(message: SAILErrorMessageRef, length: *mut usize) -> *const u8 {
    let message = message.as_ref();
    *length = message.len();
    message.as_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn SAILDisposeErrorMessage(message: SAILErrorMessageRef) {
    message.into_box();
}

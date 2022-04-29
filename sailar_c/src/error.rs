//! Error handling functions.

pub struct SAILErrorRef(*mut dyn std::error::Error);

pub unsafe fn from_error<E: std::error::Error + 'static>(error: E) -> SAILErrorRef {
    SAILErrorRef(Box::into_raw(Box::new(error)))
}

#[no_mangle]
pub unsafe extern "C" fn SAILDisposeError(error: SAILErrorRef) {
    Box::from_raw(error.0);
}

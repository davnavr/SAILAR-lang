//! Code for manipulating byte buffers.

crate::box_wrapper!(Buffer(pub Box<[u8]>));

#[no_mangle]
pub unsafe extern "C" fn sailar_create_buffer_from_address(address: *const u8, length: usize) -> Buffer {
    if address.is_null() {
        panic!("source address must not be null")
    }

    let source = if length == 0 {
        &[]
    } else {
        std::slice::from_raw_parts(address, length)
    };

    Buffer::new(Box::from(source))
}

#[no_mangle]
pub unsafe extern "C" fn sailar_dispose_buffer(buffer: Buffer) {
    buffer.into_box();
}

#[no_mangle]
pub unsafe extern "C" fn sailar_get_buffer_contents(buffer: Buffer, length: *mut usize) -> *mut u8 {
    if buffer.is_null() {
        *length = 0;
        std::ptr::null_mut()
    } else {
        let unwrapped = buffer.into_mut();
        *length = unwrapped.len();
        unwrapped.as_mut_ptr()
    }

}

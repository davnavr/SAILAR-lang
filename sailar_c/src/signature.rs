//! Functions for manipulating type and function signatures.

use sailar::binary::{index, signature};

crate::box_wrapper!(TypeSignature(pub signature::Type));

#[no_mangle]
pub unsafe extern "C" fn sailar_get_type_signature_kind(signature: TypeSignature) -> u8 {
    signature.into_box().code().into()
}

#[no_mangle]
pub unsafe extern "C" fn sailar_dispose_type_signature(signature: TypeSignature) {
    signature.into_box();
}

crate::box_wrapper!(FunctionSignature(pub signature::Function));

#[no_mangle]
pub unsafe extern "C" fn sailar_dispose_function_signature(signature: FunctionSignature) {
    signature.into_box();
}

#[no_mangle]
pub unsafe extern "C" fn sailar_get_function_signature_counts(
    signature: FunctionSignature,
    result_count: *mut usize,
    parameter_count: *mut usize,
) {
    let signature = signature.into_ref();
    *result_count = signature.return_types().len();
    *parameter_count = signature.parameter_types().len();
}

#[no_mangle]
pub unsafe extern "C" fn sailar_get_function_signature_return_type(
    signature: FunctionSignature,
    index: usize,
    return_type: *mut index::TypeSignature,
) -> bool {
    match signature.into_ref().return_types().get(index) {
        Some(type_index) => {
            *return_type = *type_index;
            true
        }
        None => false,
    }
}

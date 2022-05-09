//! Functions for manipulating type and function signatures.

use sailar::binary::signature;

crate::box_wrapper!(TypeSignature(pub signature::Type));

#[no_mangle]
pub unsafe extern "C" fn sailar_get_type_signature_kind(signature: TypeSignature) -> u8 {
    signature.into_box().code().into()
}

#[no_mangle]
pub unsafe extern "C" fn sailar_dispose_type_signature(signature: TypeSignature) {
    signature.into_box();
}

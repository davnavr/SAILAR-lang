//! Functions for interacting with SAILAR modules.

use crate::identifier::SAILIdentifierRef;

crate::box_wrapper!(SAILModuleRef, sailar::ModuleDefinition, "module must not be null");

#[no_mangle]
pub unsafe extern "C" fn SAILCreateModule(
    name: SAILIdentifierRef,
    version_numbers: *const usize,
    version_number_count: usize,
) -> SAILModuleRef {
    SAILModuleRef::new(sailar::ModuleDefinition::new(
        name.as_ref().clone(),
        std::slice::from_raw_parts(version_numbers, version_number_count),
    ))
}

#[no_mangle]
pub unsafe extern "C" fn SAILDisposeModule(module: SAILModuleRef) {
    module.into_box();
}

/// Prints the formatted contents of the module to standard error.
#[no_mangle]
pub unsafe extern "C" fn SAILDumpModuleDebug(module: SAILModuleRef) {
    eprintln!("{:?}", module.as_ref())
}

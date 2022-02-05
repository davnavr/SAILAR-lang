use crate::builder;
use crate::format;

pub mod function;

pub use function::Definitions as DefinedFunctions;

pub struct Definitions {
    functions: DefinedFunctions,
}

impl Definitions {
    pub(super) fn new() -> Self {
        Self {
            functions: DefinedFunctions::new(),
        }
    }

    pub fn functions(&self) -> &DefinedFunctions {
        &self.functions
    }

    pub(super) fn build(
        &self,
        identifiers: &mut builder::identifiers::Identifiers,
    ) -> format::ModuleDefinitions {
        format::ModuleDefinitions {
            defined_structs: format::LenBytes(format::LenVec(Vec::new())),
            defined_globals: format::LenBytes(format::LenVec(Vec::new())),
            defined_fields: format::LenBytes(format::LenVec(Vec::new())),
            defined_functions: format::LenBytes(format::LenVec(self.functions.build(identifiers))),
        }
    }
}

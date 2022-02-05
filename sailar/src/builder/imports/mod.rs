use crate::builder;
use crate::format;

pub mod function;
pub mod module;

pub use function::Imports as ImportedFunctions;
pub use module::Imports as ImportedModules;

pub struct Imports {
    modules: ImportedModules,
    functions: ImportedFunctions,
}

impl Imports {
    pub(super) fn new() -> Self {
        Self {
            modules: ImportedModules::new(),
            functions: ImportedFunctions::new(),
        }
    }

    pub fn modules(&self) -> &ImportedModules {
        &self.modules
    }

    pub fn functions(&self) -> &ImportedFunctions {
        &self.functions
    }

    pub(super) fn build(
        &self,
        identifiers: &mut builder::identifiers::Identifiers,
    ) -> format::ModuleImports {
        format::ModuleImports {
            imported_modules: format::LenBytes(format::LenVec(self.modules.build())),
            imported_structs: format::LenBytes(format::LenVec(Vec::new())),
            imported_globals: format::LenBytes(format::LenVec(Vec::new())),
            imported_fields: format::LenBytes(format::LenVec(Vec::new())),
            imported_functions: format::LenBytes(format::LenVec(self.functions.build(identifiers))),
        }
    }
}

use crate::format;

mod block;
mod code;
mod counter;
mod error;
mod type_signatures;

pub use block::{Block, Error as InvalidInstruction};
pub use code::{Code, Definitions as CodeDefinitions};
pub use error::{Error, Result};
pub use format::{FormatVersion, Identifier, VersionNumbers};
pub use type_signatures::{Signatures as TypeSignatures, Type};

pub struct Builder {
    identifier: format::ModuleIdentifier,
    format_version: FormatVersion,
    code: CodeDefinitions,
    type_signatures: TypeSignatures,
}

impl Builder {
    pub fn new(name: Identifier) -> Self {
        Self {
            identifier: format::ModuleIdentifier {
                name,
                version: VersionNumbers::default(),
            },
            format_version: FormatVersion::minimum_supported_version().clone(),
            code: CodeDefinitions::new(),
            type_signatures: TypeSignatures::new(),
        }
    }

    pub fn set_format_version(&mut self, version: FormatVersion) {
        self.format_version = version;
    }

    pub fn set_module_version(&mut self, version: VersionNumbers) {
        self.identifier.version = version;
    }

    pub fn code(&mut self) -> &mut CodeDefinitions {
        &mut self.code
    }

    pub fn type_signatures(&mut self) -> &mut TypeSignatures {
        &mut self.type_signatures
    }

    pub fn finish(mut self) -> format::Module {
        format::Module {
            integer_size: format::numeric::IntegerSize::I4,
            format_version: self.format_version,
            header: format::LenBytes(format::ModuleHeader {
                identifier: self.identifier,
            }),
            identifiers: format::LenBytes(format::LenVec(Vec::new())),
            namespaces: format::LenBytes(format::LenVec(Vec::new())),
            type_signatures: format::LenBytes(format::LenVec(Vec::new())),
            function_signatures: format::LenBytes(format::LenVec(Vec::new())),
            function_bodies: format::LenBytes(format::LenVec(self.code.build())),
            data: format::LenBytes(format::LenVec(Vec::new())),
            imports: format::LenBytes(format::ModuleImports {
                imported_modules: format::LenBytes(format::LenVec(Vec::new())),
                imported_structs: format::LenBytes(format::LenVec(Vec::new())),
                imported_globals: format::LenBytes(format::LenVec(Vec::new())),
                imported_fields: format::LenBytes(format::LenVec(Vec::new())),
                imported_functions: format::LenBytes(format::LenVec(Vec::new())),
            }),
            definitions: format::LenBytes(format::ModuleDefinitions {
                defined_structs: format::LenBytes(format::LenVec(Vec::new())),
                defined_globals: format::LenBytes(format::LenVec(Vec::new())),
                defined_fields: format::LenBytes(format::LenVec(Vec::new())),
                defined_functions: format::LenBytes(format::LenVec(Vec::new())),
            }),
            struct_layouts: format::LenBytes(format::LenVec(Vec::new())),
            entry_point: format::LenBytes(None),
        }
    }
}

impl std::fmt::Debug for Builder {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Builder")
            .field("identifier", &self.identifier)
            .field("format_version", &self.format_version)
            .finish()
    }
}

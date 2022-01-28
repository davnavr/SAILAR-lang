use crate::format;

mod block;
mod code;
mod error;

pub use block::Block;
pub use code::{Code, Definitions as CodeDefinitions};
pub use error::{Error, Result};
pub use format::{FormatVersion, Identifier, VersionNumbers};

pub struct Builder {
    module_name: Identifier,
    module_version: VersionNumbers,
    format_version: FormatVersion,
    code: CodeDefinitions,
}

impl Builder {
    pub fn new(name: Identifier) -> Self {
        Self {
            module_name: name,
            module_version: VersionNumbers::default(),
            format_version: FormatVersion::minimum_supported_version().clone(),
            code: CodeDefinitions::new(),
        }
    }

    pub fn set_format_version(&mut self, version: FormatVersion) {
        self.format_version = version;
    }

    pub fn set_module_version(&mut self, version: VersionNumbers) {
        self.module_version = version;
    }

    pub fn code(&mut self) -> &mut CodeDefinitions {
        &mut self.code
    }

    pub fn finish(self) -> format::Module {
        format::Module {
            integer_size: format::numeric::IntegerSize::I4,
            format_version: self.format_version,
            header: format::LenBytes(format::ModuleHeader {
                identifier: format::ModuleIdentifier {
                    name: self.module_name,
                    version: self.module_version,
                },
            }),
            identifiers: format::LenBytes(format::LenVec(Vec::new())),
            namespaces: format::LenBytes(format::LenVec(Vec::new())),
            type_signatures: format::LenBytes(format::LenVec(Vec::new())),
            function_signatures: format::LenBytes(format::LenVec(Vec::new())),
            function_bodies: format::LenBytes(format::LenVec(Vec::new())),
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

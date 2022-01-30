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

pub struct Setup {
    builder_identifier: Box<()>,
}

#[derive(Copy, Clone)]
#[repr(transparent)]
struct BuilderIdentifier<'b>(&'b ());

pub struct Builder<'b> {
    module_identifier: format::ModuleIdentifier,
    format_version: FormatVersion,
    code: CodeDefinitions<'b>,
    type_signatures: TypeSignatures<'b>,
}

impl<'b> Builder<'b> {
    pub fn new(setup: &'b mut Option<Setup>, name: Identifier) -> Self {
        let identifier = BuilderIdentifier(
            &setup
                .insert(Setup {
                    builder_identifier: Box::new(()),
                })
                .builder_identifier,
        );

        Self {
            module_identifier: format::ModuleIdentifier {
                name,
                version: format::VersionNumbers::default(),
            },
            format_version: FormatVersion::minimum_supported_version().clone(),
            code: CodeDefinitions::new(identifier),
            type_signatures: TypeSignatures::new(identifier),
        }
    }

    pub fn set_format_version(&'b mut self, version: FormatVersion) {
        self.format_version = version;
    }

    pub fn set_module_version(&'b mut self, version: VersionNumbers) {
        self.module_identifier.version = version;
    }

    pub fn code(&'b mut self) -> &'b mut CodeDefinitions {
        &mut self.code
    }

    pub fn type_signatures(&'b self) -> &'b TypeSignatures {
        &self.type_signatures
    }

    pub fn finish(&'b mut self) -> format::Module {
        format::Module {
            integer_size: format::numeric::IntegerSize::I4,
            format_version: self.format_version.clone(),
            header: format::LenBytes(format::ModuleHeader {
                identifier: self.module_identifier.clone(),
            }),
            identifiers: format::LenBytes(format::LenVec(Vec::new())),
            namespaces: format::LenBytes(format::LenVec(Vec::new())),
            type_signatures: format::LenBytes(format::LenVec(Vec::new())),
            function_signatures: format::LenBytes(format::LenVec(Vec::new())),
            function_bodies: format::LenBytes(format::LenVec(/* self.code.build() */ Vec::new())),
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

impl<'b> std::fmt::Debug for &'b Builder<'b> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Builder")
            .field("module_identifier", &self.module_identifier)
            .field("format_version", &self.format_version)
            .finish()
    }
}

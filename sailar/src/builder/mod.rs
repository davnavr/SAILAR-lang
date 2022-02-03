use crate::format;

mod block;
mod code;
mod counter;
mod definitions;
mod error;
mod type_signatures;

pub use block::{Block, Error as InvalidInstruction};
pub use code::{Code, Definitions as CodeDefinitions};

// pub use definitions::{
//     Builder as Definitions, Function as FunctionDefinition, FunctionBody,
//     FunctionBuilder as FunctionDefinitionBuilder, Functions as DefinedFunctions,
// };

pub use error::{Error, Result};
pub use format::{FormatVersion, VersionNumbers};
pub use type_signatures::{Signatures as TypeSignatures, Type};

// TODO: Add borrowed version of format::Identifier simialr to how &str is borrowed String
//pub type Name<'a> = std::borrow::Cow<'a, format::Identifier>;

pub struct Builder {
    //builder_identifier: Box<()>,
    module_identifier: format::ModuleIdentifier,
    format_version: FormatVersion,
    code: code::Definitions,
    type_signatures: std::rc::Rc<TypeSignatures>,
}

impl Builder {
    pub fn new(name: format::Identifier) -> Self {
        let type_signatures = std::rc::Rc::new(TypeSignatures::new());

        Self {
            //builder_identifier: Box::new(()),
            module_identifier: format::ModuleIdentifier {
                name,
                version: format::VersionNumbers::default(),
            },
            format_version: FormatVersion::minimum_supported_version().clone(),
            code: code::Definitions::new(type_signatures.clone()),
            type_signatures,
        }
    }

    pub fn format_version_mut(&mut self) -> &mut FormatVersion {
        &mut self.format_version
    }

    pub fn module_version_mut(&mut self) -> &mut VersionNumbers {
        &mut self.module_identifier.version
    }

    pub fn code(&self) -> &CodeDefinitions {
        &self.code
    }

    pub fn finish(mut self) -> format::Module {
        format::Module {
            integer_size: format::numeric::IntegerSize::I4,
            format_version: self.format_version,
            header: format::LenBytes(format::ModuleHeader {
                identifier: self.module_identifier,
            }),
            identifiers: format::LenBytes(format::LenVec(Vec::new())),
            namespaces: format::LenBytes(format::LenVec(Vec::new())),
            type_signatures: format::LenBytes(format::LenVec(self.type_signatures.build())),
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
            .field("module_identifier", &self.module_identifier)
            .field("format_version", &self.format_version)
            .finish()
    }
}

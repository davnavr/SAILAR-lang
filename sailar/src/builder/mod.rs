use crate::format;
use std::rc::Rc;

mod block;
mod code;
mod counter;
mod data;
mod definitions;
mod error;
mod function_signatures;
mod identifiers;
mod imports;
mod type_signatures;

pub use block::{Block, Error as InvalidInstruction};
pub use code::{Code, Definitions as CodeDefinitions};
pub use data::{Data, Definitions as DataDefinitions};
pub use definitions::function::{Body as FunctionBody, Function as FunctionDefinition};
pub use definitions::{DefinedFunctions, Definitions};
pub use error::{Error, Result};
pub use format::{FormatVersion, VersionNumbers};
pub use function_signatures::{Function as FunctionSig, Signatures as FunctionSignatures};
pub use imports::function::Function as FunctionImport;
pub use imports::module::Module as ModuleImport;
pub use imports::{ImportedFunctions, ImportedModules, Imports};
pub use type_signatures::{Signatures as TypeSignatures, Type};

// TODO: Add borrowed version of format::Identifier simialr to how &str is borrowed String
//pub type Name<'a> = std::borrow::Cow<'a, format::Identifier>;

macro_rules! definition_or_import {
    ($name: ident, $defined_type: ty, $imported_type: ty, $index_type: ty) => {
        #[derive(Clone, Debug)]
        pub enum $name {
            Defined(Rc<$defined_type>),
            Imported(Rc<$imported_type>),
        }

        impl Function {
            pub fn index(&self) -> $index_type {
                match self {
                    Self::Defined(definition) => <$index_type>::Defined(definition.index()),
                    Self::Imported(import) => <$index_type>::Imported(import.index()),
                }
            }
        }
    };
}

definition_or_import!(
    Function,
    FunctionDefinition,
    FunctionImport,
    format::indices::Function
);

impl Function {
    pub fn signature(&self) -> &FunctionSig {
        match self {
            Self::Defined(definition) => definition.signature(),
            Self::Imported(import) => import.signature(),
        }
    }
}

// Could have an enum for Module to refer to module import or current module.
//pub enum Module

pub struct Builder {
    //builder_identifier: Box<()>,
    module_identifier: format::ModuleIdentifier,
    format_version: FormatVersion,
    code: CodeDefinitions,
    data: DataDefinitions,
    imports: Imports,
    definitions: Definitions,
    function_signatures: Rc<FunctionSignatures>,
    type_signatures: Rc<TypeSignatures>,
    entry_point: Option<Rc<FunctionDefinition>>,
}

impl Builder {
    pub fn new(name: format::Identifier) -> Self {
        let type_signatures = Rc::new(TypeSignatures::new());
        let function_signatures = Rc::new(FunctionSignatures::new());

        Self {
            //builder_identifier: Box::new(()),
            module_identifier: format::ModuleIdentifier {
                name,
                version: format::VersionNumbers::default(),
            },
            format_version: FormatVersion::minimum_supported_version().clone(),
            code: CodeDefinitions::new(type_signatures.clone()),
            data: DataDefinitions::new(),
            imports: Imports::new(),
            definitions: Definitions::new(),
            function_signatures,
            type_signatures,
            entry_point: None,
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

    pub fn data(&self) -> &DataDefinitions {
        &self.data
    }

    pub fn type_signatures(&self) -> &TypeSignatures {
        &self.type_signatures
    }

    pub fn function_signatures(&self) -> &FunctionSignatures {
        &self.function_signatures
    }

    pub fn imports(&self) -> &Imports {
        &self.imports
    }

    pub fn definitions(&self) -> &Definitions {
        &self.definitions
    }

    pub fn set_entry_point(&mut self, entry_point: Rc<FunctionDefinition>) {
        self.entry_point = Some(entry_point);
    }

    pub fn finish(mut self) -> format::Module {
        let mut identifiers = identifiers::Identifiers::new();

        let code = self.code.build();
        let function_signatures = self.function_signatures.build();
        let type_signatures = self.type_signatures.build();
        let definitions = self.definitions.build(&mut identifiers);
        let imports = self.imports.build(&mut identifiers);

        format::Module {
            integer_size: format::numeric::IntegerSize::I4,
            format_version: self.format_version,
            header: format::LenBytes(format::ModuleHeader {
                identifier: self.module_identifier,
            }),
            identifiers: format::LenBytes(format::LenVec(identifiers.build())),
            namespaces: format::LenBytes(format::LenVec(Vec::new())),
            type_signatures: format::LenBytes(format::LenVec(type_signatures)),
            function_signatures: format::LenBytes(format::LenVec(function_signatures)),
            function_bodies: format::LenBytes(format::LenVec(code)),
            data: format::LenBytes(format::LenVec(self.data.build())),
            imports: format::LenBytes(imports),
            definitions: format::LenBytes(definitions),
            struct_layouts: format::LenBytes(format::LenVec(Vec::new())),
            entry_point: format::LenBytes(self.entry_point.map(|main| main.index())),
        }
    }

    pub fn write_to<W: std::io::Write>(self, output: &mut W) -> crate::writer::Result {
        crate::writer::write_module(&self.finish(), output)
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

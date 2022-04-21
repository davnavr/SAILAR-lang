//! Reading and writing of SAILAR modules.

use crate::binary::buffer;
use crate::binary::{LengthSize, RawModule};
use crate::function;
use crate::identifier::{Id, Identifier};
use std::collections::hash_map;
use std::fmt::{Debug, Formatter};
use std::sync::Arc;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum Export {
    Yes,
    No,
}

/// Specifies the version of a SAILAR module file.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
#[non_exhaustive]
pub struct FormatVersion {
    /// The major version number, incremented when backwards incompatible changes are made to the format.
    pub major: u8,
    pub minor: u8,
}

impl FormatVersion {
    /// The minimum version of the format supported by this API.
    pub const MINIMUM_SUPPORTED: &'static Self = &Self { major: 0, minor: 12 };
}

/// Used to help keep track of symbols in modules in order to avoid definitions with duplicate symbols.
#[derive(Clone)]
pub(crate) enum DefinedSymbol {
    Function(Arc<function::Template>),
}

impl DefinedSymbol {
    pub(crate) fn symbol(&self) -> &Id {
        match self {
            Self::Function(function) => function.function().symbol(),
        }
    }
}

impl std::cmp::PartialEq for DefinedSymbol {
    fn eq(&self, other: &Self) -> bool {
        self.symbol() == other.symbol()
    }
}

impl std::cmp::Eq for DefinedSymbol {}

impl std::hash::Hash for DefinedSymbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::hash::Hash::hash(self.symbol(), state)
    }
}

impl Debug for DefinedSymbol {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct(match self {
            Self::Function(_) => "Function",
        })
        .field("symbol", &self.symbol())
        .finish_non_exhaustive()
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct DefinedFunction {
    template: Arc<function::Template>,
    definition: function::Definition,
}

impl DefinedFunction {
    pub(crate) fn new(template: Arc<function::Template>, export: Export, body: function::Body) -> Self {
        Self {
            template,
            definition: function::Definition::new(body, export),
        }
    }

    #[inline]
    pub fn template(&self) -> &Arc<function::Template> {
        &self.template
    }

    #[inline]
    pub fn definition(&self) -> &function::Definition {
        &self.definition
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ImportedFunction {
    template: Arc<function::Template>,
}

impl ImportedFunction {
    #[inline]
    pub fn template(&self) -> &Arc<function::Template> {
        &self.template
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ModuleIdentifier {
    name: Identifier,
    version: Box<[usize]>,
}

impl ModuleIdentifier {
    pub fn new(name: Identifier, version: Box<[usize]>) -> Self {
        Self { name, version }
    }

    #[inline]
    pub fn name(&self) -> &Id {
        self.name.as_id()
    }

    /// Gets the numbers that specify the version of the module, used to disambiguate between modules with the same name.
    #[inline]
    pub fn version(&self) -> &[usize] {
        &self.version
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Module {
    Definition(Arc<ModuleIdentifier>),
    Import(Arc<Import>),
}

/// A module hash, which further disambiguates two modules with the same name and version numbers.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Hash {
    None,
    SHA256(Box<[u8; 256]>),
}

/// Represents a SAILAR module that was imported.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Import {
    identifier: Arc<ModuleIdentifier>,
    hash: Hash,
}

impl Import {
    pub fn from_identifier_with_hash(identifier: Arc<ModuleIdentifier>, hash: Hash) -> Self {
        Self { identifier, hash }
    }

    pub fn new_with_hash(name: Identifier, version: Box<[usize]>, hash: Hash) -> Self {
        Self::from_identifier_with_hash(Arc::new(ModuleIdentifier::new(name, version)), hash)
    }

    pub fn new(name: Identifier, version: Box<[usize]>) -> Self {
        Self::new_with_hash(name, version, Hash::None)
    }

    #[inline]
    pub fn identifier(&self) -> &Arc<ModuleIdentifier> {
        &self.identifier
    }

    #[inline]
    pub fn hash(&self) -> &Hash {
        &self.hash
    }
}

impl From<Arc<ModuleIdentifier>> for Import {
    fn from(identifier: Arc<ModuleIdentifier>) -> Self {
        Self::from_identifier_with_hash(identifier, Hash::None)
    }
}

impl From<ModuleIdentifier> for Import {
    fn from(identifier: ModuleIdentifier) -> Self {
        Self::from(Arc::new(identifier))
    }
}

pub(crate) type SymbolLookup = rustc_hash::FxHashMap<DefinedSymbol, ()>;

/// A SAILAR module.
pub struct Definition {
    contents: Option<RawModule>,
    format_version: FormatVersion,
    length_size: LengthSize,
    identifier: Arc<ModuleIdentifier>,
    symbols: SymbolLookup,
    function_definitions: Vec<DefinedFunction>,
    function_imports: Vec<ImportedFunction>,
    //function_instantiations: Vec<Arc<>>
    //entry_point: _,
}

mod parser;

pub use parser::{
    Error as ParseError, ErrorKind as ParseErrorKind, InvalidInstructionError as ParsedInstructionError,
    InvalidInstructionKind as ParsedInstructionErrorKind, InvalidMagicError,
};

mod writer;

impl From<Arc<ModuleIdentifier>> for Definition {
    fn from(identifier: Arc<ModuleIdentifier>) -> Self {
        let mut length_size = LengthSize::One;
        length_size.resize_to_fit(identifier.name.len());
        length_size.resize_to_fit(identifier.version.len());
        length_size.resize_to_fit_many(identifier.version.iter(), |n| *n);

        Self {
            contents: None,
            format_version: FormatVersion::MINIMUM_SUPPORTED.clone(),
            length_size,
            identifier,
            symbols: SymbolLookup::default(),
            function_definitions: Vec::new(),
            function_imports: Vec::new(),
        }
    }
}

impl From<ModuleIdentifier> for Definition {
    fn from(identifier: ModuleIdentifier) -> Self {
        Self::from(Arc::new(identifier))
    }
}

impl Definition {
    pub fn new(name: Identifier, version: Box<[usize]>) -> Self {
        Self::from(ModuleIdentifier::new(name, version))
    }

    #[inline]
    pub fn format_version(&self) -> &FormatVersion {
        &self.format_version
    }

    /// Gets a value indicating the size of length integers in the binary format of the module.
    #[inline]
    pub fn length_size(&self) -> LengthSize {
        self.length_size
    }

    /// Gets the module's identifier, which distinguishes one module from another.
    #[inline]
    pub fn identifier(&self) -> &Arc<ModuleIdentifier> {
        &self.identifier
    }

    /// Writes the bytes binary contents of the module to the specified destination.
    ///
    /// For writers such as [`std::fs::File`], consider wrapping the destination in a [`std::io::BufWriter`].
    pub fn write<W: std::io::Write>(&self, destination: W, buffer_pool: Option<&buffer::Pool>) -> std::io::Result<()> {
        writer::write(self, destination, buffer_pool)
    }

    /// Writes the binary contents of the module to a file, automatically wrapping it in a [`std::io::BufWriter`].
    pub fn write_to_file(&self, destination: std::fs::File, buffer_pool: Option<&buffer::Pool>) -> std::io::Result<()> {
        self.write(std::io::BufWriter::new(destination), buffer_pool)
    }

    /// Returns the binary contents of the module.
    ///
    /// # Examples
    ///
    /// ```
    /// # use sailar::{Identifier, ModuleDefinition};
    /// let mut module = ModuleDefinition::new(Identifier::from_str("Testing")?, vec![1, 2, 3].into_boxed_slice());
    /// let contents = module.raw_contents(None).bytes().to_vec();
    /// assert_eq!(sailar::binary::MAGIC.as_slice(), &contents[0..6]);
    /// let format_version = module.format_version();
    /// assert_eq!(&[ format_version.major, format_version.minor ], &contents[6..8]);
    /// assert_eq!(u8::from(sailar::binary::LengthSize::One), contents[8]);
    /// assert_eq!(12, contents[9]);
    /// assert_eq!(7u8, contents[10]); // Module name length
    /// assert_eq!(b"Testing", &contents[11..18]); // Module name
    /// assert_eq!(3u8, contents[18]); // Module version number count
    /// assert_eq!(&[ 1, 2, 3 ], &contents[19..22]); // Module version numbers
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    pub fn raw_contents(&mut self, buffer_pool: Option<&buffer::Pool>) -> &RawModule {
        if self.contents.is_none() {
            let mut module_buffer = buffer::RentedOrOwned::with_capacity(512, buffer_pool);

            if let Err(error) = Self::write(self, module_buffer.as_mut_vec(), buffer_pool) {
                unreachable!("unable to write module: {:?}", error)
            }

            self.contents.insert(RawModule::from_vec(module_buffer.into_vec()))
        } else if let Some(existing) = &self.contents {
            existing
        } else {
            unreachable!()
        }
    }

    //pub fn drop_raw_contents
    //pub fn take_raw_contents(&mut self) -> binary::RawModule

    /// Parses a module.
    ///
    /// For sources such as [`std::fs::File`], consider wrapping the reader in a [`std::io::BufReader`].
    #[inline]
    pub fn parse<R: std::io::Read>(source: R, buffer_pool: Option<&buffer::Pool>) -> Result<Self, ParseError> {
        parser::parse(source, buffer_pool)
    }

    /// Parses a module contained a byte slice.
    #[inline]
    pub fn from_slice(bytes: &[u8], buffer_pool: Option<&buffer::Pool>) -> Result<Self, ParseError> {
        Self::parse(bytes, buffer_pool)
    }

    /// Parses a module contained in the byte vector, and stores the bytes alongside the parsed [`Module`].
    ///
    /// The byte vector can be retrieved again by calling [`Module::raw_contents()`].
    pub fn from_vec(bytes: Vec<u8>, buffer_pool: Option<&buffer::Pool>) -> Result<Self, ParseError> {
        let mut module = Self::from_slice(&bytes, buffer_pool)?;
        module.contents = Some(crate::binary::RawModule::from_vec(bytes));
        Ok(module)
    }
}

impl TryFrom<Vec<u8>> for Definition {
    type Error = ParseError;

    #[inline]
    fn try_from(bytes: Vec<u8>) -> Result<Self, Self::Error> {
        Self::from_vec(bytes, None)
    }
}

#[derive(Clone, Debug)]
pub struct DuplicateSymbolError(DefinedSymbol);

impl DuplicateSymbolError {
    #[inline]
    pub fn symbol(&self) -> &Id {
        self.0.symbol()
    }
}

impl std::fmt::Display for DuplicateSymbolError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "a definition corresponding to the symbol \"{}\" already exists",
            self.symbol()
        )
    }
}

impl std::error::Error for DuplicateSymbolError {}

impl Definition {
    pub fn to_module(&self) -> Module {
        Module::Definition(self.identifier().clone())
    }

    fn length_size_fit_function(&mut self, function: &function::Function) {
        self.length_size.resize_to_fit(function.symbol().len());
        self.length_size.resize_to_fit(function.signature().result_types().len());
        self.length_size.resize_to_fit(function.signature().parameter_types().len());
    }

    pub fn define_function(
        &mut self,
        symbol: Identifier,
        signature: Arc<function::Signature>,
        definition: function::Definition,
    ) -> Result<Arc<function::Template>, DuplicateSymbolError> {
        let template = function::Template::new(symbol, signature, self.to_module());

        match self.symbols.entry(DefinedSymbol::Function(template.clone())) {
            hash_map::Entry::Vacant(vacant) => {
                vacant.insert(());
            }
            hash_map::Entry::Occupied(occupied) => return Err(DuplicateSymbolError(occupied.key().clone())),
        }

        if let function::Body::Foreign(ref foreign) = definition.body() {
            self.length_size.resize_to_fit(foreign.library_name().len());
            self.length_size.resize_to_fit(foreign.entry_point_name().len());
        }

        // TODO: Update length size for function instructions.

        self.function_definitions.push(DefinedFunction {
            template: template.clone(),
            definition,
        });

        self.length_size_fit_function(template.function());
        self.contents = None;
        Ok(template)
    }

    pub fn import_function(&mut self, module: Arc<Import>, symbol: Identifier, signature: Arc<function::Signature>) -> Arc<function::Template> {
        let template = function::Template::new(symbol, signature, Module::Import(module));
        self.function_imports.push(ImportedFunction { template: template.clone() });
        self.length_size_fit_function(template.function());
        self.contents = None;
        template
    }

    #[inline]
    pub fn function_definitions(&self) -> &[DefinedFunction] {
        &self.function_definitions
    }

    #[inline]
    pub fn function_imports(&self) -> &[ImportedFunction] {
        &self.function_imports
    }
}

impl std::fmt::Debug for Definition {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        struct SymbolLookupDebug<'a>(&'a SymbolLookup);

        impl Debug for SymbolLookupDebug<'_> {
            fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
                f.debug_set().entries(self.0.keys()).finish()
            }
        }

        f.debug_struct("Definition")
            .field("format_version", &self.format_version)
            .field("length_size", &self.length_size)
            .field("identifier", &self.identifier)
            .field("symbols", &SymbolLookupDebug(&self.symbols))
            .field("function_definitions", &self.function_definitions)
            .field("function_imports", &self.function_imports)
            .field("contents", &self.contents)
            .finish_non_exhaustive()
    }
}

impl std::cmp::PartialEq for Definition {
    /// Checks that the contents of two modules are roughly equivalent.
    fn eq(&self, other: &Self) -> bool {
        let compare_symbols = || {
            if self.symbols.len() != other.symbols.len() {
                return false;
            }

            for definition in self.symbols.keys() {
                match other.symbols.get_key_value(definition) {
                    Some((other_symbol, _)) => match (definition, other_symbol) {
                        (DefinedSymbol::Function(defined_function), DefinedSymbol::Function(other_function)) => {
                            if defined_function != other_function {
                                return false;
                            }
                        }
                    },
                    None => return false,
                }
            }

            true
        };

        self.format_version == other.format_version && self.identifier == other.identifier && compare_symbols()
    }
}

impl std::cmp::Eq for Definition {}

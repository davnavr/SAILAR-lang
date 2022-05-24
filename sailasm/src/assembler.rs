//! Provides functions for assembling SAILAR modules given an abstract syntax tree.

use crate::ast;
use crate::parser;
use sailar::binary;
use sailar::binary::record;
use sailar::binary::Builder;
use sailar::versioning;
use std::borrow::Cow;

#[derive(Clone, Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("symbol @{0} is defined more than once")]
    DuplicateSymbolDefinition(Box<str>),
    #[error(transparent)]
    UnresolvedReference(#[from] UnresolvedReferenceError),
    #[error("{0} format version number was already specified")]
    DuplicateFormatVersion(ast::FormatVersionKind),
    #[error("missing corresponding major format version number")]
    MissingMajorFormatVersion,
    #[error(transparent)]
    UnsupportedFormatVersion(#[from] versioning::UnsupportedFormatError),
    #[error("metadata field \"{0}\" is already defined")]
    DuplicateMetadataField(&'static str),
    #[error("type signature is recursive")]
    RecursiveTypeSignature,
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("{kind}")]
pub struct Error {
    kind: ErrorKind,
    location: Option<ast::LocationRange>,
}

impl Error {
    pub fn new<K: Into<ErrorKind>>(kind: K, location: Option<ast::LocationRange>) -> Self {
        Self {
            kind: kind.into(),
            location,
        }
    }

    pub fn with_location<K: Into<ErrorKind>, L: Into<ast::LocationRange>>(kind: K, location: L) -> Self {
        Self::new(kind, Some(location.into()))
    }

    #[inline]
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    #[inline]
    pub fn location(&self) -> Option<&ast::LocationRange> {
        self.location.as_ref()
    }
}

#[derive(Debug)]
enum FormatVersion {
    Unspecified,
    MajorOnly(u8),
    MinorOnly(u8),
    Full(versioning::Format),
}

impl TryFrom<FormatVersion> for versioning::Format {
    type Error = Error;

    fn try_from(version: FormatVersion) -> Result<Self, Error> {
        match version {
            FormatVersion::Unspecified => Ok(*versioning::SupportedFormat::MINIMUM),
            FormatVersion::MajorOnly(major) => Ok(Self::new(major, 0)),
            FormatVersion::MinorOnly(_) => Err(Error::new(ErrorKind::MissingMajorFormatVersion, None)),
            FormatVersion::Full(full) => Ok(full),
        }
    }
}

/// Ensures that no symbols are defined more than once.
#[derive(Default)]
struct SymbolSet<'s> {
    lookup: rustc_hash::FxHashSet<&'s sailar::Id>,
}

impl<'s> SymbolSet<'s> {
    fn insert(&mut self, symbol: &ast::Symbol<'s>) -> Result<(), Error> {
        let name = symbol.item();
        if self.lookup.insert(name) {
            Ok(())
        } else {
            Err(Error::with_location(
                ErrorKind::DuplicateSymbolDefinition(Box::from(name.as_str())),
                symbol.location().clone(),
            ))
        }
    }
}

#[derive(Clone, Debug)]
enum UnresolvedReferenceKind {
    Index(u32),
    Symbol(Box<str>),
}

trait NamedItem {
    fn item_name() -> &'static str;
}

impl NamedItem for TypeSignatureAssembler<'_, '_> {
    fn item_name() -> &'static str {
        "type signature"
    }
}

#[derive(Clone, Debug)]
pub struct UnresolvedReferenceError {
    item_name: &'static str,
    symbol: UnresolvedReferenceKind,
}

impl std::fmt::Display for UnresolvedReferenceError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "a {} corresponding to the ", self.item_name)?;
        match &self.symbol {
            UnresolvedReferenceKind::Index(index) => write!(f, "index {}", index)?,
            UnresolvedReferenceKind::Symbol(symbol) => write!(f, "symbol @{}", symbol)?,
        }
        write!(f, " could not be found")
    }
}

impl std::error::Error for UnresolvedReferenceError {}

struct SymbolMap<'s, T> {
    items: Vec<T>,
    lookup: rustc_hash::FxHashMap<&'s sailar::Id, usize>,
}

impl<'s, T> SymbolMap<'s, T> {
    fn push(&mut self, item: T) -> usize {
        let index = self.items.len();
        self.items.push(item);
        index
    }

    fn len(&self) -> usize {
        self.items.len()
    }

    fn insert(&mut self, symbol: Option<&'s sailar::Id>, item: T) -> usize {
        let index = self.push(item);
        if let Some(symbol) = symbol {
            self.lookup.insert(symbol, index);
        }
        index
    }

    fn insert_with_symbol(&mut self, symbol: Option<&ast::Symbol<'s>>, item: T) -> usize {
        self.insert(symbol.map(|s| *s.item()), item)
    }

    fn get_index_from_symbol(&self, symbol: &'s sailar::Id) -> Option<usize> {
        self.lookup.get(symbol).copied()
    }

    fn iter(&self) -> impl std::iter::ExactSizeIterator<Item = &T> {
        self.items.iter()
    }
}

impl<'s, T: NamedItem> SymbolMap<'s, T> {
    fn get_index_from_reference(&self, reference: &ast::Reference<'s>) -> Result<usize, Error> {
        let location;
        let result = match reference {
            ast::Reference::Index(index) => {
                location = index.location();
                usize::try_from(*index.item()).ok().filter(|i| *i < self.len())
            }
            ast::Reference::Symbol(symbol) => {
                location = symbol.location();
                self.get_index_from_symbol(symbol.item())
            }
        };

        result.ok_or_else(|| {
            Error::with_location(
                UnresolvedReferenceError {
                    item_name: T::item_name(),
                    symbol: match reference {
                        ast::Reference::Index(index) => UnresolvedReferenceKind::Index(*index.item()),
                        ast::Reference::Symbol(symbol) => UnresolvedReferenceKind::Symbol(Box::from(symbol.item().as_str())),
                    },
                },
                location.clone(),
            )
        })
    }
}

impl<T> Default for SymbolMap<'_, T> {
    fn default() -> Self {
        Self {
            items: Vec::default(),
            lookup: Default::default(),
        }
    }
}

// TODO: Allow deciding whether duplicate records should be removed and whether records should be in source order.

struct TypeSignatureAssembler<'t, 's> {
    symbol: &'t Option<ast::Symbol<'s>>,
    signature: &'t ast::TypeSignature<'s>,
    signature_location: &'t ast::LocationRange,
    references: Box<[&'t ast::Reference<'s>]>,
}

struct Directives<'t, 's> {
    format_version: FormatVersion,
    module_identifier: Option<(&'t sailar::Id, Box<[usize]>)>,
    symbols: SymbolSet<'s>,
    identifiers: SymbolMap<'s, &'t sailar::Id>,
    #[allow(clippy::borrowed_box)]
    data_arrays: SymbolMap<'s, &'t Box<[u8]>>,
    type_signatures: SymbolMap<'s, TypeSignatureAssembler<'t, 's>>,
    function_signatures: SymbolMap<'s, &'t ast::FunctionSignature<'s>>,
}

/// The first pass of the assembler, iterates through all directives and adds all unknown symbols to a table.
fn get_record_definitions<'t, 's>(errors: &mut Vec<Error>, input: &'t parser::Output<'s>) -> Directives<'t, 's> {
    let mut directives = Directives {
        format_version: FormatVersion::Unspecified,
        module_identifier: None,
        symbols: Default::default(),
        identifiers: Default::default(),
        data_arrays: Default::default(),
        type_signatures: Default::default(),
        function_signatures: Default::default(),
    };

    macro_rules! define_symbol {
        ($symbol: expr) => {
            if let Err(e) = directives.symbols.insert($symbol) {
                errors.push(e);
            }
        };
    }

    for directive in input.tree().iter() {
        match directive.item() {
            ast::Directive::Array => todo!("array record generation is not yet supported"),
            ast::Directive::Format(ast::FormatVersionKind::Major, major) => match directives.format_version {
                FormatVersion::Unspecified => directives.format_version = FormatVersion::MajorOnly(*major),
                FormatVersion::MinorOnly(minor) => {
                    directives.format_version = FormatVersion::Full(versioning::Format::new(*major, minor))
                }
                FormatVersion::MajorOnly(_) | FormatVersion::Full(_) => errors.push(Error::with_location(
                    ErrorKind::DuplicateFormatVersion(ast::FormatVersionKind::Major),
                    directive.location().clone(),
                )),
            },
            ast::Directive::Format(ast::FormatVersionKind::Minor, minor) => match directives.format_version {
                FormatVersion::Unspecified => directives.format_version = FormatVersion::MinorOnly(*minor),
                FormatVersion::MajorOnly(major) => {
                    directives.format_version = FormatVersion::Full(versioning::Format::new(major, *minor))
                }
                FormatVersion::MinorOnly(_) | FormatVersion::Full(_) => errors.push(Error::with_location(
                    ErrorKind::DuplicateFormatVersion(ast::FormatVersionKind::Minor),
                    directive.location().clone(),
                )),
            },
            ast::Directive::Metadata(metadata) => match metadata {
                ast::Metadata::Identifier(name, version_numbers) => match directives.module_identifier {
                    Some(_) => errors.push(Error::with_location(
                        ErrorKind::DuplicateMetadataField("id"),
                        directive.location().clone(),
                    )),
                    None => {
                        directives.module_identifier = Some((
                            name.item(),
                            version_numbers.iter().map(|v| usize::try_from(*v).unwrap()).collect(),
                        ))
                    }
                },
            },
            ast::Directive::Identifier(symbol, identifier) => {
                if let Some(symbol) = symbol {
                    define_symbol!(symbol);
                }

                directives.identifiers.insert_with_symbol(symbol.as_ref(), identifier.item());
            }
            ast::Directive::Data(symbol, data) => {
                if let Some(symbol) = symbol {
                    define_symbol!(symbol);
                }

                directives.data_arrays.insert_with_symbol(symbol.as_ref(), data.item());
            }
            ast::Directive::Signature(symbol, signature) => {
                if let Some(symbol) = symbol {
                    define_symbol!(symbol);
                }

                let location = signature.location();

                match signature.item() {
                    ast::Signature::Type(type_signature) => {
                        directives.type_signatures.insert_with_symbol(
                            symbol.as_ref(),
                            TypeSignatureAssembler {
                                symbol,
                                signature: type_signature,
                                signature_location: location,
                                references: match type_signature {
                                    ast::TypeSignature::RawPtr(pointee_type) => Box::from([pointee_type]),
                                    _ => Box::default(),
                                },
                            },
                        );
                    }
                    ast::Signature::Function(function_signature) => {
                        directives
                            .function_signatures
                            .insert_with_symbol(symbol.as_ref(), function_signature);
                    }
                }
            }
        }
    }

    directives
}

/// The second pass of the assembler, produces record definitions in the module for every directive.
fn assemble_directives<'t, 's>(errors: &mut Vec<Error>, mut directives: Directives<'t, 's>) -> Builder<'t> {
    let format_version = match versioning::Format::try_from(directives.format_version) {
        Ok(version) => version,
        Err(e) => {
            errors.push(e);
            *versioning::SupportedFormat::MINIMUM
        }
    };

    let actual_format_version = match versioning::SupportedFormat::try_from(format_version) {
        Ok(version) => version,
        Err(e) => {
            errors.push(Error::new(e, None));
            versioning::SupportedFormat::MINIMUM
        }
    };

    let mut builder = Builder::with_format_version(actual_format_version);

    if let Some((name, version_numbers)) = directives.module_identifier.take() {
        builder.add_record(record::Record::MetadataField(record::MetadataField::ModuleIdentifier {
            name: Cow::Borrowed(name),
            version: sailar::helper::borrow::CowBox::Boxed(version_numbers),
        }))
    }

    for id in directives.identifiers.iter() {
        builder.add_record(record::Record::Identifier(Cow::Borrowed(id)))
    }

    for data in directives.data_arrays.iter() {
        builder.add_record(record::Record::Data(Cow::Borrowed(record::DataArray::from_bytes(data))));
    }

    for (index, assembler) in (0u32..).zip(directives.type_signatures.iter()) {
        for reference in assembler.references.iter().copied() {
            match reference {
                ast::Reference::Index(i) => {
                    if *i.item() == index {
                        errors.push(Error::new(
                            ErrorKind::RecursiveTypeSignature,
                            Some(assembler.signature_location.clone()),
                        ));
                    }
                }
                ast::Reference::Symbol(symbol) => match assembler.symbol {
                    Some(self_symbol) if symbol.item() == self_symbol.item() => errors.push(Error::new(
                        ErrorKind::RecursiveTypeSignature,
                        Some(assembler.signature_location.clone()),
                    )),
                    _ => (),
                },
            }
        }

        let signature = match assembler.signature {
            ast::TypeSignature::U8 => binary::signature::Type::U8,
            ast::TypeSignature::S8 => binary::signature::Type::S8,
            ast::TypeSignature::U16 => binary::signature::Type::U16,
            ast::TypeSignature::S16 => binary::signature::Type::S16,
            ast::TypeSignature::U32 => binary::signature::Type::U32,
            ast::TypeSignature::S32 => binary::signature::Type::S32,
            ast::TypeSignature::U64 => binary::signature::Type::U64,
            ast::TypeSignature::S64 => binary::signature::Type::S64,
            ast::TypeSignature::UAddr => binary::signature::Type::UAddr,
            ast::TypeSignature::SAddr => binary::signature::Type::SAddr,
            ast::TypeSignature::F32 => binary::signature::Type::F32,
            ast::TypeSignature::F64 => binary::signature::Type::F64,
            ast::TypeSignature::VoidPtr => binary::signature::Type::RawPtr(None),
            ast::TypeSignature::RawPtr(pointee) => match directives.type_signatures.get_index_from_reference(pointee) {
                Ok(index) => binary::signature::Type::RawPtr(Some(binary::index::TypeSignature::from(index))),
                Err(e) => {
                    errors.push(e);
                    continue;
                }
            },
        };

        builder.add_record(record::Record::TypeSignature(Cow::Owned(signature)));
    }

    let get_type_signature_indices =
        |references: &[ast::Reference<'s>], indices: &mut Vec<binary::index::TypeSignature>, errors: &mut Vec<Error>| {
            let mut failed = false;
            for r in references.iter() {
                match directives.type_signatures.get_index_from_reference(r) {
                    Ok(index) if !failed => indices.push(binary::index::TypeSignature::try_from(index).unwrap()),
                    Ok(_) => (),
                    Err(e) => {
                        errors.push(e);
                        failed = true;
                    }
                }
            }
        };

    for signature in directives.function_signatures.iter() {
        let return_types = signature.return_types();
        let parameter_types = signature.parameter_types();
        let mut indices = Vec::with_capacity(return_types.len() + parameter_types.len());
        get_type_signature_indices(return_types, &mut indices, errors);
        get_type_signature_indices(parameter_types, &mut indices, errors);
        builder.add_record(record::Record::FunctionSignature(Cow::Owned(
            binary::signature::Function::from_boxed_slice(indices.into_boxed_slice(), return_types.len()),
        )));
    }

    builder
}

/// Assembles a SAILAR module from an abstract syntax tree.
pub fn assemble<'tree, 'source: 'tree>(input: &'tree parser::Output<'source>) -> Result<Builder<'tree>, Vec<Error>> {
    let mut errors = Vec::default();
    let directives = get_record_definitions(&mut errors, input);
    let module = assemble_directives(&mut errors, directives);

    if errors.is_empty() {
        Ok(module)
    } else {
        Err(errors)
    }
}

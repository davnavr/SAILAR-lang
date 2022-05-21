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
    #[error("{0} format version number was already specified")]
    DuplicateFormatVersion(ast::FormatVersionKind),
    #[error("missing corresponding major format version number")]
    MissingMajorFormatVersion,
    #[error(transparent)]
    UnsupportedFormatVersion(#[from] versioning::UnsupportedFormatError),
    #[error("metadata field \"{0}\" is already defined")]
    DuplicateMetadataField(&'static str),
    #[error("symbol @{0} is defined more than once")]
    DuplicateSymbolDefinition(Box<str>),
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

type FxIndexMap<K, V> = indexmap::map::IndexMap<K, V, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;

struct SymbolMap<'t, T> {
    items: FxIndexMap<&'t sailar::Id, T>,
}

// TODO: Allow deciding whether duplicate records should be removed and whether records should be in source order.

#[derive(Debug)]
struct Directives<'t> {
    format_version: FormatVersion,
    module_identifier: Option<(&'t sailar::Id, Box<[usize]>)>,
    /// Ensures that no symbols are defined more than once.
    symbols: rustc_hash::FxHashSet<&'t sailar::Id>,
    identifiers: Vec<&'t sailar::Id>,
    data_arrays: Vec<&'t Box<[u8]>>,
    type_signatures: Vec<binary::signature::Type>, // TODO: This should contain a struct that helps resolve any symbols to structs.
}

/// The first pass of the assembler, iterates through all directives and adds all unknown symbols to a table.
fn get_record_definitions<'t>(errors: &mut Vec<Error>, input: &'t parser::Output) -> Directives<'t> {
    let mut directives = Directives {
        format_version: FormatVersion::Unspecified,
        module_identifier: None,
        symbols: Default::default(),
        identifiers: Vec::default(),
        data_arrays: Vec::default(),
        type_signatures: Vec::default(),
    };

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
                if symbol.is_some() {
                    todo!("identifier symbols not yet supported");
                }

                directives.identifiers.push(identifier.item());
            }
            ast::Directive::Data(symbol, data) => {
                if symbol.is_some() {
                    todo!("data symbols not yet supported");
                }

                directives.data_arrays.push(data.item());
            }
            ast::Directive::Signature(symbol, ast::Signature::Type(type_signature)) => {
                if symbol.is_some() {
                    todo!("type signature symbols not yet supported");
                }

                match type_signature {
                    ast::TypeSignature::Primitive(ast::PrimitiveType::Int(ast::IntegerType::Fixed(fixed_integer))) => {
                        directives.type_signatures.push(match fixed_integer {
                            ast::FixedIntegerType::U8 => binary::signature::Type::U8,
                            ast::FixedIntegerType::S8 => binary::signature::Type::S8,
                            ast::FixedIntegerType::U16 => binary::signature::Type::U16,
                            ast::FixedIntegerType::S16 => binary::signature::Type::S16,
                            ast::FixedIntegerType::U64 => binary::signature::Type::U64,
                            ast::FixedIntegerType::S64 => binary::signature::Type::S64,
                            ast::FixedIntegerType::U32 => binary::signature::Type::U32,
                            ast::FixedIntegerType::S32 => binary::signature::Type::S32,
                        });
                    }
                    _ => todo!("add support for {:?}", type_signature),
                }
            }
        }
    }

    directives
}

/// The second pass of the assembler, produces record definitions in the module for every directive.
fn assemble_directives<'t>(errors: &mut Vec<Error>, mut directives: Directives<'t>) -> Builder<'t> {
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

    for id in directives.identifiers.into_iter() {
        builder.add_record(record::Record::Identifier(Cow::Borrowed(id)))
    }

    for data in directives.data_arrays.into_iter() {
        builder.add_record(record::Record::Data(Cow::Borrowed(record::DataArray::from_bytes(data))));
    }

    for signature in directives.type_signatures.into_iter() {
        builder.add_record(record::Record::TypeSignature(Cow::Owned(signature)));
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

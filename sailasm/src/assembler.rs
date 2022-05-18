//! Provides functions for assembling SAILAR modules given an abstract syntax tree.

use crate::ast;
use crate::parser;
use sailar::binary::Builder;
use sailar::versioning;

#[derive(Clone, Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("{0} format version number was already specified")]
    DuplicateFormatVersion(ast::FormatVersionKind),
    #[error("missing corresponding major format version number")]
    MissingMajorFormatVersion,
    #[error(transparent)]
    UnsupportedFormatVersion(#[from] versioning::UnsupportedFormatError),
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
enum FormatVersion<'t> {
    Unspecified,
    MajorOnly(u8, &'t ast::LocationRange),
    MinorOnly(u8, &'t ast::LocationRange),
    Full(versioning::Format, &'t ast::LocationRange, &'t ast::LocationRange),
}

impl TryFrom<FormatVersion<'_>> for versioning::Format {
    type Error = Error;

    fn try_from(version: FormatVersion) -> Result<Self, Error> {
        match version {
            FormatVersion::Unspecified => Ok(*versioning::SupportedFormat::MINIMUM),
            FormatVersion::MajorOnly(major, _) => Ok(Self::new(major, 0)),
            FormatVersion::MinorOnly(_, _) => Err(Error::new(ErrorKind::MissingMajorFormatVersion, None)),
            FormatVersion::Full(full, _, _) => Ok(full),
        }
    }
}

//struct SymbolMap<T> {
//    items: Vec<T>,
//}

#[derive(Debug)]
struct Directives<'t> {
    format_version: FormatVersion<'t>,
    identifiers: Vec<Cow<'t, sailar::Id>>,
}

/// The first pass of the assembler, iterates through all directives and adds all unknown symbols to a table.
fn get_record_definitions<'t>(errors: &mut Vec<Error>, input: &'t parser::Output) -> Directives<'t> {
    let mut directives = Directives {
        format_version: FormatVersion::Unspecified,
        identifiers: Default::default(),
    };

    for directive in input.tree().iter() {
        match directive.node() {
            ast::Directive::Format(ast::FormatVersionKind::Major, major) => match directives.format_version {
                FormatVersion::Unspecified => directives.format_version = FormatVersion::MajorOnly(*major, directive.location()),
                FormatVersion::MinorOnly(minor, minor_location) => {
                    directives.format_version =
                        FormatVersion::Full(versioning::Format::new(*major, minor), directive.location(), minor_location)
                }
                FormatVersion::MajorOnly(_, location) | FormatVersion::Full(_, location, _) => errors.push(Error::with_location(
                    ErrorKind::DuplicateFormatVersion(ast::FormatVersionKind::Major),
                    location.clone(),
                )),
            },
            ast::Directive::Format(ast::FormatVersionKind::Minor, minor) => match directives.format_version {
                FormatVersion::Unspecified => directives.format_version = FormatVersion::MinorOnly(*minor, directive.location()),
                FormatVersion::MajorOnly(major, major_location) => {
                    directives.format_version =
                        FormatVersion::Full(versioning::Format::new(major, *minor), major_location, directive.location())
                }
                FormatVersion::MinorOnly(_, location) | FormatVersion::Full(_, _, location) => errors.push(Error::with_location(
                    ErrorKind::DuplicateFormatVersion(ast::FormatVersionKind::Minor),
                    location.clone(),
                )),
            },
            ast::Directive::Identifier(symbol, identifier) => {
                if symbol.is_some() {
                    todo!("identifier symbols not yet supported");
                }

                directives.identifiers.push(identifier);
            }
            _ => todo!("assemble {:?}", directive),
        }
    }

    directives
}

/// The second pass of the assembler, produces record definitions in the module for every directive.
fn assemble_directives<'s>(errors: &mut Vec<Error>, directives: Directives) -> Builder<'s> {
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

    // TODO: Build the module.

    builder
}

/// Assembles a SAILAR module from an abstract syntax tree.
pub fn assemble<'s>(input: &parser::Output<'s>) -> Result<Builder<'s>, Vec<Error>> {
    let mut errors = Vec::default();
    let directives = get_record_definitions(&mut errors, input);
    let module = assemble_directives(&mut errors, directives);

    if errors.is_empty() {
        Ok(module)
    } else {
        Err(errors)
    }
}

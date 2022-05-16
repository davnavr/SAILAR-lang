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
#[error("{location}: kind")]
pub struct Error {
    kind: ErrorKind,
    location: ast::LocationRange,
}

impl Error {
    pub fn new<K: Into<ErrorKind>, L: Into<ast::LocationRange>>(kind: K, location: L) -> Self {
        Self {
            kind: kind.into(),
            location: location.into(),
        }
    }

    #[inline]
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    #[inline]
    pub fn location(&self) -> &ast::LocationRange {
        &self.location
    }
}

#[derive(Debug)]
enum FormatVersion<'t> {
    Unspecified,
    MajorOnly(u8, &'t ast::LocationRange),
    MinorOnly(u8, &'t ast::LocationRange),
    Full(versioning::Format),
}

impl TryFrom<FormatVersion<'_>> for versioning::Format {
    type Error = Error;

    fn try_from(version: FormatVersion) -> Result<Self, Error> {
        match version {
            FormatVersion::Unspecified => Ok(*versioning::SupportedFormat::MINIMUM),
            FormatVersion::MajorOnly(major, _) => Ok(Self::new(major, 0)),
            FormatVersion::MinorOnly(_, location) => Err(Error::new(ErrorKind::MissingMajorFormatVersion, location.clone())),
            FormatVersion::Full(full) => Ok(full),
        }
    }
}

#[derive(Debug)]
struct Directives<'t> {
    format_version: FormatVersion<'t>,
}

/// The first pass of the assembler, iterates through all directives and adds all unknown symbols to a table.
fn get_record_definitions<'t>(errors: &mut Vec<Error>, input: &'t parser::Output) -> Directives<'t> {
    let mut directives = Directives {
        format_version: FormatVersion::Unspecified,
    };

    for directive in input.tree().iter() {
        match directive.node() {
            ast::Directive::Format(ast::FormatVersionKind::Major, major) => match directives.format_version {
                FormatVersion::Unspecified => directives.format_version = FormatVersion::MajorOnly(*major, directive.location()),
                FormatVersion::MinorOnly(minor, _) => {
                    directives.format_version = FormatVersion::Full(versioning::Format::new(*major, minor))
                }
                FormatVersion::MajorOnly(_, location) => errors.push(Error::new(
                    ErrorKind::DuplicateFormatVersion(ast::FormatVersionKind::Major),
                    location.clone(),
                )),
                FormatVersion::Full(_) => todo!("what location to use when full version conflits with a new .format major node?"),
            },
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
        Err(_) => todo!("what location to use for unsupported format version?"),
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

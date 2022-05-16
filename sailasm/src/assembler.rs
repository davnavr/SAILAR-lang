//! Provides functions for assembling SAILAR modules given an abstract syntax tree.

use crate::ast;
use crate::parser;
use sailar::binary::Builder;

#[derive(Clone, Debug)]
pub enum ErrorKind {
    #[error("{0} format version number was already specified")]
    DuplicateFormatVersion(ast::FormatVersionKind),
    #[error("missing corresponding major format version number")]
    MissingMajorFormatVersion,
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
            kind: Box::new(kind.into()),
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
enum FormatVersion {
    Unspecified,
    MajorOnly(u8),
    MinorOnly(ast::LocationRange),
    Full(versioning::Format),
}

impl TryFrom<FormatVersion> for versioning::Format {
    type Error = Error;

    fn try_from(version: FormatVersion) -> Result<Self, Error> {
        match version {
            FormatVersion::Unspecified => Ok(Self::MINIMUM_SUPPORTED),
            FormatVersion::MajorOnly(major) => Ok(Self::new(major, 0)),
            FormatVersion::MinorOnly(location) => Err(Error::new(ErrorKind::MissingMajorFormatVersion, location)),
        }
    }
}

#[derive(Debug)]
struct Directives {
    format_version: FormatVersion,
}

/// The first pass of the assembler, iterates through all directives and adds all unknown symbols to a table.
fn get_record_definitions(errors: &mut Vec<Error>, input: &parser::Output) -> Directives {
    let mut directives = {
        format_version: FormatVersion::Unspecified,
    };

    directives
}

/// The second pass of the assembler, produces record definitions in the module for every directive.
fn assemble_directives(errors: &mut Vec<Error>, directives: Directives) -> Builder<'s> {

}

/// Assembles a SAILAR module from an abstract syntax tree.
pub fn assemble<'s>(input: &parser::Output<'s>) -> Result<Builder<'s>, Vec<Error>> {
    let mut errors = Vec::default();
    let directives = get_record_definitions(&mut errors, input);
    let module = assemble_definitions(&mut errors, directives);

    if errors.is_empty() {
        Ok(module)
    } else {
        Err(errors)
    }
}

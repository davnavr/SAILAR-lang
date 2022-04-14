//! Reading and writing of SAILAR modules.

use crate::binary::{self, buffer};
use std::cell::RefCell;

/// Specifies the version of a SAILAR module file.
#[derive(Debug)]
#[non_exhaustive]
pub struct FormatVersion {
    /// The major version number, incremented when backwards incompatible changes are made to the format.
    pub major: u8,
    pub minor: u8,
}

/// A SAILAR module.
#[derive(Debug)]
pub struct Module {
    contents: Option<binary::RawModule>,
    format_version: FormatVersion,
}

impl Module {
    pub fn format_version(&self) -> &FormatVersion {
        &self.format_version
    }

    pub fn raw_contents(&mut self, buffer_pool: Option<&mut buffer::Pool>) -> &binary::RawModule {
        self.contents.get_or_insert_with(|| {
            let buffer = buffer::RentedOrOwned::with_capacity(512, buffer_pool);

            todo!("create the raw contents")
        })
    }
}

#[derive(Clone, Debug)]
pub struct InvalidMagicError {
    actual: Vec<u8>,
}

impl std::fmt::Display for InvalidMagicError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "expected magic {:?}, but got {:?}",
            buffer::ByteDebug::from(binary::MAGIC),
            buffer::ByteDebug::from(&self.actual),
        )
    }
}

impl std::error::Error for InvalidMagicError {}

#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ParseErrorKind {
    #[error(transparent)]
    InvalidMagic(#[from] InvalidMagicError),
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("error at offset {offset:#X}, {kind}")]
pub struct ParseError {
    offset: usize,
    kind: ParseErrorKind,
}

impl ParseError {
    /// A byte offset into the module file indicating where the error occured.
    pub fn offset(&self) -> usize {
        self.offset
    }

    /// The kind of error that occured.
    pub fn kind(&self) -> &ParseErrorKind {
        &self.kind
    }
}

impl TryFrom<Vec<u8>> for Module {
    type Error = ParseError;

    fn try_from(bytes: Vec<u8>) -> Result<Self, Self::Error> {
        todo!("implement parsing")
    }
}

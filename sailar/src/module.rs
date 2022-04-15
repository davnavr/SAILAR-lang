//! Reading and writing of SAILAR modules.

use crate::binary::{self, buffer};
use std::cell::RefCell;

/// Specifies the version of a SAILAR module file.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct FormatVersion {
    /// The major version number, incremented when backwards incompatible changes are made to the format.
    pub major: u8,
    pub minor: u8,
}

impl FormatVersion {
    // static
    /// The minimum version of the format supported by this API.
    pub const MINIMUM_SUPPORTED: Self = Self {
        major: 0,
        minor: 12,
    };
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

    /// Writes the bytes that make up this module to the specified destination.
    ///
    /// For writers such as [`std::io::File`], consider wrapping the destination in a [`std::io::BufWriter`].
    pub fn write<W: std::io::Write>(
        &self,
        destination: W,
        buffer_pool: Option<&buffer::Pool>,
    ) -> std::io::Result<()> {
        let mut out = destination;
        let buffer_pool = buffer::Pool::existing_or_default(buffer_pool);
        out.write_all(binary::MAGIC.as_slice())?;
        out.write_all(&[self.format_version.major, self.format_version.minor])?;
        todo!("create the raw contents");
        out.flush()
    }

    pub fn raw_contents(&mut self, buffer_pool: Option<&buffer::Pool>) -> &binary::RawModule {
        match &self.contents {
            Some(contents) => contents,
            None => {}
        }
    }

    //pub fn drop_raw_contents
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

impl Module {
    /// Parses a module.
    ///
    /// For sources such as [`std::io::File`], consider wrapping the reader in a [`std::io::BufReader`].
    pub fn parse<R: std::io::Read>(
        mut source: R,
        buffer_pool: Option<&buffer::Pool>,
    ) -> Result<Self, ParseError> {
        let buffer_pool = buffer::Pool::existing_or_default(buffer_pool);

        // TODO: Read first 6 bytes and store them in a buffer.
        todo!("implement parsing");

        //# TODO: Store bytes in contents field
    }

    /// Parses a module contained a byte slice.
    pub fn from_slice(
        bytes: &[u8],
        buffer_pool: Option<&buffer::Pool>,
    ) -> Result<Self, ParseError> {
        Self::parse(bytes, buffer_pool)
    }

    /// Parses a module contained in the byte vector, and stores the bytes alongside the parsed [`Module`].
    ///
    /// The byte vector can be retrieved again by calling [`raw_contents()`].
    pub fn from_vec(
        bytes: Vec<u8>,
        buffer_pool: Option<&buffer::Pool>,
    ) -> Result<Self, ParseError> {
        let mut module = Self::from_slice(&bytes, buffer_pool)?;
        module.contents = Some(binary::RawModule::from_vec(bytes));
        Ok(module)
    }
}

impl TryFrom<Vec<u8>> for Module {
    type Error = ParseError;

    fn try_from(bytes: Vec<u8>) -> Result<Self, Self::Error> {
        Self::from_vec(bytes, None)
    }
}

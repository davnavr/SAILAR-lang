//! Reading and writing of SAILAR modules.

use crate::binary::buffer;

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
    pub const MINIMUM_SUPPORTED: &'static Self = &Self {
        major: 0,
        minor: 12,
    };
}

/// A SAILAR module.
#[derive(Debug)]
pub struct Module {
    contents: Option<crate::binary::RawModule>,
    format_version: FormatVersion,
    //length_size:
}

mod parser;

pub use parser::{Error as ParseError, ErrorKind as ParseErrorKind, InvalidMagicError};

mod writer;

impl Module {
    pub fn format_version(&self) -> &FormatVersion {
        &self.format_version
    }

    /// Writes the bytes that make up this module to the specified destination.
    ///
    /// For writers such as [`std::fs::File`], consider wrapping the destination in a [`std::io::BufWriter`].
    pub fn write<W: std::io::Write>(
        &self,
        destination: W,
        buffer_pool: Option<&buffer::Pool>,
    ) -> std::io::Result<()> {
        writer::write(self, destination, buffer_pool)
    }

    pub fn raw_contents(
        &mut self,
        buffer_pool: Option<&buffer::Pool>,
    ) -> &crate::binary::RawModule {
        if self.contents.is_none() {
            let mut module_buffer = buffer::RentedOrOwned::with_capacity(512, buffer_pool);

            if let Err(error) = Self::write(self, module_buffer.as_mut_slice(), buffer_pool) {
                unreachable!("unable to write module: {:?}", error)
            }

            self.contents
                .insert(crate::binary::RawModule::from_vec(module_buffer.into_vec()))
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
    pub fn parse<R: std::io::Read>(
        source: R,
        buffer_pool: Option<&buffer::Pool>,
    ) -> Result<Self, ParseError> {
        parser::parse(source, buffer_pool)
    }

    /// Parses a module contained a byte slice.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use sailar::module::{FormatVersion, Module};
    /// let contents = &[ b'S', b'A', b'I', b'L', b'A', b'R', 0, 0, 12, 0 ];
    /// let module = Module::from_slice(contents, None)?;
    /// assert_eq!(module.format_version(), FormatVersion::MINIMUM_SUPPORTED);
    /// # Ok::<(), sailar::module::ParseError>(())
    /// ```
    pub fn from_slice(
        bytes: &[u8],
        buffer_pool: Option<&buffer::Pool>,
    ) -> Result<Self, ParseError> {
        Self::parse(bytes, buffer_pool)
    }

    /// Parses a module contained in the byte vector, and stores the bytes alongside the parsed [`Module`].
    ///
    /// The byte vector can be retrieved again by calling [`Module::raw_contents()`].
    pub fn from_vec(
        bytes: Vec<u8>,
        buffer_pool: Option<&buffer::Pool>,
    ) -> Result<Self, ParseError> {
        let mut module = Self::from_slice(&bytes, buffer_pool)?;
        module.contents = Some(crate::binary::RawModule::from_vec(bytes));
        Ok(module)
    }
}

impl TryFrom<Vec<u8>> for Module {
    type Error = ParseError;

    fn try_from(bytes: Vec<u8>) -> Result<Self, Self::Error> {
        Self::from_vec(bytes, None)
    }
}

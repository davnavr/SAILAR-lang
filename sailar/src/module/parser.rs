//! Code for parsing SAILAR modules.

use crate::binary::{self, buffer};

/// Used when an invalid magic value used to indicate the start of a SAILAR module is invalid.
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

#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ErrorKind {
    #[error(transparent)]
    InvalidMagic(#[from] InvalidMagicError),
    #[error("expected format version but got EOF")]
    MissingFormatVersion,
    #[error("expected length size value but got EOF")]
    MissingLengthSize,
    #[error(transparent)]
    InvalidLengthSize(#[from] binary::InvalidLengthSize),
    #[error(transparent)]
    IO(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
#[error("error at offset {offset:#X}, {kind}")]
pub struct Error {
    offset: usize,
    kind: ErrorKind,
}

impl Error {
    /// A byte offset into the module file indicating where the error occured.
    pub fn offset(&self) -> usize {
        self.offset
    }

    /// The kind of error that occured.
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }
}

pub fn parse<R: std::io::Read>(
    source: R,
    buffer_pool: Option<&buffer::Pool>,
) -> Result<crate::module::Module, Error> {
    #[derive(Debug)]
    struct Wrapper<R> {
        source: R,
        offset: usize,
    }

    impl<R: std::io::Read> Wrapper<R> {
        fn error(&self, error: ErrorKind) -> Error {
            Error {
                offset: self.offset,
                kind: error,
            }
        }

        fn read(&mut self, buf: &mut [u8]) -> Result<usize, Error> {
            match self.source.read(buf) {
                Ok(count) => {
                    self.offset += count;
                    Ok(count)
                }
                Err(error) => Err(self.error(ErrorKind::IO(error))),
            }
        }

        /// Attempts to fill the specified buffer, returning a slice of the bytes that were read.
        fn fill<'b>(&mut self, buf: &'b mut [u8]) -> Result<&'b mut [u8], Error> {
            let count = self.read(buf)?;
            Ok(&mut buf[0..count])
        }
    }

    let mut src = Wrapper { source, offset: 0 };

    macro_rules! error {
        ($value: expr) => {
            return Err(src.error($value.into()));
        };
    }

    macro_rules! result {
        ($value: expr) => {
            $value.map_err(|error| src.error(error.into()))?
        };
    }

    {
        let mut magic_buffer = [0u8; binary::MAGIC.len()];
        let magic = src.fill(&mut magic_buffer)?;
        if magic != binary::MAGIC {
            error!(InvalidMagicError {
                actual: magic.into()
            });
        }
    }

    let format_version;
    let length_size;

    {
        let mut module_information = [0u8; 3];
        let parsed_module_information = src.fill(&mut module_information)?;

        let get_information_byte = |index, error: fn() -> ErrorKind| {
            parsed_module_information
                .get(index)
                .copied()
                .ok_or_else(|| src.error(error()))
        };

        format_version = crate::module::FormatVersion {
            major: get_information_byte(0, || ErrorKind::MissingFormatVersion)?,
            minor: get_information_byte(1, || ErrorKind::MissingFormatVersion)?,
        };

        length_size = result!(binary::LengthSize::try_from(get_information_byte(
            2,
            || ErrorKind::MissingLengthSize
        )?));
    }

    let buffer_pool = buffer::Pool::existing_or_default(buffer_pool);

    Ok(crate::module::Module {
        format_version,
        contents: None,
    })
}

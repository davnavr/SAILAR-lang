//! Code for parsing SAILAR modules.

use crate::binary::{self, buffer};
use crate::Identifier;

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
    #[error("the length value {0} is too large and cannot be used")]
    LengthTooLarge(u32),
    #[error("expected size of module header but got EOF")]
    MissingHeaderSize,
    #[error("expected module header field count but got EOF")]
    MissingHeaderFieldCount,
    #[error("the module header size cannot be empty")]
    MissingModuleHeader,
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

pub type ParseResult<T> = Result<T, Error>;

pub fn parse<R: std::io::Read>(
    source: R,
    buffer_pool: Option<&buffer::Pool>,
) -> ParseResult<crate::module::Module> {
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

        fn read(&mut self, buf: &mut [u8]) -> ParseResult<usize> {
            match self.source.read(buf) {
                Ok(count) => {
                    self.offset += count;
                    Ok(count)
                }
                Err(error) => Err(self.error(ErrorKind::IO(error))),
            }
        }

        fn read_exact(&mut self, buf: &mut [u8]) -> ParseResult<()> {
            match self.source.read_exact(buf) {
                Ok(()) => {
                    self.offset += buf.len();
                    Ok(())
                }
                Err(error) => Err(self.error(ErrorKind::IO(error))),
            }
        }

        fn parse_buffer<T, P: FnOnce(Wrapper<&[u8]>) -> ParseResult<T>>(
            &mut self,
            pool: &buffer::Pool,
            length: usize,
            parser: P,
        ) -> ParseResult<T> {
            let mut buffer = pool.rent_with_length(length);
            let start_offset = self.offset;
            self.read_exact(&mut buffer)?;
            parser(Wrapper {
                source: buffer.as_slice(),
                offset: start_offset,
            })
        }

        /// Attempts to fill the specified buffer, returning a slice of the bytes that were read.
        fn fill<'b>(&mut self, buf: &'b mut [u8]) -> ParseResult<&'b mut [u8]> {
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
    let length_integer_or_else: fn(&mut Wrapper<R>, fn() -> ErrorKind) -> ParseResult<usize>;

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

        let length_size = result!(binary::LengthSize::try_from(get_information_byte(
            2,
            || ErrorKind::MissingLengthSize
        )?));

        length_integer_or_else = match length_size {
            binary::LengthSize::One => |src, error| {
                let mut buffer = 0u8;
                src.read_exact(std::slice::from_mut(&mut buffer))
                    .map_err(|_| src.error(error()))?;
                Ok(usize::from(buffer))
            },
            binary::LengthSize::Two => |src, error| {
                let mut buffer = [0u8; 2];
                src.read_exact(&mut buffer)
                    .map_err(|_| src.error(error()))?;
                Ok(usize::from(u16::from_le_bytes(buffer)))
            },
            binary::LengthSize::Four => |src, error| {
                let mut buffer = [0u8; 4];
                src.read_exact(&mut buffer)
                    .map_err(|_| src.error(error()))?;
                let value = u32::from_le_bytes(buffer);
                usize::try_from(value).map_err(|_| src.error(ErrorKind::LengthTooLarge(value)))
            },
        };
    }

    let buffer_pool = buffer::Pool::existing_or_default(buffer_pool);

    #[derive(Debug)]
    struct Header {
        name: Identifier,
        version: Box<[u8]>,
    }

    let header = {
        let header_size = length_integer_or_else(&mut src, || ErrorKind::MissingHeaderSize)?;

        if header_size == 0 {
            error!(ErrorKind::MissingModuleHeader);
        }

        src.parse_buffer(&buffer_pool, header_size, |src| todo!("parse identifiers"))?
    };

    Ok(crate::module::Module {
        format_version,
        contents: None,
    })
}

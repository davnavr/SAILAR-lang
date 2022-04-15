//! Code for parsing SAILAR modules.

use crate::binary::{self, buffer};
use crate::identifier::{self, Identifier};
use std::fmt::{Display, Formatter};

/// Used when an invalid magic value used to indicate the start of a SAILAR module is invalid.
#[derive(Clone, Debug)]
pub struct InvalidMagicError {
    actual: Vec<u8>,
}

impl Display for InvalidMagicError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "expected magic {:?}, but got {:?}",
            buffer::ByteDebug::from(binary::MAGIC),
            buffer::ByteDebug::from(&self.actual),
        )
    }
}

impl std::error::Error for InvalidMagicError {}

/// Used when a module version number could not be parsed since the end of the file was reached.
#[derive(Clone, Debug)]
pub struct MissingModuleVersionNumberError {
    index: usize,
}

impl Display for MissingModuleVersionNumberError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "expected {}th module version number but got EOF",
            self.index + 1
        )
    }
}

impl std::error::Error for MissingModuleVersionNumberError {}

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
    #[error("invalid identifier, {0}")]
    InvalidIdentifier(#[from] identifier::ParseError),
    #[error("expected module version length but got EOF")]
    MissingModuleVersionLength,
    #[error(transparent)]
    MissingModuleVersionNumber(#[from] MissingModuleVersionNumberError),
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

mod input {
    use super::{Error, ErrorKind, ParseResult};
    use crate::binary::{buffer, LengthSize};
    use crate::identifier::{self, Identifier};

    type LengthIntegerParser<R> = fn(&mut Wrapper<R>) -> ParseResult<Option<usize>>;

    pub struct Wrapper<R> {
        source: R,
        offset: usize,
        length_size: LengthSize,
        length_parser: LengthIntegerParser<R>,
    }

    impl<R: std::io::Read> Wrapper<R> {
        pub fn new(source: R) -> Self {
            Self {
                source,
                offset: 0,
                length_size: LengthSize::One,
                length_parser: Wrapper::read_length_one,
            }
        }

        pub fn length_size(&self) -> LengthSize {
            self.length_size
        }

        pub fn error(&self, error: ErrorKind) -> Error {
            Error {
                offset: self.offset,
                kind: error,
            }
        }

        pub fn read(&mut self, buffer: &mut [u8]) -> ParseResult<usize> {
            match self.source.read(buffer) {
                Ok(count) => {
                    self.offset += count;
                    Ok(count)
                }
                Err(error) => Err(self.error(ErrorKind::IO(error))),
            }
        }

        pub fn read_exact(&mut self, buffer: &mut [u8]) -> ParseResult<()> {
            match self.source.read_exact(buffer) {
                Ok(()) => {
                    self.offset += buffer.len();
                    Ok(())
                }
                Err(error) => Err(self.error(ErrorKind::IO(error))),
            }
        }

        fn read_length_one(&mut self) -> ParseResult<Option<usize>> {
            let mut buffer = 0u8;
            Ok(if self.read(std::slice::from_mut(&mut buffer))? == 1 {
                Some(usize::from(buffer))
            } else {
                None
            })
        }

        fn read_length_two(&mut self) -> ParseResult<Option<usize>> {
            let mut buffer = [0u8; 2];
            Ok(if self.read(&mut buffer)? == 2 {
                Some(usize::from(u16::from_le_bytes(buffer)))
            } else {
                None
            })
        }

        fn read_length_four(&mut self) -> ParseResult<Option<usize>> {
            let mut buffer = [0u8; 4];
            if self.read(&mut buffer)? == 4 {
                let value = u32::from_le_bytes(buffer);
                usize::try_from(value)
                    .map_err(|_| self.error(ErrorKind::LengthTooLarge(value)))
                    .map(Some)
            } else {
                Ok(None)
            }
        }

        fn select_length_parser(size: LengthSize) -> LengthIntegerParser<R> {
            match size {
                LengthSize::One => Self::read_length_one,
                LengthSize::Two => Self::read_length_two,
                LengthSize::Four => Self::read_length_four,
            }
        }

        pub fn read_length<E: FnOnce() -> ErrorKind>(
            &mut self,
            missing_error: E,
        ) -> ParseResult<usize> {
            match (self.length_parser)(self) {
                Ok(Some(length)) => Ok(length),
                Ok(None) => Err(self.error(missing_error())),
                Err(error) => Err(error),
            }
        }

        pub fn set_length_size(&mut self, size: LengthSize) {
            self.length_size = size;
            self.length_parser = Self::select_length_parser(size);
        }

        pub fn parse_buffer<T, P: FnOnce(Wrapper<&[u8]>) -> ParseResult<T>>(
            &mut self,
            pool: &buffer::Pool,
            length: usize, //buffer: &[u8],
            parser: P,
        ) -> ParseResult<T> {
            let mut buffer = pool.rent_with_length(length);
            let start_offset = self.offset;
            let length_size = self.length_size;
            self.read_exact(&mut buffer)?;
            parser(Wrapper {
                source: buffer.as_slice(),
                offset: start_offset,
                length_size,
                length_parser: Wrapper::select_length_parser(length_size),
            })
        }

        pub fn read_identifier<'b, B: FnOnce(usize) -> buffer::RentedOrOwned<'b>>(
            &mut self,
            mut buffer_source: B,
        ) -> ParseResult<Identifier> {
            let length = self.read_length(|| {
                identifier::ParseError::InvalidIdentifier(identifier::InvalidError::Empty).into()
            })?;

            let mut buffer = buffer_source(length);
            self.read_exact(buffer.as_mut_slice())?;
            Identifier::from_byte_slice(&buffer).map_err(|error| self.error(error.into()))
        }

        pub fn read_identifier_pooled<'b>(
            &mut self,
            pool: &'b buffer::Pool,
        ) -> ParseResult<Identifier> {
            self.read_identifier(|length| pool.rent_with_length(length).into())
        }

        pub fn read_many<P: FnMut(&mut Self, usize) -> ParseResult<()>>(
            &mut self,
            length: usize,
            mut parser: P,
        ) -> ParseResult<()> {
            for i in 0..length {
                parser(self, i)?;
            }
            Ok(())
        }

        /// Runs the specified `parser` a fixed number of times, appending the parsed items to the specified `vector`.
        pub fn read_many_to_vec<T, P: FnMut(&mut Self, usize) -> ParseResult<T>>(
            &mut self,
            length: usize,
            vector: &mut Vec<T>,
            mut parser: P,
        ) -> ParseResult<()> {
            self.read_many(length, |src, index| {
                vector.push(parser(src, index)?);
                Ok(())
            })
        }

        /// Attempts to fill the specified buffer, returning a slice of the bytes that were read.
        pub fn fill<'b>(&mut self, buffer: &'b mut [u8]) -> ParseResult<&'b mut [u8]> {
            let count = self.read(buffer)?;
            Ok(&mut buffer[0..count])
        }
    }
}

pub fn parse<R: std::io::Read>(
    source: R,
    buffer_pool: Option<&buffer::Pool>,
) -> ParseResult<crate::module::Module> {
    let mut src = input::Wrapper::new(source);

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

        src.set_length_size(length_size);
    }

    let buffer_pool = buffer::Pool::existing_or_default(buffer_pool);
    let buffer_pool: &buffer::Pool = &buffer_pool;

    #[derive(Debug)]
    struct Header {
        name: Identifier,
        version: Box<[usize]>,
    }

    let header = {
        let header_size = src.read_length(|| ErrorKind::MissingHeaderSize)?;

        if header_size == 0 {
            error!(ErrorKind::MissingModuleHeader);
        }

        src.parse_buffer(buffer_pool, header_size, |mut src| {
            Ok(Header {
                name: src.read_identifier_pooled(buffer_pool)?,
                version: {
                    let length = src.read_length(|| ErrorKind::MissingModuleVersionLength)?;
                    let mut numbers = Vec::with_capacity(length);
                    src.read_many_to_vec(length, &mut numbers, |src, index| {
                        src.read_length(|| MissingModuleVersionNumberError { index }.into())
                    })?;
                    numbers.into_boxed_slice()
                },
            })
        })?
    };

    Ok(crate::module::Module {
        format_version,
        contents: None,
        length_size: src.length_size(),
        name: header.name,
        version: header.version,
    })
}

//! Low-level API to read the binary contents of a SAILAR module.

use crate::binary;
use crate::binary::record::{self, Record};
use crate::identifier;
use crate::versioning;
use std::borrow::Cow;
use std::fmt::{Display, Formatter};
use std::io::Read;

/// Used when an invalid magic value used to indicate the start of a SAILAR module is invalid.
#[derive(Clone, Debug)]
pub struct InvalidMagicError {
    actual: Box<[u8]>,
}

impl InvalidMagicError {
    fn new<A: Into<Box<[u8]>>>(actual: A) -> Self {
        Self { actual: actual.into() }
    }

    #[inline]
    pub fn actual_bytes(&self) -> &[u8] {
        &self.actual
    }
}

impl Display for InvalidMagicError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "expected magic {:?}, but got {:?}",
            binary::buffer::ByteDebug::from(binary::MAGIC),
            binary::buffer::ByteDebug::from(self.actual_bytes()),
        )
    }
}

impl std::error::Error for InvalidMagicError {}

#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ErrorKind {
    #[error(transparent)]
    InvalidMagic(#[from] InvalidMagicError),
    #[error("expected format version")]
    MissingFormatVersion,
    #[error(transparent)]
    UnuspportedFormatVersion(#[from] versioning::UnsupportedFormatError),
    #[error("expected integer size")]
    MissingIntegerSize,
    #[error(transparent)]
    InvalidIntegerSize(#[from] binary::InvalidVarIntSize),
    #[error("expected record count")]
    MissingRecordCount,
    #[error("the integer value {0} was too large")]
    IntegerTooLarge(u32),
    #[error("expected record type byte")]
    MissingRecordType,
    #[error(transparent)]
    InvalidRecordType(#[from] record::InvalidTypeError),
    #[error("expected record byte size")]
    MissingRecordSize,
    #[error("unexpected end of record, expected {expected_size} bytes of content but got {actual_size}")]
    UnexpectedEndOfRecord { expected_size: usize, actual_size: usize },
    #[error(transparent)]
    InvalidIdentifier(#[from] identifier::ParseError),
    #[error("expected {expected_size} bytes for {name} but got {actual_size}")]
    UnexpectedEndOfData {
        name: &'static str,
        expected_size: usize,
        actual_size: usize,
    },
    #[error("expected identifier byte length")]
    MissingIdentifierLength,
    #[error("expected element count integer for array record")]
    MissingRecordArrayCount,
    #[error("expected end of file")]
    ExpectedEOF,
    #[error(transparent)]
    IO(#[from] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
pub struct Error {
    kind: Box<ErrorKind>,
    offset: usize,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "error occured at offset {:#X}: {}", self.offset, self.kind)
    }
}

impl Error {
    pub(crate) fn new<E: Into<ErrorKind>>(error: E, offset: usize) -> Self {
        Self {
            kind: Box::new(error.into()),
            offset,
        }
    }

    #[inline]
    pub fn offset(&self) -> usize {
        self.offset
    }

    #[inline]
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
struct Wrapper<R> {
    source: R,
    previous_offset: usize,
    offset: usize,
}

impl<R: Read> Wrapper<R> {
    fn new(source: R) -> Self {
        Self {
            source,
            offset: 0,
            previous_offset: 0,
        }
    }

    fn wrap_error<E: Into<ErrorKind>>(&self, error: E) -> Error {
        Error::new(error, self.previous_offset)
    }

    fn fail_with<T, E: Into<ErrorKind>>(&self, error: E) -> Result<T> {
        Err(self.wrap_error(error))
    }

    fn wrap_result<T, E: Into<ErrorKind>>(&self, result: std::result::Result<T, E>) -> Result<T> {
        result.map_err(|error| self.wrap_error(error))
    }

    fn read_bytes(&mut self, buffer: &mut [u8]) -> Result<usize> {
        self.previous_offset = self.offset;
        let result = self.source.read(buffer);
        let count = self.wrap_result(result)?;
        self.offset += count;
        Ok(count)
    }

    fn read_to_wrapped_buffer<'b>(&mut self, buffer: &'b mut [u8]) -> Result<(usize, Wrapper<&'b [u8]>)> {
        let count = self.read_bytes(buffer)?;
        Ok((
            count,
            Wrapper {
                source: buffer,
                offset: self.offset,
                previous_offset: self.offset,
            },
        ))
    }
}

type BufferWrapper<'b> = Wrapper<&'b [u8]>;

type IntegerReader<R> = for<'a> fn(&'a mut Wrapper<R>, fn() -> ErrorKind) -> Result<usize>;

fn select_integer_reader<R: Read>(size: binary::VarIntSize) -> IntegerReader<R> {
    match size {
        binary::VarIntSize::One => |source, error| {
            let mut value = 0u8;
            if source.read_bytes(std::slice::from_mut(&mut value))? == 1 {
                Ok(usize::from(value))
            } else {
                source.fail_with(error())
            }
        },
        binary::VarIntSize::Two => |source, error| {
            let mut buffer = [0u8; 2];
            if source.read_bytes(&mut buffer)? == 2 {
                Ok(usize::from(u16::from_le_bytes(buffer)))
            } else {
                source.fail_with(error())
            }
        },
        binary::VarIntSize::Four => |source, error| {
            let mut buffer = [0u8; 4];
            if source.read_bytes(&mut buffer)? == 4 {
                let value = u32::from_le_bytes(buffer);
                usize::try_from(value).map_err(|_| source.wrap_error(ErrorKind::IntegerTooLarge(value)))
            } else {
                source.fail_with(error())
            }
        },
    }
}

/// Provides a way to read the contents of a SAILAR module.
#[derive(Debug)]
pub struct Reader<R> {
    source: Wrapper<R>,
}

impl<R: Read> Reader<R> {
    pub fn new(source: R) -> Self {
        Self {
            source: Wrapper::new(source),
        }
    }

    /// Reads the magic number, format version, and integer size.
    ///
    /// # Examples
    ///
    /// ```
    /// # use sailar::binary::module::reader::{Reader};
    /// let input = "What happens if nonsense is used as input?";
    /// let reader = Reader::new(input.as_bytes());
    /// assert!(matches!(reader.to_record_reader(), Err(_)));
    /// ```
    pub fn to_record_reader(mut self) -> Result<(versioning::Format, binary::VarIntSize, RecordReader<R>)> {
        {
            let mut magic_buffer = [0u8; binary::MAGIC.len()];
            let magic_length = self.source.read_bytes(&mut magic_buffer)?;
            if magic_length < magic_buffer.len() {
                return self.source.fail_with(InvalidMagicError::new(&magic_buffer[0..magic_length]));
            }
        }

        let format_version: versioning::Format;
        let integer_size: binary::VarIntSize;

        {
            let mut values = [0u8; 3];
            let value_count = self.source.read_bytes(&mut values)?;

            if value_count < 2 {
                return self.source.fail_with(ErrorKind::MissingFormatVersion);
            }

            format_version = versioning::Format {
                major: values[0],
                minor: values[1],
            };

            if !format_version.is_supported() {
                return self.source.fail_with(versioning::UnsupportedFormatError::new(format_version));
            }

            if value_count < 3 {
                return self.source.fail_with(ErrorKind::MissingIntegerSize);
            }

            integer_size = self.source.wrap_result(binary::VarIntSize::try_from(values[2]))?;
        }

        let integer_reader = select_integer_reader(integer_size);
        let record_count = integer_reader(&mut self.source, || ErrorKind::MissingRecordCount)?;

        Ok((
            format_version,
            integer_size,
            RecordReader::new(self.source, integer_size, integer_reader, record_count),
        ))
    }
}

impl<R: Read> From<R> for Reader<R> {
    #[inline]
    fn from(source: R) -> Self {
        Self::new(source)
    }
}

/// Reads the records of a SAILAR module.
pub struct RecordReader<R> {
    count: usize,
    source: Wrapper<R>,
    integer_size: binary::VarIntSize,
    integer_reader: IntegerReader<R>,
    buffer: Vec<u8>,
}

impl<R: Read> RecordReader<R> {
    fn new(source: Wrapper<R>, integer_size: binary::VarIntSize, integer_reader: IntegerReader<R>, count: usize) -> Self {
        Self {
            count,
            source,
            integer_size,
            integer_reader,
            buffer: Vec::default(),
        }
    }

    /// Returns the remaining number of records in the module.
    #[inline]
    pub fn record_count(&self) -> usize {
        self.count
    }

    fn read_record<'a>(&mut self) -> Result<Record<'a>> {
        fn read_record_type<R: Read>(source: &mut Wrapper<R>) -> Result<record::Type> {
            let mut type_value = 0u8;
            if source.read_bytes(std::slice::from_mut(&mut type_value))? == 0 {
                return source.fail_with(ErrorKind::MissingRecordType);
            }
            source.wrap_result(record::Type::try_from(type_value))
        }

        let record_type = read_record_type(&mut self.source)?;
        let record_size = (self.integer_reader)(&mut self.source, || ErrorKind::MissingRecordSize)?;

        const STACK_BUFFER_LENGTH: usize = 512;
        let mut stack_buffer: [u8; STACK_BUFFER_LENGTH];

        let content_buffer = if record_size <= STACK_BUFFER_LENGTH {
            stack_buffer = [0u8; STACK_BUFFER_LENGTH];
            &mut stack_buffer[0..record_size]
        } else {
            self.buffer.clear();
            self.buffer.resize(record_size, 0u8);
            &mut self.buffer
        };

        let (actual_content_size, mut record_content) = self.source.read_to_wrapped_buffer(content_buffer)?;
        let content = &mut record_content;
        let content_integer_reader: IntegerReader<&[u8]> = select_integer_reader(self.integer_size);

        if actual_content_size != record_size {
            return self.source.fail_with(ErrorKind::UnexpectedEndOfRecord {
                expected_size: record_size,
                actual_size: actual_content_size,
            });
        }

        fn read_identifier_content(source: &mut BufferWrapper, size: usize) -> Result<identifier::Identifier> {
            let mut buffer = vec![0u8; size];
            let actual_size = source.read_bytes(&mut buffer)?;
            if actual_size != buffer.len() {
                return source.fail_with(ErrorKind::UnexpectedEndOfData {
                    name: "identifier",
                    actual_size,
                    expected_size: size,
                });
            }

            source.wrap_result(identifier::Identifier::from_utf8(buffer))
        }

        fn read_identifier<'b>(
            source: &mut BufferWrapper<'b>,
            integer_reader: IntegerReader<&'b [u8]>,
        ) -> Result<identifier::Identifier> {
            let size = integer_reader(source, || ErrorKind::MissingIdentifierLength)?;
            read_identifier_content(source, size)
        }

        self.count -= 1;

        Ok(match record_type {
            record::Type::Array => {
                let array_type = read_record_type(content)?;
                let array_count = content_integer_reader(content, || ErrorKind::MissingRecordArrayCount)?;

                match array_type {
                    record::Type::Identifier => {
                        let mut identifiers = Vec::with_capacity(array_count);

                        for _ in 0..array_count {
                            identifiers.push(Cow::Owned(read_identifier(content, content_integer_reader)?));
                        }

                        record::Record::Array(record::Array::Identifier(identifiers))
                    }
                    _ => todo!("array of {:?}", array_type),
                }
            }
            record::Type::Identifier => record::Record::from(read_identifier_content(content, record_size)?),
            _ => todo!("parse a {:?}", record_type),
        })
    }

    // TODO: What if reader returned elements of arrays as if they were not in an array?
    pub fn next_record<'a>(&mut self) -> Option<Result<Record<'a>>> {
        if self.count == 0 {
            return None;
        }

        Some(self.read_record())
    }

    /// Skips over any remaining records in the module, and checks that there are no remaining bytes in the input.
    pub fn finish(mut self) -> Result<()> {
        while let Some(record) = self.next_record() {
            record?;
        }

        let mut buffer = [0u8];
        if self.source.read_bytes(&mut buffer)? != 0 {
            return self.source.fail_with(ErrorKind::ExpectedEOF);
        }

        Ok(())
    }
}

impl<R: Read> std::iter::Iterator for RecordReader<R> {
    type Item = Result<Record<'static>>; // TODO: Allow any lifetime for records in iterator. Maybe make a RecordIterator struct.

    fn next(&mut self) -> Option<Self::Item> {
        self.next_record()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.count, Some(self.count))
    }
}

impl<R: Read> std::iter::ExactSizeIterator for RecordReader<R> {
    #[inline]
    fn len(&self) -> usize {
        self.count
    }
}

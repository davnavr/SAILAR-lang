//! Low-level API to read the binary contents of a SAILAR module.

use crate::binary;
use crate::versioning;
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
}

type IntegerReader<R> = for<'a> fn(&'a mut Wrapper<R>, fn() -> ErrorKind) -> Result<usize>;

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
    /// let input = "What happens if nonsense is used as input?";
    /// let mut reader =
    /// ```
    pub fn to_record_reader(mut self) -> Result<(versioning::Format, binary::VarIntSize, RecordReader<R>)> {
        {
            let mut magic_buffer = [0u8; binary::MAGIC.len()];
            let mut magic_length = self.source.read_bytes(&mut magic_buffer)?;
            if magic_length < magic_buffer.len() {
                return self.source.fail_with(InvalidMagicError::new(&magic_buffer[0..magic_length]));
            }
        }

        let format_version: versioning::Format;
        let integer_size: binary::VarIntSize;

        {
            let mut values = [0u8, 3];
            let mut value_count = self.source.read_bytes(&mut values)?;

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

        let integer_reader: IntegerReader<R> = match integer_size {
            binary::VarIntSize::One => |source, error| {
                let mut value = 0u8;
                if source.read_bytes(std::slice::from_mut(&mut value))? == 1 {
                    Ok(usize::from(value))
                } else {
                    source.fail_with(error())
                }
            },
        };

        let record_count = integer_reader(&mut self.source, || ErrorKind::MissingRecordCount)?;

        Ok((format_version, integer_size, RecordReader::new(self.source, integer_reader, record_count)))
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
    integer_reader: IntegerReader<R>,
    buffer: Vec<u8>,
}

impl<R: Read> RecordReader<R> {
    fn new(source: Wrapper<R>, integer_reader: IntegerReader<R>, count: usize) -> Self {
        Self {
            count,
            source,
            integer_reader,
            buffer: Vec::default(),
        }
    }

    /// Returns the remaining number of records in the module.
    #[inline]
    pub fn record_count(&self) -> usize {
        self.count
    }

    pub fn next_record(&mut self) -> Option<Result<Record>> {
        if self.count == 0 {
            return None;
        }

        todo!("parse record")
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

pub struct Record;

impl<R: Read> std::iter::Iterator for RecordReader<R> {
    type Item = Result<Record>;

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

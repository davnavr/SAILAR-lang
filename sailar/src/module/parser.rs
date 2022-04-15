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

        fn fill<'b>(&mut self, buf: &'b mut [u8]) -> Result<&'b mut [u8], Error> {
            let count = self.read(buf)?;
            Ok(&mut buf[0..count])
        }
    }

    let buffer_pool = buffer::Pool::existing_or_default(buffer_pool);
    let mut src = Wrapper { source, offset: 0 };

    macro_rules! error {
        ($value: expr) => {
            return Err(src.error($value.into()));
        };
    }

    {
        let mut magic_buffer = buffer_pool.rent_with_length(binary::MAGIC.len());
        let magic = src.fill(&mut magic_buffer)?;
        if magic != binary::MAGIC {
            error!(InvalidMagicError {
                actual: magic.into()
            });
        }
    }

    todo!("implement parsing");

    //# TODO: Store bytes in contents field
}

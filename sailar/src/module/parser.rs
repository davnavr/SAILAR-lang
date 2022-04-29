//! Code for parsing SAILAR modules.

use crate::binary::{self, buffer};
use crate::identifier;
use crate::module;
use crate::type_system;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::sync::Arc;

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

#[derive(Clone, Debug, thiserror::Error)]
pub enum InvalidInstructionKind {
    #[error("expected overflow behavior byte, but reached end")]
    MissingOverflowBehavior,
    #[error(transparent)]
    InvalidOverflowBehavior(#[from] crate::instruction_set::InvalidOverflowBehaviorError),
    #[error("expected value flag byte")]
    MissingValueFlag,
    #[error("the value flag {value:#02X} is invalid")]
    InvalidValueFlag { value: u8 },
    #[error("expected index to a register, but reached end")]
    MissingRegisterIndex,
    #[error("only constant integers are currently supported")]
    UnknownConstantKind,
    #[error("missing return value count for instruction")]
    MissingReturnCount,
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("invalid instruction at index {instruction_index} in code block {block_index}, {kind}")]
pub struct InvalidInstructionError {
    block_index: usize,
    instruction_index: usize,
    kind: InvalidInstructionKind,
}

impl InvalidInstructionError {
    #[inline]
    pub fn block_index(&self) -> usize {
        self.block_index
    }

    #[inline]
    pub fn instruction_index(&self) -> usize {
        self.instruction_index
    }

    #[inline]
    pub fn kind(&self) -> &InvalidInstructionKind {
        &self.kind
    }
}

#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ErrorKind {
    #[error(transparent)]
    InvalidMagic(#[from] InvalidMagicError),
    #[error("expected {0} but reached end")]
    Missing(&'static str),
    #[error(transparent)]
    InvalidIntegerSize(#[from] binary::InvalidVarIntSize),
    #[error("the length value {0} is too large and cannot be used")]
    IntegerTooLarge(u32),
    #[error("invalid identifier, {0}")]
    InvalidIdentifier(#[from] identifier::ParseError),
    #[error(transparent)]
    InvalidRecordType(#[from] binary::InvalidRecordTypeError),
    #[error("nested array records are not allowed")]
    NestedArrayRecord,
    #[error(transparent)]
    InvalidTypeSignatureTag(#[from] binary::signature::InvalidTypeCode),
    #[error("unable to find {description} corresponding to index {index}")]
    NotFound { description: &'static str, index: usize },
    #[error(transparent)]
    IO(#[from] std::io::Error),
}

#[derive(thiserror::Error)]
#[error("error at offset {offset:#X}, {kind}")]
pub struct Error {
    offset: usize,
    kind: Box<ErrorKind>,
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

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        struct Offset(usize);

        impl std::fmt::Debug for Offset {
            fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
                write!(f, "{:#X}", self.0)
            }
        }

        f.debug_struct("Error")
            .field("offset", &Offset(self.offset))
            .field("kind", self.kind())
            .finish()
    }
}

pub type ParseResult<T> = Result<T, Error>;

mod input {
    use super::{Error, ErrorKind, ParseResult};
    use crate::binary::VarIntSize;

    type IntegerParser<R> = fn(&mut Wrapper<R>) -> ParseResult<Option<usize>>;

    pub struct Wrapper<R> {
        source: R,
        offset: usize,
        integer_size: VarIntSize,
        integer_parser: IntegerParser<R>,
    }

    impl<R: std::io::Read> Wrapper<R> {
        pub fn new(source: R) -> Self {
            Self {
                source,
                offset: 0,
                integer_size: VarIntSize::One,
                integer_parser: Wrapper::read_integer_one,
            }
        }

        pub fn offset(&self) -> usize {
            self.offset
        }

        pub fn integer_size(&self) -> VarIntSize {
            self.integer_size
        }

        pub fn error(&self, error: ErrorKind) -> Error {
            Error {
                offset: self.offset,
                kind: Box::new(error),
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

        fn read_integer_one(&mut self) -> ParseResult<Option<usize>> {
            let mut buffer = 0u8;
            Ok(if self.read(std::slice::from_mut(&mut buffer))? == 1 {
                Some(usize::from(buffer))
            } else {
                None
            })
        }

        fn read_integer_two(&mut self) -> ParseResult<Option<usize>> {
            let mut buffer = [0u8; 2];
            Ok(if self.read(&mut buffer)? == 2 {
                Some(usize::from(u16::from_le_bytes(buffer)))
            } else {
                None
            })
        }

        fn read_integer_four(&mut self) -> ParseResult<Option<usize>> {
            let mut buffer = [0u8; 4];
            if self.read(&mut buffer)? == 4 {
                let value = u32::from_le_bytes(buffer);
                usize::try_from(value)
                    .map_err(|_| self.error(ErrorKind::IntegerTooLarge(value)))
                    .map(Some)
            } else {
                Ok(None)
            }
        }

        fn select_integer_parser(size: VarIntSize) -> IntegerParser<R> {
            match size {
                VarIntSize::One => Self::read_integer_one,
                VarIntSize::Two => Self::read_integer_two,
                VarIntSize::Four => Self::read_integer_four,
            }
        }

        pub fn read_integer<E: FnOnce() -> ErrorKind>(&mut self, missing_error: E) -> ParseResult<usize> {
            match (self.integer_parser)(self) {
                Ok(Some(length)) => Ok(length),
                Ok(None) => Err(self.error(missing_error())),
                Err(error) => Err(error),
            }
        }

        pub fn read_size_and_count<S: FnOnce() -> ErrorKind, C: FnOnce() -> ErrorKind>(
            &mut self,
            missing_size: S,
            missing_count: C,
        ) -> ParseResult<(usize, usize)> {
            let size = self.read_integer(missing_size)?;
            if size == 0 {
                Ok((size, 0))
            } else {
                Ok((size, self.read_integer(missing_count)?))
            }
        }

        pub fn set_integer_size(&mut self, size: VarIntSize) {
            self.integer_size = size;
            self.integer_parser = Self::select_integer_parser(size);
        }

        pub fn read_buffer<T, P: FnOnce(Wrapper<&[u8]>) -> ParseResult<T>>(
            &mut self,
            mut buffer: &mut [u8],
            parser: P,
        ) -> ParseResult<T> {
            let start_offset = self.offset;
            let integer_size = self.integer_size;
            self.read_exact(&mut buffer)?;
            parser(Wrapper {
                source: buffer,
                offset: start_offset,
                integer_size,
                integer_parser: Wrapper::select_integer_parser(integer_size),
            })
        }

        pub fn read_identifier(&mut self) -> ParseResult<crate::Identifier> {
            let length = self.read_integer(|| {
                crate::identifier::ParseError::InvalidIdentifier(crate::identifier::InvalidError::Empty).into()
            })?;

            let mut buffer = vec![0u8; length];
            self.read_exact(buffer.as_mut_slice())?;
            crate::Identifier::from_byte_slice(&buffer).map_err(|error| self.error(error.into()))
        }

        pub fn read_many<P: FnMut(&mut Self, usize) -> ParseResult<()>>(
            &mut self,
            count: usize,
            mut parser: P,
        ) -> ParseResult<()> {
            for i in 0..count {
                parser(self, i)?;
            }
            Ok(())
        }

        /// Runs the specified `parser` a fixed number of times, appending the parsed items to the specified `vector`.
        pub fn read_many_to_vec<T, P: FnMut(&mut Self, usize) -> ParseResult<T>>(
            &mut self,
            count: usize,
            vector: &mut Vec<T>,
            mut parser: P,
        ) -> ParseResult<()> {
            self.read_many(count, |src, index| {
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

    impl<'a> Wrapper<&'a [u8]> {
        pub fn parse_slice<T, P: FnOnce(Self) -> ParseResult<T>, E: FnOnce(usize) -> ErrorKind>(
            &mut self,
            length: usize,
            error: E,
            parser: P,
        ) -> ParseResult<T> {
            let actual_length = self.source.len();
            if length > actual_length {
                let start_offset = self.offset;
                let (contents, remaining) = self.source.split_at(length);
                self.offset += length;
                self.source = remaining;
                parser(Wrapper {
                    source: contents,
                    offset: start_offset,
                    integer_size: self.integer_size,
                    integer_parser: self.integer_parser,
                })
            } else {
                Err(self.error(error(actual_length)))
            }
        }

        pub fn take_remaining_bytes(&mut self) -> &'a [u8] {
            std::mem::take(&mut self.source)
        }
    }

    impl<R: std::fmt::Debug> std::fmt::Debug for Wrapper<R> {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.debug_struct("Wrapper")
                .field("source", &self.source)
                .field("offset", &self.offset)
                .field("integer_size", &self.integer_size)
                .finish()
        }
    }
}
macro_rules! error {
    ($src: expr, $value: expr) => {
        return Err($src.error($value.into()))
    };
}

macro_rules! result {
    ($src: expr, $value: expr) => {
        $value.map_err(|error| $src.error(error.into()))?
    };
}

pub fn parse<R: std::io::Read>(source: R, buffer_pool: Option<&buffer::Pool>) -> ParseResult<module::Definition> {
    use input::Wrapper;

    let mut src = Wrapper::new(source);

    {
        let mut magic_buffer = [0u8; binary::MAGIC.len()];
        let magic = src.fill(&mut magic_buffer)?;
        if magic != binary::MAGIC {
            error!(src, InvalidMagicError { actual: magic.into() });
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

        format_version = module::FormatVersion {
            major: get_information_byte(0, || ErrorKind::Missing("major format version"))?,
            minor: get_information_byte(1, || ErrorKind::Missing("minor format version"))?,
        };

        let integer_size = result!(
            src,
            binary::VarIntSize::try_from(get_information_byte(2, || { ErrorKind::Missing("integer size") })?)
        );

        src.set_integer_size(integer_size);
    }

    let module_identifer = Arc::new(module::ModuleIdentifier::new(src.read_identifier()?, {
        let length = src.read_integer(|| ErrorKind::Missing("module version length"))?;
        let mut numbers = Vec::with_capacity(length);
        src.read_many_to_vec(length, &mut numbers, |src, _| {
            src.read_integer(|| ErrorKind::Missing("module version number"))
        })?;
        numbers.into_boxed_slice()
    }));

    let buffer_pool = buffer::Pool::existing_or_default(buffer_pool); // TODO: Could just have a single buffer instead, remove buffer_pool?
    let record_count = src.read_integer(|| ErrorKind::Missing("record count"))?;

    #[derive(Debug)]
    struct FunctionSignature {
        return_types: Box<[usize]>,
        parameter_types: Box<[usize]>,
    }

    #[derive(Debug, Default)]
    pub struct IndexBuffer {
        buffer: Vec<usize>,
    }

    impl IndexBuffer {
        pub fn with_buffer<F: FnOnce(&mut Vec<usize>) -> ParseResult<()>>(
            &mut self,
            count: usize,
            f: F,
        ) -> ParseResult<Box<[usize]>> {
            self.buffer.clear();
            self.buffer.reserve(count);
            let result = f(&mut self.buffer)?;
            Ok(self.buffer.clone().into_boxed_slice())
        }
    }

    let mut index_buffer = IndexBuffer::default();

    let identifiers = RefCell::<Vec<identifier::Identifier>>::default();
    let data_arrays = RefCell::<Vec<Arc<[u8]>>>::default();
    let type_signatures = RefCell::<Vec<type_system::Any>>::default(); // TODO: Make a TypeSignature enum?
    let mut raw_function_signatures = RefCell::<Vec<FunctionSignature>>::default();

    src.read_many(record_count, |src, _| {
        use binary::RecordType;

        let mut parse_record = |contents: &mut Wrapper<&[u8]>, record_type: RecordType| match record_type {
            RecordType::Identifier => {
                identifiers.borrow_mut().push(contents.read_identifier()?);
                Ok(())
            }
            RecordType::Data => {
                data_arrays.borrow_mut().push(contents.take_remaining_bytes().into());
                Ok(())
            }
            RecordType::TypeSignature => {
                use binary::signature::TypeCode;

                let mut tag_value = 0u8;
                if contents.read(std::slice::from_mut(&mut tag_value))? == 0 {
                    error!(contents, ErrorKind::Missing("type signature tag"))
                }

                let tag = result!(contents, TypeCode::try_from(tag_value));
                type_signatures.borrow_mut().push(match tag {
                    TypeCode::U8 => type_system::FixedInt::U8.into(),
                    TypeCode::U16 => type_system::FixedInt::U16.into(),
                    TypeCode::U32 => type_system::FixedInt::U32.into(),
                    TypeCode::U64 => type_system::FixedInt::U64.into(),
                    TypeCode::S8 => type_system::FixedInt::S8.into(),
                    TypeCode::S16 => type_system::FixedInt::S16.into(),
                    TypeCode::S32 => type_system::FixedInt::S32.into(),
                    TypeCode::S64 => type_system::FixedInt::S64.into(),
                    TypeCode::F32 => type_system::Real::F32.into(),
                    TypeCode::F64 => type_system::Real::F64.into(),
                });
                Ok(())
            }
            RecordType::FunctionSignature => {
                let return_type_count = contents.read_integer(|| ErrorKind::Missing("function signature return type count"))?;
                let parameter_type_count =
                    contents.read_integer(|| ErrorKind::Missing("function signature parameter type count"))?;

                raw_function_signatures.borrow_mut().push(FunctionSignature {
                    return_types: index_buffer.with_buffer(return_type_count, |indices| {
                        contents.read_many_to_vec(return_type_count, indices, |c, _| {
                            c.read_integer(|| ErrorKind::Missing("function signature return type index"))
                        })
                    })?,
                    parameter_types: index_buffer.with_buffer(parameter_type_count, |indices| {
                        contents.read_many_to_vec(parameter_type_count, indices, |c, _| {
                            c.read_integer(|| ErrorKind::Missing("function signature parameter type index"))
                        })
                    })?,
                });
                Ok(())
            }
            RecordType::Array => error!(contents, ErrorKind::NestedArrayRecord),
            bad => todo!("unsupported record type {:?}", bad),
        };

        fn parse_record_type<R: std::io::Read>(src: &mut Wrapper<R>) -> ParseResult<RecordType> {
            let mut type_byte = 0u8;
            if src.read(std::slice::from_mut(&mut type_byte))? == 0 {
                error!(src, ErrorKind::Missing("record type"))
            }
            Ok(result!(src, RecordType::try_from(type_byte)))
        }

        let record_type = parse_record_type(src)?;
        let content_size = src.read_integer(|| ErrorKind::Missing("record content size"))?;
        let mut contents_buffer = buffer_pool.rent_with_length(content_size);
        src.read_buffer(&mut contents_buffer, |mut contents| match record_type {
            RecordType::Array => {
                let element_type = parse_record_type(&mut contents)?;
                let element_count = contents.read_integer(|| ErrorKind::Missing("array record element count"))?;

                match element_type {
                    RecordType::Identifier => identifiers.borrow_mut().reserve(element_count),
                    RecordType::Data => data_arrays.borrow_mut().reserve(element_count),
                    RecordType::TypeSignature => type_signatures.borrow_mut().reserve(element_count),
                    RecordType::FunctionSignature => raw_function_signatures.borrow_mut().reserve(element_count),
                    _ => (),
                }

                contents.read_many(element_count, |elements, _| parse_record(elements, element_type))
            }
            _ => parse_record(&mut contents, record_type),
        })
    })?;

    let type_signatures = type_signatures.take();

    let get_type_signature = |index| -> Result<&type_system::Any, _> {
        type_signatures.get(index).ok_or(ErrorKind::NotFound {
            description: "type signature",
            index,
        })
    };

    // TODO: Have Vec<Option<Arc>>, so that allocation only occurs for function signatures that are used.
    let function_signatures = {
        let mut raw_function_signatures = raw_function_signatures.get_mut().drain(..);
        let mut function_signatures = Vec::with_capacity(raw_function_signatures.len());
        let mut result_type_buffer = Vec::default();
        let mut parameter_type_buffer = Vec::default();

        let get_function_types =
            |indices: &[usize], buffer: &mut Vec<type_system::Any>| -> ParseResult<Box<[type_system::Any]>> {
                buffer.clear();
                buffer.reserve(indices.len());
                for index in indices.iter().cloned() {
                    buffer.push(result!(src, get_type_signature(index)).clone());
                }
                Ok(buffer.clone().into_boxed_slice())
            };

        for signature in raw_function_signatures {
            function_signatures.push(Arc::new(crate::function::Signature::new(
                get_function_types(&signature.return_types, &mut result_type_buffer)?,
                get_function_types(&signature.parameter_types, &mut parameter_type_buffer)?,
            )));
        }

        function_signatures
    };

    let get_function_signature = |index| -> Result<Arc<_>, _> {
        function_signatures.get(index).cloned().ok_or(ErrorKind::NotFound {
            description: "function signature",
            index,
        })
    };

    //todo!("parse")

    Ok(module::Definition {
        format_version,
        contents: None,
        integer_size: src.integer_size(),
        identifier: module_identifer,
        symbols: Default::default(), //symbol_lookup.lookup,
        function_definitions: Default::default(),
        function_imports: Default::default(),
    })
}

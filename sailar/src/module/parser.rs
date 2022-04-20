//! Code for parsing SAILAR modules.

use crate::binary::{self, buffer, signature::TypeCode};
use crate::function;
use crate::identifier::{self, Identifier};
use crate::type_system;
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

/// Used when a module version number could not be parsed since the end of the file was reached.
#[derive(Clone, Debug)]
pub struct MissingModuleVersionNumberError {
    index: usize,
}

impl Display for MissingModuleVersionNumberError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "expected {}th module version number but got EOF", self.index + 1)
    }
}

impl std::error::Error for MissingModuleVersionNumberError {}

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
    #[error("expected module header field count but reached end")]
    MissingHeaderFieldCount,
    #[error("the module header size cannot be empty")]
    MissingModuleHeader,
    #[error("invalid identifier, {0}")]
    InvalidIdentifier(#[from] identifier::ParseError),
    #[error("expected module version length but reached end")]
    MissingModuleVersionLength,
    #[error(transparent)]
    MissingModuleVersionNumber(#[from] MissingModuleVersionNumberError),
    #[error("expected byte size of identifiers but got EOF")]
    MissingIdentifierSize,
    #[error("expected identifier count but got EOF")]
    MissingIdentifierCount,
    #[error("expected byte size of type signatures but got EOF")]
    MissingTypeSignatureSize,
    #[error("expected type signature count but got EOF")]
    MissingTypeSignatureCount,
    #[error("expected type signature at index {index} but reached end")]
    MissingTypeSignature { index: usize },
    #[error(transparent)]
    InvalidTypeSignatureTag(#[from] binary::signature::InvalidTypeCode),
    #[error("expected byte size of function signatures but got EOF")]
    MissingFunctionSignatureSize,
    #[error("expected function signature count but got EOF")]
    MissingFunctionSignatureCount,
    #[error("expected return type count in function signature but got EOF")]
    MissingFunctionSignatureReturnTypeCount,
    #[error("expected parameter count in function signature but got EOF")]
    MissingFunctionSignatureParameterCount,
    #[error("type signature at index {index} does not exist")]
    TypeSignatureNotFound { index: usize },
    #[error("expected function signature return type at index {index} but got EOF")]
    MissingFunctionSignatureReturnType { index: usize },
    #[error("expected function signature parameter at index {index} but got EOF")]
    MissingFunctionSignatureParameter { index: usize },
    #[error("function signature at index {index} does not exist")]
    FunctionSignatureNotFound { index: usize },
    #[error("expected total byte size for data but got EOF")]
    MissingDataSize,
    #[error("expected data array count but got EOF")]
    MissingDataCount,
    #[error("expected byte length for data array at index {index} but reached end")]
    MissingDataArrayLength { index: usize },
    #[error("expected data array of length {expected_length} bytes but actual was {actual_length}")]
    IncompleteDataArray { expected_length: usize, actual_length: usize },
    #[error("expected byte length for code blocks but got EOF")]
    MissingCodeSize,
    #[error("expected code block count but got EOF")]
    MissingCodeBlockCount,
    #[error("expected input count for code block at index {index} but reached end")]
    MissingBlockInputCount { index: usize },
    #[error("expected result count for code block at index {index} but reached end")]
    MissingBlockResultCount { index: usize },
    #[error("expected temporary count for code block at index {index} but reached end")]
    MissingBlockTemporaryCount { index: usize },
    #[error("missing register type index for code block at index {block_index}")]
    MissingCodeBlockRegisterType { block_index: usize },
    #[error("expected byte size for instruction of code block at index {block_index}, but reached end")]
    MissingCodeBlockInstructionSize { block_index: usize },
    #[error("expected instruction count for code block at index {block_index}, but reached end")]
    MissingCodeBlockInstructionCount { block_index: usize },
    #[error("at least one instruction expected in code block at index {block_index}, but block was empty")]
    EmptyCodeBlockInstructions { block_index: usize },
    #[error(transparent)]
    InvalidOpcode(#[from] crate::instruction_set::InvalidOpcodeError),
    #[error(
        "expected opcode byte for instruction {instruction_index} in code block {block_index}, but reached end of instructions"
    )]
    MissingOpcode { block_index: usize, instruction_index: usize },
    #[error(transparent)]
    InvalidInstruction(#[from] InvalidInstructionError),
    #[error("expected byte size of module definitions, but got EOF")]
    MissingDefinitionSize,
    #[error("expected function definition count, but reached end")]
    MissingFunctionDefinitionCount,
    #[error("expected flag byte for function definition at index {index}, but reached end")]
    MissingFunctionDefinitionFlags { index: usize },
    #[error("function definition at index {index} has a flag value of {value:#02X} which is invalid")]
    InvalidFunctionDefinitionFlags { index: usize, value: u8 },
    #[error("function definition at index {index} is missing a function signature")]
    MissingFunctionDefinitionSignature { index: usize },
    #[error("function definition at index {index} is missing its entry block")]
    MissingFunctionDefinitionEntryBlock { index: usize },
    #[error("code block at index {index} does not exist")]
    CodeBlockNotFound { index: usize },
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

        pub fn offset(&self) -> usize {
            self.offset
        }

        pub fn length_size(&self) -> LengthSize {
            self.length_size
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

        pub fn read_length<E: FnOnce() -> ErrorKind>(&mut self, missing_error: E) -> ParseResult<usize> {
            match (self.length_parser)(self) {
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
            let size = self.read_length(missing_size)?;
            if size == 0 {
                Ok((size, 0))
            } else {
                Ok((size, self.read_length(missing_count)?))
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

        pub fn read_identifier(&mut self) -> ParseResult<Identifier> {
            let length =
                self.read_length(|| identifier::ParseError::InvalidIdentifier(identifier::InvalidError::Empty).into())?;

            let mut buffer = vec![0u8; length];
            self.read_exact(buffer.as_mut_slice())?;
            Identifier::from_byte_slice(&buffer).map_err(|error| self.error(error.into()))
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

    impl Wrapper<&[u8]> {
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
                    length_size: self.length_size,
                    length_parser: self.length_parser,
                })
            } else {
                Err(self.error(error(actual_length)))
            }
        }
    }

    impl<R: std::fmt::Debug> std::fmt::Debug for Wrapper<R> {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.debug_struct("Wrapper")
                .field("source", &self.source)
                .field("offset", &self.offset)
                .field("length_size", &self.length_size)
                .finish()
        }
    }
}

pub fn parse<R: std::io::Read>(source: R, buffer_pool: Option<&buffer::Pool>) -> ParseResult<crate::module::Module> {
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
            error!(InvalidMagicError { actual: magic.into() });
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

        let length_size = result!(binary::LengthSize::try_from(get_information_byte(2, || {
            ErrorKind::MissingLengthSize
        })?));

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
                name: src.read_identifier()?,
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

    let identifiers: Vec<Identifier> = {
        let (byte_size, count) =
            src.read_size_and_count(|| ErrorKind::MissingIdentifierSize, || ErrorKind::MissingIdentifierCount)?;

        src.parse_buffer(buffer_pool, byte_size, |mut src| {
            let mut identifiers = Vec::with_capacity(count);
            src.read_many_to_vec(count, &mut identifiers, |src, _| src.read_identifier())?;
            Ok(identifiers)
        })?
    };

    let type_signatures: Vec<type_system::Any> = {
        let (byte_size, count) = src.read_size_and_count(
            || ErrorKind::MissingTypeSignatureSize,
            || ErrorKind::MissingTypeSignatureCount,
        )?;

        src.parse_buffer(buffer_pool, byte_size, |mut src| {
            let mut signatures = Vec::with_capacity(count);
            src.read_many_to_vec(count, &mut signatures, |src, index| {
                let mut tag_value = 0u8;

                if src.read(std::slice::from_mut(&mut tag_value))? == 0 {
                    return Err(src.error(ErrorKind::MissingTypeSignature { index }));
                }

                let tag = TypeCode::try_from(tag_value).map_err(|bad| src.error(bad.into()))?;
                match tag {
                    TypeCode::U8 => Ok(type_system::FixedInt::U8.into()),
                    TypeCode::U16 => Ok(type_system::FixedInt::U16.into()),
                    TypeCode::U32 => Ok(type_system::FixedInt::U32.into()),
                    TypeCode::U64 => Ok(type_system::FixedInt::U64.into()),
                    TypeCode::S8 => Ok(type_system::FixedInt::S8.into()),
                    TypeCode::S16 => Ok(type_system::FixedInt::S16.into()),
                    TypeCode::S32 => Ok(type_system::FixedInt::S32.into()),
                    TypeCode::S64 => Ok(type_system::FixedInt::S64.into()),
                    TypeCode::F32 => Ok(type_system::Real::F32.into()),
                    TypeCode::F64 => Ok(type_system::Real::F64.into()),
                }
            })?;
            Ok(signatures)
        })?
    };

    let get_type_signature = |index| {
        type_signatures
            .get(index)
            .cloned()
            .ok_or(ErrorKind::TypeSignatureNotFound { index })
    };

    let function_signatures: Vec<Arc<function::Signature>> = {
        let (byte_size, count) = src.read_size_and_count(
            || ErrorKind::MissingFunctionSignatureSize,
            || ErrorKind::MissingFunctionSignatureCount,
        )?;

        src.parse_buffer(buffer_pool, byte_size, |mut src| {
            let mut signatures = Vec::with_capacity(count);
            src.read_many_to_vec(count, &mut signatures, |src, _| {
                let return_type_count = src.read_length(|| ErrorKind::MissingFunctionSignatureReturnTypeCount)?;
                let parameter_count = src.read_length(|| ErrorKind::MissingFunctionSignatureParameterCount)?;

                let mut return_types = Vec::with_capacity(return_type_count);
                src.read_many_to_vec(return_type_count, &mut return_types, |src, index| {
                    get_type_signature(src.read_length(|| ErrorKind::MissingFunctionSignatureReturnType { index })?)
                        .map_err(|error| src.error(error))
                })?;

                let mut parameter_types = Vec::with_capacity(parameter_count);
                src.read_many_to_vec(parameter_count, &mut parameter_types, |src, index| {
                    get_type_signature(src.read_length(|| ErrorKind::MissingFunctionSignatureParameter { index })?)
                        .map_err(|error| src.error(error))
                })?;

                Ok(Arc::new(function::Signature::new(return_types, parameter_types)))
            })?;
            Ok(signatures)
        })?
    };

    let get_function_signature = |index| -> Result<Arc<_>, _> {
        function_signatures
            .get(index)
            .cloned()
            .ok_or(ErrorKind::FunctionSignatureNotFound { index })
    };

    let _data_arrays: Vec<Arc<[u8]>> = {
        let (byte_size, count) = src.read_size_and_count(|| ErrorKind::MissingDataSize, || ErrorKind::MissingDataCount)?;

        src.parse_buffer(buffer_pool, byte_size, |mut src| {
            let mut arrays = Vec::with_capacity(count);
            src.read_many_to_vec(count, &mut arrays, |src, index| {
                let length = src.read_length(|| ErrorKind::MissingDataArrayLength { index })?;
                let mut data = buffer_pool.rent_with_length(length);

                let actual_length = src.read(&mut data)?;
                if actual_length != length {
                    return Err(src.error(ErrorKind::IncompleteDataArray {
                        expected_length: length,
                        actual_length,
                    }));
                }

                Ok(Arc::<[u8]>::from(data.as_slice()))
            })?;
            Ok(arrays)
        })?
    };

    let code_blocks: Vec<Arc<crate::block::Block>> = {
        let (byte_size, count) = src.read_size_and_count(|| ErrorKind::MissingCodeSize, || ErrorKind::MissingCodeBlockCount)?;

        src.parse_buffer(buffer_pool, byte_size, |mut src| {
            let mut blocks = Vec::with_capacity(count);
            src.read_many_to_vec(count, &mut blocks, |src, block_index| {
                let input_count = src.read_length(|| ErrorKind::MissingBlockInputCount { index: block_index })?;
                let result_count = src.read_length(|| ErrorKind::MissingBlockResultCount { index: block_index })?;
                let temporary_count = src.read_length(|| ErrorKind::MissingBlockTemporaryCount { index: block_index })?;

                macro_rules! register_types {
                    ($vector_name: ident, $register_count: ident) => {
                        let mut $vector_name = Vec::with_capacity($register_count);
                        src.read_many_to_vec($register_count, &mut $vector_name, |src, _| {
                            get_type_signature(src.read_length(|| ErrorKind::MissingCodeBlockRegisterType { block_index })?)
                                .map_err(|error| src.error(error.into()))
                        })?;
                    };
                }

                register_types!(input_register_types, input_count);
                register_types!(result_register_types, result_count);
                register_types!(temporary_register_types, temporary_count);

                let instruction_size = src.read_length(|| ErrorKind::MissingCodeBlockInstructionSize { block_index })?;
                let instruction_count = src.read_length(|| ErrorKind::MissingCodeBlockInstructionCount { block_index })?;

                if instruction_size == 0 || instruction_count == 0 {
                    return Err(src.error(ErrorKind::EmptyCodeBlockInstructions { block_index }));
                }

                let mut instructions = Vec::with_capacity(instruction_count);

                // NOTE: Since input is a slice, can avoid allocating a buffer here.
                src.parse_buffer(buffer_pool, instruction_size, |mut src| {
                    src.read_many_to_vec(instruction_count, &mut instructions, |stream, instruction_index| {
                        use crate::instruction_set::{self, Instruction, Opcode};

                        let mut opcode_value = 0u8;
                        if stream.read(std::slice::from_mut(&mut opcode_value))? == 0 {
                            return Err(stream.error(ErrorKind::MissingOpcode {
                                block_index,
                                instruction_index,
                            }));
                        }

                        type Stream<'a, 'b> = &'b mut input::Wrapper<&'a [u8]>;

                        let invalid_instruction = |stream: Stream, error: InvalidInstructionKind| {
                            stream.error(ErrorKind::InvalidInstruction(InvalidInstructionError {
                                block_index,
                                instruction_index,
                                kind: error,
                            }))
                        };

                        let read_length = |stream: Stream, error: fn() -> InvalidInstructionKind| {
                            stream.read_length(|| {
                                ErrorKind::InvalidInstruction(InvalidInstructionError {
                                    block_index,
                                    instruction_index,
                                    kind: error(),
                                })
                            })
                        };

                        let instruction_value = |stream: Stream| -> ParseResult<instruction_set::Value> {
                            let mut flag_value = 0u8;
                            if stream.read(std::slice::from_mut(&mut flag_value))? == 0 {
                                return Err(invalid_instruction(stream, InvalidInstructionKind::MissingValueFlag));
                            }

                            let flag = instruction_set::ValueFlags::from_bits(flag_value).ok_or_else(|| {
                                invalid_instruction(stream, InvalidInstructionKind::InvalidValueFlag { value: flag_value })
                            })?;

                            if flag.contains(instruction_set::ValueFlags::CONSTANT) {
                                if !flag.contains(instruction_set::ValueFlags::INTEGER) {
                                    return Err(invalid_instruction(stream, InvalidInstructionKind::UnknownConstantKind));
                                }

                                let value_embedded = flag.contains(instruction_set::ValueFlags::INTEGER_EMBEDDED);

                                macro_rules! sized_integer_value {
                                    ($size: literal, $case: ident) => {{
                                        let mut bytes = [0u8; $size];
                                        stream.read_exact(&mut bytes)?;
                                        instruction_set::Value::Constant(instruction_set::Constant::Integer(
                                            instruction_set::ConstantInteger::$case(bytes),
                                        ))
                                    }};
                                }

                                macro_rules! embedded_integer_value {
                                    ($integer_type: ty) => {{
                                        let embedded_value: $integer_type =
                                            if flag.contains(instruction_set::ValueFlags::INTEGER_EMBEDDED_ONE) {
                                                1
                                            } else {
                                                0
                                            };
                                        instruction_set::Value::from(embedded_value)
                                    }};
                                }

                                Ok(match (flag & instruction_set::ValueFlags::INTEGER_SIZE_MASK).bits() >> 2 {
                                    0 if value_embedded => embedded_integer_value!(u8),
                                    0 => {
                                        let mut value = 0u8;
                                        stream.read_exact(std::slice::from_mut(&mut value))?;
                                        instruction_set::Value::from(value)
                                    }
                                    1 if value_embedded => embedded_integer_value!(u16),
                                    1 => sized_integer_value!(2, I16),
                                    2 if value_embedded => embedded_integer_value!(u32),
                                    2 => sized_integer_value!(4, I32),
                                    3 if value_embedded => embedded_integer_value!(u64),
                                    3 => sized_integer_value!(8, I64),
                                    _ => unreachable!("unsupported integer constant size"),
                                })
                            } else {
                                Ok(instruction_set::Value::IndexedRegister(read_length(stream, || {
                                    InvalidInstructionKind::MissingRegisterIndex
                                })?))
                            }
                        };

                        let integer_arithmetic_operands = |stream: Stream| -> ParseResult<Box<_>> {
                            let mut overflow_behavior_value = 0u8;
                            stream.read_exact(std::slice::from_mut(&mut overflow_behavior_value))?;
                            let overflow_behavior = instruction_set::OverflowBehavior::try_from(overflow_behavior_value)
                                .map_err(|error| invalid_instruction(stream, error.into()))?;

                            Ok(Box::new(instruction_set::IntegerArithmetic::new(
                                overflow_behavior,
                                instruction_value(stream)?,
                                instruction_value(stream)?,
                            )))
                        };

                        let opcode = Opcode::try_from(opcode_value).map_err(|error| stream.error(error.into()))?;
                        Ok(match opcode {
                            Opcode::Nop => Instruction::Nop,
                            Opcode::Break => Instruction::Break,
                            Opcode::Ret => {
                                let return_count = read_length(stream, || InvalidInstructionKind::MissingReturnCount)?;
                                let mut return_values = Vec::with_capacity(return_count);
                                stream
                                    .read_many_to_vec(return_count, &mut return_values, |stream, _| instruction_value(stream))?;
                                Instruction::Ret(return_values.into_boxed_slice())
                            }
                            Opcode::AddI => Instruction::AddI(integer_arithmetic_operands(stream)?),
                            _ => todo!("unsupported opcode {:?} at offset {:#X}", opcode, stream.offset() - 1),
                        })
                    })
                })?;

                Ok(Arc::new(crate::block::Block::new_unchecked(
                    input_register_types.into(),
                    result_register_types.into(),
                    temporary_register_types.into(),
                    instructions.into(),
                )))
            })?;
            Ok(blocks)
        })?
    };

    let get_code_block = |index| -> Result<Arc<crate::block::Block>, _> {
        code_blocks.get(index).cloned().ok_or(ErrorKind::CodeBlockNotFound { index })
    };

    macro_rules! ignore_length {
        ($message: literal) => {{
            let length = src.read_length(|| todo!("missing length"))?;
            if length != 0 {
                todo!($message)
            }
        }};
    }

    ignore_length!("TODO: Parse module imports");

    // TODO: Could keep track of duplicate symbols somehow?
    let mut function_definitions = Vec::<crate::module::DefinedFunction>::new();

    {
        let byte_size = src.read_length(|| ErrorKind::MissingDefinitionSize)?;
        if byte_size > 0 {
            src.parse_buffer(buffer_pool, byte_size, |mut definitions| {
                let function_count = definitions.read_length(|| ErrorKind::MissingFunctionDefinitionCount)?;
                function_definitions.reserve_exact(function_count);
                definitions.read_many_to_vec(function_count, &mut function_definitions, |function, index| {
                    let mut flags_value = 0u8;
                    if function.read(std::slice::from_mut(&mut flags_value))? == 0 {
                        return Err(function.error(ErrorKind::MissingFunctionDefinitionFlags { index }));
                    }

                    let flags = function::Flags::from_bits(flags_value).ok_or_else(|| {
                        function.error(ErrorKind::InvalidFunctionDefinitionFlags {
                            index,
                            value: flags_value,
                        })
                    })?;

                    let export = if flags.contains(function::Flags::EXPORT) {
                        crate::module::Export::Yes
                    } else {
                        crate::module::Export::No
                    };

                    let signature =
                        get_function_signature(function.read_length(|| ErrorKind::MissingFunctionDefinitionSignature { index })?)
                            .map_err(|error| function.error(error))?;

                    let symbol = function.read_identifier()?;

                    let body = if flags.contains(function::Flags::FOREIGN) {
                        todo!("parse foreign func")
                    } else {
                        function::Body::Defined(
                            get_code_block(function.read_length(|| ErrorKind::MissingFunctionDefinitionEntryBlock { index })?)
                                .map_err(|error| function.error(error))?,
                        )
                    };

                    Ok(crate::module::DefinedFunction::new(symbol, signature, export, body))
                })?;
                
                todo!("parse")
            })?;
        }
    }

    ignore_length!("TODO: Parse struct instantiations");
    ignore_length!("TODO: Parse function instantiations");
    // TODO: Parse entry point
    // TODO: Parse initializer
    ignore_length!("TODO: Parse namespaces");
    ignore_length!("TODO: Parse debugging information");

    Ok(crate::module::Module {
        format_version,
        contents: None,
        length_size: src.length_size(),
        name: header.name,
        version: header.version,
        symbols: Default::default(),              // TODO: Build the symbols set earlier.
        function_definitions: Default::default(), // TODO: Build the function definitions list earlier.
    })
}

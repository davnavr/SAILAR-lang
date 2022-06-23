//! Low-level API to read the binary contents of a SAILAR module.

use crate::binary;
use crate::helper::borrow::CowBox;
use crate::identifier;
use crate::index;
use crate::instruction::{self, Instruction, Opcode};
use crate::num::VarIntSize;
use crate::record;
use crate::signature;
use crate::versioning;
use std::borrow::Cow;
use std::fmt::{Display, Formatter};
use std::io::Read;

pub type Record = record::Record<'static>;

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
            crate::helper::buffer::ByteDebug::from(binary::MAGIC),
            crate::helper::buffer::ByteDebug::from(self.actual_bytes()),
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
    #[error("expected reserved integer value")]
    MissingReservedInteger,
    #[error("reserved value is invalid")]
    InvalidReservedValue,
    #[error(transparent)]
    InvalidIntegerSize(#[from] crate::num::InvalidVarIntSize),
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
    #[error("expected tag byte for type signature")]
    MissingTypeSignatureTag,
    #[error(transparent)]
    InvalidTypeSignatureTag(#[from] signature::InvalidTypeCode),
    #[error("expected type signature index")]
    MissingTypeSignatureIndex,
    #[error("expected integer return type count")]
    MissingReturnTypeCount,
    #[error("expected integer parameter type count")]
    MissingParameterTypeCount,
    #[error("expected function signature index")]
    MissingFunctionSignatureIndex,
    #[error("expected data array byte length")]
    MissingDataLength,
    #[error("expected flag byte of function definition")]
    MissingFunctionDefinitionFlags,
    #[error("reserved bits in function definition flags {0:#02X} must not be set")]
    InvalidFunctionDefinitionFlags(u8),
    #[error("expected code block index")]
    MissingCodeBlockIndex,
    #[error("expected identifier index to the foreign function's library name")]
    MissingForeignLibraryName,
    #[error("expected function template index integer")]
    MissingFunctionTemplateIndex,
    #[error("expected integer number of temporary registers")]
    MissingTemporaryRegisterCount,
    #[error("expected integer count of instructions in code block")]
    MissingInstructionCount,
    #[error("expected opcode byte for instruction")]
    MissingInstructionOpcode,
    #[error(transparent)]
    InvalidInstructionOpcode(#[from] instruction::InvalidOpcodeError),
    #[error("expected flag byte for value in instruction")]
    MissingInstructionValueFlags,
    #[error("{0:#02X} is not a valid value flags combination")]
    InvalidInstructionValuesFlags(u8),
    #[error("expected integer register index")]
    MissingRegisterIndex,
    #[error("expected integer count of values for instruction")]
    MissingInstructionValueCount,
    #[error("missing function index for call instruction")]
    MissingInstructionCalleeIndex,
    #[error("unknown constant value kind")]
    InvalidConstantValueKind,
    #[error("expected {expected} bytes for constant value, but got {actual} ")]
    UnexpectedEndOfConstantInteger { expected: usize, actual: usize },
    #[error("expected integer overflow byte")]
    MissingInstructionOverflowValue,
    #[error(transparent)]
    InvalidInstructionOverflowValue(#[from] instruction::InvalidOverflowBehaviorError),
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

    fn into_boxed_wrapper<'a>(self) -> Wrapper<Box<dyn Read + 'a>>
    where
        R: 'a,
    {
        Wrapper {
            source: Box::new(self.source),
            previous_offset: self.previous_offset,
            offset: self.offset,
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

fn select_integer_reader<R: Read>(size: VarIntSize) -> IntegerReader<R> {
    match size {
        VarIntSize::One => |source, error| {
            let mut value = 0u8;
            if source.read_bytes(std::slice::from_mut(&mut value))? == 1 {
                Ok(usize::from(value))
            } else {
                source.fail_with(error())
            }
        },
        VarIntSize::Two => |source, error| {
            let mut buffer = [0u8; 2];
            if source.read_bytes(&mut buffer)? == 2 {
                Ok(usize::from(u16::from_le_bytes(buffer)))
            } else {
                source.fail_with(error())
            }
        },
        VarIntSize::Four => |source, error| {
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

/// Allows the reading of the contents of a SAILAR module from a source.
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

    pub fn into_boxed_reader<'a>(self) -> Reader<Box<dyn Read + 'a>>
    where
        R: 'a,
    {
        Reader {
            source: self.source.into_boxed_wrapper(),
        }
    }

    /// Reads the magic number, format version, and integer size.
    ///
    /// # Examples
    ///
    /// ```
    /// # use sailar::binary::reader::Reader;
    /// let input = "What happens if nonsense is used as input?";
    /// let reader = Reader::new(input.as_bytes());
    /// assert!(matches!(reader.to_record_reader(), Err(_)));
    /// ```
    pub fn to_record_reader(mut self) -> Result<(versioning::SupportedFormat, VarIntSize, RecordReader<R>)> {
        {
            let mut magic_buffer = [0u8; binary::MAGIC.len()];
            let magic_length = self.source.read_bytes(&mut magic_buffer)?;
            if magic_length < magic_buffer.len() {
                return self.source.fail_with(InvalidMagicError::new(&magic_buffer[0..magic_length]));
            }
        }

        let format_version: versioning::SupportedFormat;
        let integer_size: VarIntSize;

        {
            let mut values = [0u8; 3];
            let value_count = self.source.read_bytes(&mut values)?;

            if value_count < 2 {
                return self.source.fail_with(ErrorKind::MissingFormatVersion);
            }

            format_version = self
                .source
                .wrap_result(versioning::SupportedFormat::try_from(versioning::Format {
                    major: values[0],
                    minor: values[1],
                }))?;

            if value_count < 3 {
                return self.source.fail_with(ErrorKind::MissingIntegerSize);
            }

            integer_size = self.source.wrap_result(VarIntSize::try_from(values[2]))?;
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

type RecordContentReader = for<'c, 'b> fn(&'c mut Wrapper<&'b [u8]>, IntegerReader<&'b [u8]>) -> Result<Record>;

struct ArrayRecordReader {
    element_count: usize,
    element_reader: RecordContentReader,
    element_buffer: Box<[u8]>,
    element_buffer_offset: usize,
}

impl ArrayRecordReader {
    fn new(count: usize, buffer: Box<[u8]>, reader: RecordContentReader) -> Self {
        Self {
            element_count: count,
            element_reader: reader,
            element_buffer: buffer,
            element_buffer_offset: 0,
        }
    }

    // TODO: Could reuse element_buffer across iterations.
    // fn reset_buffer(&mut self, contents: &[u8]) {
    //     let mut buffer = std::mem::take(&mut self.element_buffer).into_vec();
    //     buffer.clear();
    //     buffer.extend_from_slice(contents);
    //     self.element_buffer = buffer.into_boxed_slice();
    // }

    fn read_next<'a>(&'a mut self, integer_reader: IntegerReader<&'a [u8]>) -> Option<Result<Record>> {
        if self.element_count > 0 {
            let mut wrapper = Wrapper::new(&self.element_buffer[self.element_buffer_offset..]);
            let record = (self.element_reader)(&mut wrapper, integer_reader);
            let element_size = wrapper.offset;
            self.element_count -= 1;
            self.element_buffer_offset += element_size;
            Some(record)
        } else {
            None
        }
    }
}

/// Reads the records of a SAILAR module.
pub struct RecordReader<R> {
    count: usize,
    source: Wrapper<R>,
    integer_size: VarIntSize,
    integer_reader: IntegerReader<R>,
    array_reader: Option<ArrayRecordReader>,
    buffer: Vec<u8>,
}

impl<R: Read> RecordReader<R> {
    fn new(source: Wrapper<R>, integer_size: VarIntSize, integer_reader: IntegerReader<R>, count: usize) -> Self {
        Self {
            count,
            source,
            integer_size,
            integer_reader,
            array_reader: None,
            buffer: Vec::default(),
        }
    }

    /// Returns the remaining number of records in the module.
    #[inline]
    pub fn record_count(&self) -> usize {
        self.count
    }

    /// Consumes bytes in the input, returning the parsed record, or `None` if an empty array record is encountered.
    ///
    /// When an array record is encountered, only the first element of the array is read. Further elements should be read using
    /// the `array_reader`.
    ///
    /// # Errors
    ///
    /// Returns `Some(Err(_))` when invalid input is encountered, or if an error occurs during reading.
    fn read_record(&mut self) -> Result<Option<Record>> {
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

        fn read_type_signature<'b>(source: &mut BufferWrapper<'b>, integer_reader: IntegerReader<&'b [u8]>) -> Result<Record> {
            let mut tag_value = 0u8;
            if source.read_bytes(std::slice::from_mut(&mut tag_value))? == 0 {
                return source.fail_with(ErrorKind::MissingRecordType);
            }

            Ok(Record::from(
                match source.wrap_result(signature::TypeCode::try_from(tag_value))? {
                    signature::TypeCode::U8 => signature::Type::U8,
                    signature::TypeCode::S8 => signature::Type::S8,
                    signature::TypeCode::U16 => signature::Type::U16,
                    signature::TypeCode::S16 => signature::Type::S16,
                    signature::TypeCode::U32 => signature::Type::U32,
                    signature::TypeCode::S32 => signature::Type::S32,
                    signature::TypeCode::U64 => signature::Type::U64,
                    signature::TypeCode::S64 => signature::Type::S64,
                    signature::TypeCode::UAddr => signature::Type::UAddr,
                    signature::TypeCode::SAddr => signature::Type::SAddr,
                    signature::TypeCode::F32 => signature::Type::F32,
                    signature::TypeCode::F64 => signature::Type::F64,
                    signature::TypeCode::VoidPtr => signature::Type::RawPtr(None),
                    signature::TypeCode::RawPtr => {
                        signature::Type::RawPtr(Some(integer_reader(source, || ErrorKind::MissingTypeSignatureIndex)?.into()))
                    }
                    signature::TypeCode::FuncPtr => {
                        signature::Type::FuncPtr(integer_reader(source, || ErrorKind::MissingFunctionSignatureIndex)?.into())
                    }
                },
            ))
        }

        fn read_function_signature<'b>(
            source: &mut BufferWrapper<'b>,
            integer_reader: IntegerReader<&'b [u8]>,
        ) -> Result<Record> {
            let return_type_count = (integer_reader)(source, || ErrorKind::MissingReturnTypeCount)?;
            let parameter_type_count = (integer_reader)(source, || ErrorKind::MissingParameterTypeCount)?;
            let total_count = return_type_count + parameter_type_count;
            let mut types = Vec::with_capacity(total_count);
            for _ in 0..total_count {
                types.push(index::TypeSignature::from((integer_reader)(source, || {
                    ErrorKind::MissingTypeSignatureIndex
                })?));
            }

            Ok(Record::from(signature::Function::from_boxed_slice(
                types.into_boxed_slice(),
                return_type_count,
            )))
        }

        fn read_code_block<'b>(source: &mut BufferWrapper<'b>, integer_reader: IntegerReader<&'b [u8]>) -> Result<Record> {
            let read_code_value = |source: &mut BufferWrapper<'b>| -> Result<instruction::Value> {
                let mut flag_value = 0u8;
                if source.read_bytes(std::slice::from_mut(&mut flag_value))? == 0 {
                    return source.fail_with(ErrorKind::MissingInstructionValueFlags);
                }

                let flags = source.wrap_result(
                    instruction::ValueFlags::from_bits(flag_value).ok_or(ErrorKind::InvalidInstructionValuesFlags(flag_value)),
                )?;

                if !flags.contains(instruction::ValueFlags::IS_CONSTANT) {
                    (integer_reader)(source, || ErrorKind::MissingRegisterIndex).map(|i| index::Register::from(i).into())
                } else {
                    if !flags.contains(instruction::ValueFlags::IS_INTEGER) {
                        return source.fail_with(ErrorKind::InvalidConstantValueKind);
                    }

                    let embedded_constant_size = (flags & instruction::ValueFlags::INTEGER_SIZE_MASK).bits() >> 2;

                    Ok(if flags.contains(instruction::ValueFlags::INTEGER_IS_EMBEDDED) {
                        if flags.contains(instruction::ValueFlags::INTEGER_EMBEDDED_ONE) {
                            match embedded_constant_size {
                                0 => instruction::Value::from(1u8),
                                1 => instruction::Value::from(1u16),
                                2 => instruction::Value::from(1u32),
                                3 => instruction::Value::from(1u64),
                                _ => unreachable!(),
                            }
                        } else {
                            match embedded_constant_size {
                                0 => instruction::Value::from(0u8),
                                1 => instruction::Value::from(0u16),
                                2 => instruction::Value::from(0u32),
                                3 => instruction::Value::from(0u64),
                                _ => unreachable!(),
                            }
                        }
                    } else {
                        fn read_constant_bytes<const N: usize, T>(
                            source: &mut BufferWrapper,
                            conversion: fn([u8; N]) -> T,
                        ) -> Result<T> {
                            let mut bytes = [0u8; N];
                            let actual_size = source.read_bytes(&mut bytes)?;
                            if actual_size != N {
                                return source.fail_with(ErrorKind::UnexpectedEndOfConstantInteger {
                                    expected: N,
                                    actual: actual_size,
                                });
                            }
                            Ok(conversion(bytes))
                        }

                        match embedded_constant_size {
                            0 => instruction::Value::from(read_constant_bytes(source, u8::from_le_bytes)?),
                            1 => instruction::Value::from(read_constant_bytes(source, u16::from_le_bytes)?),
                            2 => instruction::Value::from(read_constant_bytes(source, u32::from_le_bytes)?),
                            3 => instruction::Value::from(read_constant_bytes(source, u64::from_le_bytes)?),
                            _ => unreachable!(),
                        }
                    })
                }
            };

            let read_many_code_values = |source: &mut BufferWrapper<'b>| -> Result<Box<[instruction::Value]>> {
                let count = (integer_reader)(source, || ErrorKind::MissingInstructionValueCount)?;
                let mut values = Vec::with_capacity(count);
                for _ in 0..count {
                    values.push(read_code_value(source)?);
                }
                Ok(values.into_boxed_slice())
            };

            let read_integer_arithmteic = |source: &mut BufferWrapper<'b>| -> Result<Box<instruction::IntegerArithmetic>> {
                let mut overflow_value = 0u8;
                if source.read_bytes(std::slice::from_mut(&mut overflow_value))? == 0 {
                    return Err(source.wrap_error(ErrorKind::MissingInstructionOverflowValue));
                }

                Ok(Box::new(instruction::IntegerArithmetic::new(
                    source.wrap_result(instruction::OverflowBehavior::try_from(overflow_value))?,
                    read_code_value(source)?,
                    read_code_value(source)?,
                )))
            };

            let read_instruction = |source: &mut BufferWrapper<'b>| -> Result<Instruction> {
                let mut opcode_value = 0u8;
                if source.read_bytes(std::slice::from_mut(&mut opcode_value))? == 0 {
                    return source.fail_with(ErrorKind::MissingInstructionOpcode);
                }

                Ok(match source.wrap_result(Opcode::try_from(opcode_value))? {
                    Opcode::Nop => Instruction::Nop,
                    Opcode::Break => Instruction::Break,
                    Opcode::Ret => Instruction::Ret(read_many_code_values(source)?),
                    Opcode::Call => Instruction::Call(
                        (integer_reader)(source, || ErrorKind::MissingInstructionCalleeIndex)?.into(),
                        read_many_code_values(source)?,
                    ),
                    Opcode::AddI => Instruction::AddI(read_integer_arithmteic(source)?),
                    Opcode::SubI => Instruction::SubI(read_integer_arithmteic(source)?),
                })
            };

            let input_count = (integer_reader)(source, || ErrorKind::MissingParameterTypeCount)?;
            let result_count = (integer_reader)(source, || ErrorKind::MissingReturnTypeCount)?;
            let temporary_count = (integer_reader)(source, || ErrorKind::MissingTemporaryRegisterCount)?;
            let register_count = input_count + result_count + temporary_count;
            let mut register_types = Vec::<index::TypeSignature>::with_capacity(register_count);
            for _ in 0..register_count {
                register_types.push((integer_reader)(source, || ErrorKind::MissingTypeSignatureIndex)?.into());
            }

            let instruction_count = (integer_reader)(source, || ErrorKind::MissingInstructionCount)?;
            let mut instructions = Vec::with_capacity(instruction_count);
            for _ in 0..instruction_count {
                instructions.push(read_instruction(source)?);
            }

            Ok(Record::from(record::CodeBlock::from_types(
                CowBox::Boxed(register_types.into_boxed_slice()),
                input_count,
                result_count,
                CowBox::Boxed(instructions.into_boxed_slice()),
            )))
        }

        fn read_function_definition<'b>(
            source: &mut BufferWrapper<'b>,
            integer_reader: IntegerReader<&'b [u8]>,
        ) -> Result<Record> {
            let mut flags_value = 0u8;
            if source.read_bytes(std::slice::from_mut(&mut flags_value))? == 0 {
                return source.fail_with(ErrorKind::MissingRecordType);
            }

            if flags_value & 0b1111_1100u8 != 0u8 {
                return source.fail_with(ErrorKind::InvalidFunctionDefinitionFlags(flags_value));
            }

            let export = if flags_value & 1 == 1 {
                record::Export::Public
            } else {
                record::Export::Private
            };

            let reserved = (integer_reader)(source, || ErrorKind::MissingReservedInteger)?;

            if reserved != 0 {
                return source.fail_with(ErrorKind::InvalidReservedValue);
            }

            let signature = (integer_reader)(source, || ErrorKind::MissingFunctionSignatureIndex)?;
            let symbol = Cow::Owned(read_identifier(source, integer_reader)?);

            let body = if flags_value & 0b10 != 0b10 {
                record::FunctionBody::Definition(integer_reader(source, || ErrorKind::MissingCodeBlockIndex)?.into())
            } else {
                record::FunctionBody::Foreign {
                    library: integer_reader(source, || ErrorKind::MissingForeignLibraryName)?.into(),
                    entry_point: Cow::Owned(read_identifier(source, integer_reader)?),
                }
            };

            Ok(Record::from(record::FunctionDefinition::new(
                export,
                signature.into(),
                symbol,
                body,
            )))
        }

        fn read_function_instantiation<'b>(
            source: &mut BufferWrapper<'b>,
            integer_reader: IntegerReader<&'b [u8]>,
        ) -> Result<Record> {
            let template = (integer_reader)(source, || ErrorKind::MissingFunctionTemplateIndex)?.into();
            Ok(Record::from(record::FunctionInstantiation::from_template(template)))
        }

        self.count -= 1;

        match record_type {
            record::Type::Array => {
                let array_type = read_record_type(content)?;
                let array_count = content_integer_reader(content, || ErrorKind::MissingRecordArrayCount)?;
                let array_elements = content.source.to_vec().into_boxed_slice();

                let array_reader = self.array_reader.insert(ArrayRecordReader::new(
                    array_count,
                    array_elements,
                    match array_type {
                        record::Type::Identifier => |src, int_reader| read_identifier(src, int_reader).map(Record::from),
                        record::Type::TypeSignature => read_type_signature,
                        record::Type::Data => |source: &mut BufferWrapper, integer_reader: IntegerReader<_>| {
                            let data_size = integer_reader(source, || ErrorKind::MissingDataLength)?;
                            let mut buffer = vec![0u8; data_size];
                            let actual_size = source.read_bytes(&mut buffer)?;
                            if actual_size < data_size {
                                return Err(source.wrap_error(ErrorKind::UnexpectedEndOfData {
                                    name: "data array",
                                    actual_size,
                                    expected_size: data_size,
                                }));
                            }
                            Ok(Record::Data(Cow::Owned(buffer.into_boxed_slice())))
                        },
                        _ => todo!("add support for array record type {:?}", array_type),
                    },
                ));

                match array_reader.read_next(content_integer_reader) {
                    Some(first_element) => first_element.map(Some),
                    None => Ok(None),
                }
            }
            record::Type::Identifier => read_identifier_content(content, record_size).map(|id| Some(Record::from(id))),
            record::Type::TypeSignature => read_type_signature(content, content_integer_reader).map(Some),
            record::Type::FunctionSignature => read_function_signature(content, content_integer_reader).map(Some),
            record::Type::Data => Ok(Some(Record::Data(Cow::Owned(content.source.to_vec().into_boxed_slice())))),
            record::Type::CodeBlock => read_code_block(content, content_integer_reader).map(Some),
            record::Type::FunctionDefinition => read_function_definition(content, content_integer_reader).map(Some),
            record::Type::FunctionInstantiation => read_function_instantiation(content, content_integer_reader).map(Some),
            _ => todo!("parse a {:?}", record_type),
        }
    }

    // TODO: Reader could return index of record (e.g. TypeSignature index for TypeSignatures, etc.)
    /// Returns the next record in the module, or `None` if no records remain in the module.
    pub fn next_record(&mut self) -> Option<Result<Record>> {
        match self.array_reader {
            None => (),
            Some(ref mut array_reader) => {
                let element = array_reader.read_next(select_integer_reader(self.integer_size));
                if element.is_some() {
                    return element;
                } else {
                    self.array_reader = None;
                }
            }
        }

        // Read records, skipping empty arrays
        loop {
            if self.count == 0 {
                return None;
            }

            let result = self.read_record();
            match result {
                Ok(Some(record)) => break Some(Ok(record)),
                Ok(None) => continue,
                Err(err) => break Some(Err(err)),
            }
        }
    }

    pub fn next_record_transposed(&mut self) -> Result<Option<Record>> {
        self.next_record().transpose()
    }

    /// Skips over any remaining records in the module, and checks that there are no remaining bytes in the input.
    ///
    /// # Errors
    /// If there are still remaining bytes in the input after the module records, then an error is returned.
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn array_of_data_records_is_parsed() {
        let module = [
            b'S',
            b'A',
            b'I',
            b'L',
            b'A',
            b'R',
            versioning::SupportedFormat::CURRENT.major,
            versioning::SupportedFormat::CURRENT.minor,
            0,
            1, // Number of records
            1,
            14,
            5,
            2,
            6,
            0xA,
            0xB,
            0xC,
            0xD,
            0xE,
            0xF,
            4,
            b'T',
            b'E',
            b'S',
            b'T',
        ];

        let reader = Reader::new(module.as_slice());
        let (_, _, mut record_reader) = reader.to_record_reader().unwrap();

        assert!(
            matches!(record_reader.next_record_transposed().unwrap(), Some(Record::Data(bytes)) if bytes.as_ref() == &[0xA, 0xB, 0xC, 0xD, 0xE, 0xF])
        );

        assert!(
            matches!(record_reader.next_record_transposed().unwrap(), Some(Record::Data(bytes)) if bytes.as_ref() == &[b'T', b'E', b'S', b'T'])
        );

        record_reader.finish().unwrap();
    }
}

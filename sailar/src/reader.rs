//! Low-level API to read the binary contents of a SAILAR module.

use crate::binary;
use crate::helper::borrow::CowBox;
use crate::identifier;
use crate::index;
use crate::instruction::{self, Instruction, Opcode};
use crate::num::VarU28;
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
    #[error("expected reserved integer value")]
    MissingReservedInteger,
    #[error("reserved value is invalid")]
    InvalidReservedValue,
    #[error("expected record count")]
    MissingRecordCount,
    #[error(transparent)]
    IntegerLengthTooLarge(#[from] crate::num::IntegerLengthError),
    #[error(transparent)]
    IntegerConversionError(#[from] std::num::TryFromIntError),
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
    #[error("{0:#02X} is not a valid function definition flags combination")]
    InvalidFunctionDefinitionFlags(VarU28),
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

    fn read_unsigned_integer(&mut self, error: fn() -> ErrorKind) -> Result<VarU28> {
        self.previous_offset = self.offset;
        match VarU28::read_from(&mut self.source) {
            Ok(Ok(value)) => Ok(value),
            Ok(Err(err)) => Err(self.wrap_error(err)),
            Err(_) => Err(self.wrap_error(error())),
        }
    }

    fn read_unsigned_integer_try_into<T>(&mut self, error: fn() -> ErrorKind) -> Result<T>
    where
        T: TryFrom<VarU28>,
        T::Error: Into<ErrorKind>,
    {
        let value = self.read_unsigned_integer(error)?;
        self.wrap_result(T::try_from(value))
    }
}

type BufferWrapper<'b> = Wrapper<&'b [u8]>;

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

    /// Reads the magic number and format version, returning a [`RecordReader`] to read the contents of the module.
    ///
    /// # Examples
    ///
    /// ```
    /// # use sailar::reader::Reader;
    /// let input = "What happens if nonsense is used as input?";
    /// let reader = Reader::new(input.as_bytes());
    /// assert!(matches!(reader.to_record_reader(), Err(_)));
    /// ```
    pub fn to_record_reader(mut self) -> Result<(versioning::SupportedFormat, RecordReader<R>)> {
        {
            let mut magic_buffer = [0u8; binary::MAGIC.len()];
            let magic_length = self.source.read_bytes(&mut magic_buffer)?;
            if magic_length < magic_buffer.len() {
                return self.source.fail_with(InvalidMagicError::new(&magic_buffer[0..magic_length]));
            }
        }

        let format_version = {
            let mut values = [0u8; 2];
            let value_count = self.source.read_bytes(&mut values)?;

            if value_count < 2 {
                return self.source.fail_with(ErrorKind::MissingFormatVersion);
            }

            self.source
                .wrap_result(versioning::SupportedFormat::try_from(versioning::Format {
                    major: values[0],
                    minor: values[1],
                }))?
        };

        let record_count = self.source.read_unsigned_integer_try_into(|| ErrorKind::MissingRecordCount)?;

        Ok((format_version, RecordReader::new(self.source, record_count)))
    }
}

impl<R: Read> From<R> for Reader<R> {
    #[inline]
    fn from(source: R) -> Self {
        Self::new(source)
    }
}

type RecordContentReader = for<'c, 'b> fn(&'c mut Wrapper<&'b [u8]>) -> Result<Record>;

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

    fn read_next(&mut self) -> Option<Result<Record>> {
        if self.element_count > 0 {
            let mut wrapper = Wrapper::new(&self.element_buffer[self.element_buffer_offset..]);
            let record = (self.element_reader)(&mut wrapper);
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
    array_reader: Option<ArrayRecordReader>,
    buffer: Vec<u8>,
}

impl<R: Read> RecordReader<R> {
    fn new(source: Wrapper<R>, count: usize) -> Self {
        Self {
            count,
            source,
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
        let record_size = self.source.read_unsigned_integer_try_into(|| ErrorKind::MissingRecordSize)?;

        // TODO: Is repeated stack allocations really ok here? Since self.buffer already exists, why not use it? Maybe shrink this to 32 or something?
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

        fn read_identifier(source: &mut BufferWrapper<'_>) -> Result<identifier::Identifier> {
            let length = source.read_unsigned_integer_try_into(|| ErrorKind::MissingIdentifierLength)?;
            read_identifier_content(source, length)
        }

        fn read_type_signature(source: &mut BufferWrapper<'_>) -> Result<Record> {
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
                    signature::TypeCode::RawPtr => signature::Type::RawPtr(Some(
                        source.read_unsigned_integer_try_into(|| ErrorKind::MissingTypeSignatureIndex)?,
                    )),
                    signature::TypeCode::FuncPtr => signature::Type::FuncPtr(
                        source.read_unsigned_integer_try_into(|| ErrorKind::MissingFunctionSignatureIndex)?,
                    ),
                },
            ))
        }

        fn read_function_signature(source: &mut BufferWrapper<'_>) -> Result<Record> {
            let return_type_count = source.read_unsigned_integer_try_into(|| ErrorKind::MissingReturnTypeCount)?;
            let parameter_type_count: usize = source.read_unsigned_integer_try_into(|| ErrorKind::MissingParameterTypeCount)?;
            let total_count = return_type_count + parameter_type_count;
            let mut types = Vec::<index::TypeSignature>::with_capacity(total_count);
            for _ in 0..total_count {
                types.push(source.read_unsigned_integer_try_into(|| ErrorKind::MissingTypeSignatureIndex)?);
            }

            Ok(Record::from(signature::Function::from_boxed_slice(
                types.into_boxed_slice(),
                return_type_count,
            )))
        }

        fn read_code_block(source: &mut BufferWrapper<'_>) -> Result<Record> {
            let read_code_value = |source: &mut BufferWrapper<'_>| -> Result<instruction::Value> {
                let mut flag_value = 0u8;
                if source.read_bytes(std::slice::from_mut(&mut flag_value))? == 0 {
                    return source.fail_with(ErrorKind::MissingInstructionValueFlags);
                }

                let flags = source.wrap_result(
                    instruction::ValueFlags::from_bits(flag_value).ok_or(ErrorKind::InvalidInstructionValuesFlags(flag_value)),
                )?;

                if !flags.contains(instruction::ValueFlags::IS_CONSTANT) {
                    Ok(source
                        .read_unsigned_integer_try_into::<index::Register>(|| ErrorKind::MissingRegisterIndex)?
                        .into())
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

            let read_many_code_values = |source: &mut BufferWrapper<'_>| -> Result<Box<[instruction::Value]>> {
                let count = source.read_unsigned_integer_try_into(|| ErrorKind::MissingInstructionValueCount)?;
                let mut values = Vec::with_capacity(count);
                for _ in 0..count {
                    values.push(read_code_value(source)?);
                }
                Ok(values.into_boxed_slice())
            };

            let read_integer_arithmteic = |source: &mut BufferWrapper<'_>| -> Result<Box<instruction::IntegerArithmetic>> {
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

            let read_instruction = |source: &mut BufferWrapper<'_>| -> Result<Instruction> {
                let mut opcode_value = 0u8;
                if source.read_bytes(std::slice::from_mut(&mut opcode_value))? == 0 {
                    return source.fail_with(ErrorKind::MissingInstructionOpcode);
                }

                Ok(match source.wrap_result(Opcode::try_from(opcode_value))? {
                    Opcode::Nop => Instruction::Nop,
                    Opcode::Break => Instruction::Break,
                    Opcode::Ret => Instruction::Ret(read_many_code_values(source)?),
                    Opcode::Call => Instruction::Call(
                        source.read_unsigned_integer_try_into(|| ErrorKind::MissingInstructionCalleeIndex)?,
                        read_many_code_values(source)?,
                    ),
                    Opcode::AddI => Instruction::AddI(read_integer_arithmteic(source)?),
                    Opcode::SubI => Instruction::SubI(read_integer_arithmteic(source)?),
                })
            };

            let input_count = source.read_unsigned_integer_try_into(|| ErrorKind::MissingParameterTypeCount)?;
            let result_count: usize = source.read_unsigned_integer_try_into(|| ErrorKind::MissingReturnTypeCount)?;
            let temporary_count: usize = source.read_unsigned_integer_try_into(|| ErrorKind::MissingTemporaryRegisterCount)?;
            let register_count = input_count + result_count + temporary_count;
            let mut register_types = Vec::<index::TypeSignature>::with_capacity(register_count);
            for _ in 0..register_count {
                register_types.push(source.read_unsigned_integer_try_into(|| ErrorKind::MissingTypeSignatureIndex)?);
            }

            let instruction_count = source.read_unsigned_integer_try_into(|| ErrorKind::MissingInstructionCount)?;
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

        fn read_function_definition(source: &mut BufferWrapper<'_>) -> Result<Record> {
            let flag_bits = source.read_unsigned_integer(|| ErrorKind::MissingFunctionDefinitionFlags)?;
            let flags = source.wrap_result(
                record::FunctionDefinitionFlags::try_from_bits(flag_bits)
                    .ok_or(ErrorKind::InvalidFunctionDefinitionFlags(flag_bits)),
            )?;

            let export = match flags.export_kind() {
                record::ExportKind::Hidden => record::Export::Hidden,
                record::ExportKind::Private => record::Export::Private(Cow::Owned(read_identifier(source)?)),
                record::ExportKind::Export => record::Export::Export(Cow::Owned(read_identifier(source)?)),
            };

            let signature = source.read_unsigned_integer_try_into(|| ErrorKind::MissingFunctionSignatureIndex)?;

            let body = if !flags.is_body_foreign() {
                record::FunctionBody::Definition(source.read_unsigned_integer_try_into(|| ErrorKind::MissingCodeBlockIndex)?)
            } else {
                record::FunctionBody::Foreign {
                    library: source.read_unsigned_integer_try_into(|| ErrorKind::MissingForeignLibraryName)?,
                    entry_point: Cow::Owned(read_identifier(source)?),
                }
            };

            Ok(Record::from(record::FunctionDefinition::new(export, signature, body)))
        }

        fn read_function_instantiation(source: &mut BufferWrapper<'_>) -> Result<Record> {
            source
                .read_unsigned_integer_try_into(|| ErrorKind::MissingFunctionTemplateIndex)
                .map(|index| Record::from(record::FunctionInstantiation::from_template(index)))
        }

        self.count -= 1;

        match record_type {
            record::Type::Array => {
                let array_type = read_record_type(content)?;
                let array_count = content.read_unsigned_integer_try_into(|| ErrorKind::MissingRecordArrayCount)?;
                let array_elements = content.source.to_vec().into_boxed_slice();

                let array_reader = self.array_reader.insert(ArrayRecordReader::new(
                    array_count,
                    array_elements,
                    match array_type {
                        record::Type::Identifier => |src| read_identifier(src).map(Record::from),
                        record::Type::TypeSignature => read_type_signature,
                        record::Type::Data => |source: &mut BufferWrapper| {
                            let data_size = source.read_unsigned_integer_try_into(|| ErrorKind::MissingDataLength)?;
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

                match array_reader.read_next() {
                    Some(first_element) => first_element.map(Some),
                    None => Ok(None),
                }
            }
            record::Type::Identifier => read_identifier_content(content, record_size).map(|id| Some(Record::from(id))),
            record::Type::TypeSignature => read_type_signature(content).map(Some),
            record::Type::FunctionSignature => read_function_signature(content).map(Some),
            record::Type::Data => Ok(Some(Record::Data(Cow::Owned(content.source.to_vec().into_boxed_slice())))),
            record::Type::CodeBlock => read_code_block(content).map(Some),
            record::Type::FunctionDefinition => read_function_definition(content).map(Some),
            record::Type::FunctionInstantiation => read_function_instantiation(content).map(Some),
            _ => todo!("parse a {:?}", record_type),
        }
    }

    // TODO: Reader could return index of record (e.g. TypeSignature index for TypeSignatures, etc.)
    /// Returns the next record in the module, or `None` if no records remain in the module.
    pub fn next_record(&mut self) -> Option<Result<Record>> {
        match self.array_reader {
            None => (),
            Some(ref mut array_reader) => {
                let element = array_reader.read_next();
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
        let (_, mut record_reader) = reader.to_record_reader().unwrap();

        assert!(
            matches!(record_reader.next_record_transposed().unwrap(), Some(Record::Data(bytes)) if bytes.as_ref() == &[0xA, 0xB, 0xC, 0xD, 0xE, 0xF])
        );

        assert!(
            matches!(record_reader.next_record_transposed().unwrap(), Some(Record::Data(bytes)) if bytes.as_ref() == &[b'T', b'E', b'S', b'T'])
        );

        record_reader.finish().unwrap();
    }
}

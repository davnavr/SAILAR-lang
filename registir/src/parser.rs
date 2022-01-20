use crate::{
    buffers,
    format::{
        self,
        flags::{self, ExportFlag as _},
        instruction_set::{self, Instruction, Opcode},
        numeric, type_system,
    },
};
use std::io::Read;

#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    InvalidModuleMagic,
    InvalidIntegerSize(u8),
    UnsupportedFormatVersion(format::FormatVersion),
    InvalidDataVectorCount(numeric::UInteger),
    InvalidByteLength { expected: usize, actual: usize },
    InvalidHeaderFieldCount(numeric::UInteger),
    InvalidIdentifierCharacter(std::str::Utf8Error),
    EmptyIdentifier,
    InvalidNamespaceFlags(u8),
    InvalidTypeSignatureTag(u8),
    InvalidPrimitiveType(u8),
    InvalidIntegerConstantType(type_system::PrimitiveType),
    InvalidCodeBlockFlags(u8),
    InvalidOpcode(u32),
    InvalidArithmeticFlags(u8),
    InvalidNumericType(u8),
    InvalidCallFlags(u8),
    InvalidVisibilityFlags(u8),
    InvalidStructFlags(u8),
    InvalidFunctionFlags(u8),
    InvalidStructLayoutFlags(u8),
    InputOutputError(std::io::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidModuleMagic => {
                f.write_str("the file magic indicates that it is not a valid binary module")
            }
            Self::InvalidIntegerSize(value) => {
                write!(f, "{:#02X} is not a valid integer size value", value)
            }
            Self::UnsupportedFormatVersion(version) => write!(
                f,
                "The format version {}.{} is not supported",
                version.major, version.minor
            ),
            Self::InvalidDataVectorCount(count) => {
                write!(f, "{} is not a valid data vector count", count)
            }
            Self::InvalidByteLength { expected, actual } => {
                write!(f, "expected to read {} bytes but got {}", expected, actual)
            }
            Self::InvalidHeaderFieldCount(count) => write!(
                f,
                "{} is not a valid number of fields for the module header",
                count
            ),
            Self::EmptyIdentifier => f.write_str("Identifiers must not be empty"),
            Self::InvalidNamespaceFlags(flags) => write!(
                f,
                "{:#02X} is not a valid combination of namespace flags",
                flags
            ),
            Self::InvalidIdentifierCharacter(error) => error.fmt(f),
            Self::InvalidTypeSignatureTag(tag) => {
                write!(f, "{:#02X} is not a valid type signature tag", tag)
            }
            Self::InvalidPrimitiveType(tag) => {
                write!(f, "{:#02X} is not a valid primitive type", tag)
            }
            Self::InvalidIntegerConstantType(tag) => {
                write!(f, "{:?} is not a constant integer type", tag)
            }
            Self::InvalidCodeBlockFlags(flags) => write!(
                f,
                "{:#02X} is not a valid combination of code block flags",
                flags
            ),
            Self::InvalidOpcode(opcode) => write!(f, "{} is not a valid opcode", opcode),
            Self::InvalidArithmeticFlags(flags) => {
                write!(
                    f,
                    "{:#02X} is not a valid arithmetic flags combination",
                    flags
                )
            }
            Self::InvalidNumericType(value) => {
                write!(f, "{:#02X} is not a valid numeric type", value)
            }
            Self::InvalidCallFlags(flags) => {
                write!(f, "{:#02X} is not a valid call flags combination", flags)
            }
            Self::InvalidVisibilityFlags(flag) => {
                write!(f, "{:#02X} is not a valid visibility value", flag)
            }
            Self::InvalidStructFlags(flag) => {
                write!(f, "{:#02X} is not a valid struct flags combination", flag)
            }
            Self::InvalidFunctionFlags(flag) => {
                write!(f, "{:#02X} is not a valid function flags combination", flag)
            }
            Self::InvalidStructLayoutFlags(flag) => {
                write!(f, "{:#02X} is not a valid struct layout", flag)
            }
            Self::InputOutputError(error) => error.fmt(f),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::InputOutputError(error) => Some(error),
            _ => None,
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

fn fill_buffer(src: &mut impl Read, buffer: &mut [u8]) -> Result<()> {
    let count = src.read(buffer).map_err(Error::InputOutputError)?;
    if count == buffer.len() {
        Ok(())
    } else {
        Err(Error::InvalidByteLength {
            expected: buffer.len(),
            actual: count,
        })
    }
}

fn fixed_bytes<R: Read, const L: usize>(src: &mut R) -> Result<[u8; L]> {
    let mut buffer = [0u8; L];
    fill_buffer(src, &mut buffer).map(|()| buffer)
}

fn byte<R: Read>(src: &mut R) -> Result<u8> {
    fixed_bytes::<R, 1>(src).map(|bytes| bytes[0])
}

fn many_bytes<R: Read>(src: &mut R, length: usize) -> Result<Vec<u8>> {
    let mut buffer = vec![0u8; length];
    fill_buffer(src, buffer.as_mut_slice()).map(|()| buffer)
}

fn unsigned_integer<R: Read>(src: &mut R, size: numeric::IntegerSize) -> Result<numeric::UInteger> {
    match size {
        numeric::IntegerSize::I1 => byte(src).map(numeric::UInteger::from),
        numeric::IntegerSize::I2 => fixed_bytes::<R, 2>(src)
            .map(|buffer| numeric::UInteger::from(u16::from_le_bytes(buffer))),
        numeric::IntegerSize::I4 => {
            fixed_bytes::<R, 4>(src).map(|buffer| numeric::UInteger(u32::from_le_bytes(buffer)))
        }
    }
}

fn unsigned_length<R: Read>(src: &mut R, size: numeric::IntegerSize) -> Result<usize> {
    unsigned_integer(src, size).map(|value| usize::try_from(value).unwrap())
}

fn unsigned_index<I: From<numeric::UInteger>, R: Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> Result<I> {
    unsigned_integer(src, size).map(I::from)
}

fn identifier<R: Read>(
    src: &mut R,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
) -> Result<format::Identifier> {
    let length = unsigned_length(src, size)?;
    let mut buffer = buffer_pool.rent_with_length(length);
    fill_buffer(src, &mut buffer)?;
    std::str::from_utf8(&buffer)
        .map_err(Error::InvalidIdentifierCharacter)
        .and_then(|s| format::Identifier::try_from(s).map_err(|_| Error::EmptyIdentifier))
}

fn length_encoded_vector<T, P: FnMut(&mut R) -> Result<T>, R: Read>(
    src: &mut R,
    size: numeric::IntegerSize,
    mut parser: P,
) -> Result<format::LenVec<T>> {
    let length = unsigned_length(src, size)?;
    let mut buffer = Vec::<T>::with_capacity(length);

    for _ in 0..length {
        let item = parser(src)?;
        buffer.push(item);
    }

    Ok(format::LenVec(buffer))
}

fn length_encoded_indices<I: From<numeric::UInteger>, R: Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> Result<format::LenVec<I>> {
    length_encoded_vector(src, size, |src| unsigned_integer(src, size).map(I::from))
}

fn version_numbers<R: Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> Result<format::VersionNumbers> {
    length_encoded_vector(src, size, |src| unsigned_integer(src, size)).map(format::VersionNumbers)
}

fn module_identifier<R: Read>(
    src: &mut R,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
) -> Result<format::ModuleIdentifier> {
    Ok(format::ModuleIdentifier {
        name: identifier(src, size, buffer_pool)?,
        version: version_numbers(src, size)?,
    })
}

fn module_header<R: Read>(
    src: &mut R,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
) -> Result<format::ModuleHeader> {
    let field_count = unsigned_integer(src, size)?;

    if field_count < format::MIN_HEADER_FIELD_COUNT || field_count > format::MAX_HEADER_FIELD_COUNT
    {
        return Err(Error::InvalidHeaderFieldCount(field_count));
    }

    let id = module_identifier(src, size, buffer_pool)?;

    Ok(format::ModuleHeader { identifier: id })
}

fn primitive_type(tag: type_system::TypeTag) -> Option<type_system::PrimitiveType> {
    match tag {
        type_system::TypeTag::S8 => Some(type_system::PrimitiveType::S8),
        type_system::TypeTag::U8 => Some(type_system::PrimitiveType::U8),
        type_system::TypeTag::S16 => Some(type_system::PrimitiveType::S16),
        type_system::TypeTag::U16 => Some(type_system::PrimitiveType::U16),
        type_system::TypeTag::S32 => Some(type_system::PrimitiveType::S32),
        type_system::TypeTag::U32 => Some(type_system::PrimitiveType::U32),
        type_system::TypeTag::S64 => Some(type_system::PrimitiveType::S64),
        type_system::TypeTag::U64 => Some(type_system::PrimitiveType::U64),
        type_system::TypeTag::SNative => Some(type_system::PrimitiveType::SNative),
        type_system::TypeTag::UNative => Some(type_system::PrimitiveType::UNative),
        type_system::TypeTag::F32 => Some(type_system::PrimitiveType::F32),
        type_system::TypeTag::F64 => Some(type_system::PrimitiveType::F64),
        _ => None,
    }
}

// fn heap_type(tag: type_system::TypeTag) -> ParseResult<Option<type_system::HeapType>> {
//     simple_type(tag)
//         .map(|t| Ok(Some(type_system::HeapType::Val(t))))
//         .unwrap_or_else(|| {
//             todo!()
//         })
// }

fn type_signature<R: Read>(src: &mut R) -> Result<type_system::AnyType> {
    let tag: type_system::TypeTag = unsafe { std::mem::transmute(byte(src)?) }; // TODO: Define a conversion function going from u8 to TypeTag.
    primitive_type(tag)
        .map(|p| {
            Ok(type_system::AnyType::Heap(type_system::HeapType::Val(
                type_system::SimpleType::Primitive(p),
            )))
        })
        .unwrap_or_else(|| Err(Error::InvalidTypeSignatureTag(tag as u8)))
}

fn function_signature<R: Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> Result<format::FunctionSignature> {
    Ok(format::FunctionSignature {
        return_types: length_encoded_indices(src, size)?,
        parameter_types: length_encoded_indices(src, size)?,
    })
}

fn opcode<R: Read>(src: &mut R) -> Result<Opcode> {
    let mut opcode = 0u32;
    loop {
        let value = byte(src)?;
        opcode += u32::from(value);
        if value != Opcode::Continuation as u8 {
            return Opcode::try_from(opcode).map_err(|()| Error::InvalidOpcode(opcode));
        }
    }
}

fn constant_integer<R: Read>(src: &mut R) -> Result<instruction_set::IntegerConstant> {
    use instruction_set::IntegerConstant;
    use type_system::PrimitiveType;

    let tag = byte(src)?;
    let integer_type = primitive_type(unsafe { std::mem::transmute(tag) })
        .ok_or(Error::InvalidPrimitiveType(tag))?;

    match integer_type {
        PrimitiveType::U8 => Ok(IntegerConstant::U8(byte(src)?)),
        PrimitiveType::S8 => Ok(IntegerConstant::S8(byte(src)? as i8)),
        PrimitiveType::S16 => Ok(IntegerConstant::S16(i16::from_le_bytes(fixed_bytes(src)?))),
        PrimitiveType::U16 => Ok(IntegerConstant::U16(u16::from_le_bytes(fixed_bytes(src)?))),
        PrimitiveType::S32 => Ok(IntegerConstant::S32(i32::from_le_bytes(fixed_bytes(src)?))),
        PrimitiveType::U32 => Ok(IntegerConstant::U32(u32::from_le_bytes(fixed_bytes(src)?))),
        PrimitiveType::S64 => Ok(IntegerConstant::S64(i64::from_le_bytes(fixed_bytes(src)?))),
        PrimitiveType::U64 => Ok(IntegerConstant::U64(u64::from_le_bytes(fixed_bytes(src)?))),
        PrimitiveType::UNative
        | PrimitiveType::SNative
        | PrimitiveType::F32
        | PrimitiveType::F64 => Err(Error::InvalidIntegerConstantType(integer_type)),
    }
}

fn numeric_type<R: Read>(src: &mut R) -> Result<instruction_set::NumericType> {
    let tag_byte = byte(src)?;
    let tag: type_system::TypeTag = unsafe { std::mem::transmute(tag_byte) };
    primitive_type(tag)
        .ok_or(Error::InvalidNumericType(tag_byte))
        .map(instruction_set::NumericType::Primitive)
}

fn arithmetic_flags<R: Read>(src: &mut R) -> Result<instruction_set::ArithmeticFlags> {
    let bits = byte(src)?;
    let flags: instruction_set::ArithmeticFlags = unsafe { std::mem::transmute(bits) };
    if instruction_set::ArithmeticFlags::all().contains(flags) {
        Ok(flags)
    } else {
        Err(Error::InvalidArithmeticFlags(bits))
    }
}

fn basic_arithmetic_operation<R: Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> Result<instruction_set::BasicArithmeticOperation> {
    let flags = arithmetic_flags(src)?;
    Ok(instruction_set::BasicArithmeticOperation {
        overflow: instruction_set::OverflowBehavior::from(flags),
        x: unsigned_index(src, size)?,
        y: unsigned_index(src, size)?,
    })
}

fn division_operation<R: Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> Result<instruction_set::DivisionOperation> {
    let flags = arithmetic_flags(src)?;
    Ok(instruction_set::DivisionOperation {
        divide_by_zero: if flags
            .contains(instruction_set::ArithmeticFlags::RETURN_VALUE_ON_DIVIDE_BY_ZERO)
        {
            instruction_set::DivideByZeroBehavior::Return(unsigned_index(src, size)?)
        } else {
            instruction_set::DivideByZeroBehavior::Halt
        },
        overflow: instruction_set::OverflowBehavior::from(flags),
        return_type: numeric_type(src)?,
        numerator: unsigned_index(src, size)?,
        denominator: unsigned_index(src, size)?,
    })
}

fn bitwise_operation<R: Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> Result<instruction_set::BitwiseOperation> {
    Ok(instruction_set::BitwiseOperation {
        result_type: numeric_type(src)?,
        x: unsigned_index(src, size)?,
        y: unsigned_index(src, size)?,
    })
}

fn bitwise_shift_operation<R: Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> Result<instruction_set::BitwiseShiftOperation> {
    bitwise_operation(src, size).map(instruction_set::BitwiseShiftOperation)
}

fn instruction<R: Read>(src: &mut R, size: numeric::IntegerSize) -> Result<Instruction> {
    match opcode(src)? {
        Opcode::Nop => Ok(Instruction::Nop),
        Opcode::Ret => Ok(Instruction::Ret(length_encoded_indices(src, size)?)),
        Opcode::Br => Ok(Instruction::Br {
            target: unsigned_index(src, size)?,
            input_registers: length_encoded_indices(src, size)?,
        }),
        // Opcode::BrIf => Ok(Instruction::BrIf {
        //     condition: unsigned_index(src, size)?,
        //     true_branch: unsigned_index(src, size)?,
        //     false_branch: unsigned_index(src, size)?,
        //     input_registers: length_encoded_indices(src, size)?,
        // }),
        Opcode::Call => Ok(Instruction::Call(
            format::instruction_set::CallInstruction {
                function: unsigned_index(src, size)?,
                arguments: length_encoded_indices(src, size)?,
            },
        )),
        Opcode::Add => Ok(Instruction::Add(basic_arithmetic_operation(src, size)?)),
        Opcode::Sub => Ok(Instruction::Sub(basic_arithmetic_operation(src, size)?)),
        Opcode::Mul => Ok(Instruction::Mul(basic_arithmetic_operation(src, size)?)),
        // Opcode::Div => Ok(Instruction::Div(division_operation(src, size)?)),
        // Opcode::And => Ok(Instruction::And(bitwise_operation(src, size)?)),
        // Opcode::Or => Ok(Instruction::Or(bitwise_operation(src, size)?)),
        // Opcode::Not => Ok(Instruction::Not(
        //     numeric_type(src)?,
        //     unsigned_index(src, size)?,
        // )),
        // Opcode::Xor => Ok(Instruction::Xor(bitwise_operation(src, size)?)),
        // Opcode::ShL => Ok(Instruction::ShL(bitwise_shift_operation(src, size)?)),
        // Opcode::ShR => Ok(Instruction::ShR(bitwise_shift_operation(src, size)?)),
        // Opcode::RotL => Ok(Instruction::RotL(bitwise_shift_operation(src, size)?)),
        // Opcode::RotR => Ok(Instruction::RotR(bitwise_shift_operation(src, size)?)),
        Opcode::ConstI => Ok(Instruction::ConstI(constant_integer(src)?)),
        Opcode::Break => Ok(Instruction::Break),
        Opcode::Continuation => unreachable!(),
        bad => todo!(
            "TODO: Add support for parsing of more instructions such as {:?}",
            bad
        ),
    }
}

fn byte_flags<B, C: FnOnce(u8) -> Option<B>, E: FnOnce(u8) -> Error, R: Read>(
    src: &mut R,
    converter: C,
    error: E,
) -> Result<B> {
    let bits = byte(src)?;
    converter(bits).ok_or_else(|| error(bits))
}

fn code_block<R: Read>(src: &mut R, size: numeric::IntegerSize) -> Result<format::CodeBlock> {
    let flags = byte_flags(
        src,
        flags::CodeBlock::from_bits,
        Error::InvalidCodeBlockFlags,
    )?;

    let input_register_count = unsigned_integer(src, size)?;

    Ok(format::CodeBlock {
        input_register_count,
        exception_handler: if flags.is_empty() { None } else { todo!() },
        instructions: {
            let length = unsigned_length(src, size)?;
            let buffer = many_bytes(src, length)?;
            format::LenBytes(length_encoded_vector(
                &mut buffer.as_slice(),
                size,
                |src| instruction(src, size),
            )?)
        },
    })
}

fn function_body<R: Read>(src: &mut R, size: numeric::IntegerSize) -> Result<format::Code> {
    Ok(format::Code {
        entry_block: code_block(src, size)?,
        blocks: length_encoded_vector(src, size, |src| code_block(src, size))?,
    })
}

fn data_array<R: Read>(src: &mut R, size: numeric::IntegerSize) -> Result<format::DataArray> {
    let length = unsigned_length(src, size)?;
    many_bytes(src, length).map(|data| format::DataArray(format::LenVec(data)))
}

fn byte_enum<B: TryFrom<u8>, E: FnOnce(u8) -> Error, R: Read>(src: &mut R, error: E) -> Result<B> {
    byte_flags(src, |bits| B::try_from(bits).ok(), error)
}

fn namespace_definition<R: Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> Result<format::Namespace> {
    let name = unsigned_index(src, size)?;
    let flags = byte_flags(
        src,
        flags::Namespace::from_bits,
        Error::InvalidNamespaceFlags,
    )?;

    Ok(format::Namespace {
        name,
        parent: if flags.contains(flags::Namespace::HAS_PARENT) {
            Some(unsigned_index(src, size)?)
        } else {
            None
        },
        structs: length_encoded_indices(src, size)?,
        globals: length_encoded_indices(src, size)?,
        functions: length_encoded_indices(src, size)?,
    })
}

fn struct_definition<R: Read>(src: &mut R, size: numeric::IntegerSize) -> Result<format::Struct> {
    let name = unsigned_index(src, size)?;
    let flags = byte_flags(src, flags::Struct::from_bits, Error::InvalidStructFlags)?;

    Ok(format::Struct {
        name,
        is_export: flags.is_export(),
        symbol: unsigned_index(src, size)?,
        layout: unsigned_index(src, size)?,
        fields: length_encoded_indices(src, size)?,
    })
}

//global_definition

//field_definition

fn function_definition<R: Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> Result<format::Function> {
    let name = unsigned_index(src, size)?;
    let signature = unsigned_index(src, size)?;
    let flags = byte_flags(src, flags::Function::from_bits, Error::InvalidFunctionFlags)?;

    Ok(format::Function {
        name,
        signature,
        is_export: flags.is_export(),
        symbol: unsigned_index(src, size)?,
        body: if flags.contains(flags::Function::IS_EXTERNAL) {
            format::FunctionBody::External {
                library: unsigned_index(src, size)?,
                entry_point_name: unsigned_index(src, size)?,
            }
        } else {
            format::FunctionBody::Defined(unsigned_index(src, size)?)
        },
    })
}

fn struct_layout<R: Read>(src: &mut R, size: numeric::IntegerSize) -> Result<format::StructLayout> {
    Ok(
        match byte_flags(
            src,
            |bits| flags::StructLayout::try_from(bits).ok(),
            Error::InvalidStructLayoutFlags,
        )? {
            flags::StructLayout::Unspecified => format::StructLayout::Unspecified,
            _ => todo!("Parsing of specific struct layouts is not yet supported"),
        },
    )
}

fn magic_bytes<R: Read>(src: &mut R, magic: &[u8], error: Error) -> Result<()> {
    let actual = many_bytes(src, magic.len())?;
    if actual == magic {
        Ok(())
    } else {
        Err(error)
    }
}

fn integer_size<R: Read>(src: &mut R) -> Result<numeric::IntegerSize> {
    byte(src).and_then(|value| match value {
        0 => Ok(numeric::IntegerSize::I1),
        1 => Ok(numeric::IntegerSize::I2),
        2 => Ok(numeric::IntegerSize::I4),
        _ => Err(Error::InvalidIntegerSize(value)),
    })
}

fn double_length_encoded<T, F: FnMut(&mut &[u8]) -> Result<T>, R: Read>(
    src: &mut R,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
    parser: F,
) -> Result<format::LenVecBytes<T>> {
    let length = unsigned_length(src, size)?;
    Ok(format::LenBytes(if length > 0usize {
        let mut buffer = buffer_pool.rent_with_length(length);
        fill_buffer(src, &mut buffer)?;
        let mut buffer_slice: &[u8] = &mut buffer;
        length_encoded_vector(&mut buffer_slice, size, parser)?
    } else {
        format::LenVec(Vec::new())
    }))
}

fn module_data<T, D: FnOnce() -> T, F: FnOnce(&[u8]) -> Result<T>>(
    data_vectors: &[Vec<u8>],
    index: usize,
    default: D,
    parser: F,
) -> Result<format::LenBytes<T>> {
    data_vectors
        .get(index)
        .and_then(|data| {
            if data.is_empty() {
                None
            } else {
                Some(parser(data.as_slice()))
            }
        })
        .unwrap_or_else(|| Ok(default()))
        .map(format::LenBytes)
}

fn module_data_or_default<T: Default, F: FnOnce(&[u8]) -> Result<T>>(
    data_vectors: &[Vec<u8>],
    index: usize,
    parser: F,
) -> Result<format::LenBytes<T>> {
    module_data(data_vectors, index, T::default, parser)
}

/// Parses a binary module.
pub fn parse_module<R: Read>(input: &mut R) -> Result<format::Module> {
    magic_bytes(input, format::MAGIC, Error::InvalidModuleMagic)?;
    let size = integer_size(input)?;
    let format_version = format::FormatVersion {
        major: unsigned_integer(input, size)?,
        minor: unsigned_integer(input, size)?,
    };

    if !format_version.is_supported() {
        return Err(Error::UnsupportedFormatVersion(format_version));
    }

    let data_count = unsigned_integer(input, size)?;

    if data_count < format::MIN_MODULE_DATA_COUNT || data_count > format::MAX_MODULE_DATA_COUNT {
        return Err(Error::InvalidDataVectorCount(data_count));
    }

    let mut data_vectors: Vec<Vec<u8>> = Vec::with_capacity(data_count.try_into().unwrap());

    for _ in 0u32..(data_count.0) {
        let length = unsigned_length(input, size)?;
        data_vectors.push(many_bytes(input, length)?);
    }

    let buffers = buffers::BufferPool::new();

    Ok(format::Module {
        integer_size: size,
        format_version,
        // Header is always present.
        header: format::LenBytes(module_header(
            &mut data_vectors[0].as_slice(),
            size,
            &buffers,
        )?),
        identifiers: module_data(
            &data_vectors,
            1,
            || format::LenVec(Vec::new()),
            |mut data| {
                length_encoded_vector(&mut data, size, |src| identifier(src, size, &buffers))
            },
        )?,
        namespaces: module_data(
            &data_vectors,
            2,
            || format::LenVec(Vec::new()),
            |mut data| {
                length_encoded_vector(&mut data, size, |src| namespace_definition(src, size))
            },
        )?,
        type_signatures: module_data(
            &data_vectors,
            3,
            || format::LenVec(Vec::new()),
            |mut data| length_encoded_vector(&mut data, size, |src| type_signature(src)),
        )?,
        function_signatures: module_data_or_default(&data_vectors, 4, |mut data| {
            length_encoded_vector(&mut data, size, |src| function_signature(src, size))
        })?,
        function_bodies: module_data(
            &data_vectors,
            5,
            || format::LenVec(Vec::new()),
            |mut data| length_encoded_vector(&mut data, size, |src| function_body(src, size)),
        )?,
        data: module_data_or_default(&data_vectors, 6, |mut data| {
            length_encoded_vector(&mut data, size, |src| data_array(src, size))
        })?,
        imports: module_data(
            &data_vectors,
            7,
            || format::ModuleImports {
                imported_modules: format::LenBytes(format::LenVec(Vec::new())),
                imported_structs: format::LenBytes(format::LenVec(Vec::new())),
                imported_globals: format::LenBytes(format::LenVec(Vec::new())),
                imported_fields: format::LenBytes(format::LenVec(Vec::new())),
                imported_functions: format::LenBytes(format::LenVec(Vec::new())),
            },
            |mut data| {
                Ok(format::ModuleImports {
                    imported_modules: double_length_encoded(&mut data, size, &buffers, |src| {
                        module_identifier(src, size, &buffers)
                    })?,
                    imported_structs: double_length_encoded(
                        &mut data,
                        size,
                        &buffers,
                        |src| todo!(),
                    )?,
                    imported_globals: double_length_encoded(
                        &mut data,
                        size,
                        &buffers,
                        |src| todo!(),
                    )?,
                    imported_fields: double_length_encoded(
                        &mut data,
                        size,
                        &buffers,
                        |src| todo!(),
                    )?,
                    imported_functions: double_length_encoded(
                        &mut data,
                        size,
                        &buffers,
                        |src| todo!(),
                    )?,
                })
            },
        )?,
        definitions: module_data(
            &data_vectors,
            8,
            || format::ModuleDefinitions {
                defined_structs: format::LenBytes(format::LenVec(Vec::new())),
                defined_globals: format::LenBytes(format::LenVec(Vec::new())),
                defined_fields: format::LenBytes(format::LenVec(Vec::new())),
                defined_functions: format::LenBytes(format::LenVec(Vec::new())),
            },
            |mut data| {
                Ok(format::ModuleDefinitions {
                    defined_structs: double_length_encoded(&mut data, size, &buffers, |src| {
                        struct_definition(src, size)
                    })?,
                    defined_globals: double_length_encoded(
                        &mut data,
                        size,
                        &buffers,
                        |src| todo!(),
                    )?,
                    defined_fields: double_length_encoded(
                        &mut data,
                        size,
                        &buffers,
                        |src| todo!(),
                    )?,
                    defined_functions: double_length_encoded(&mut data, size, &buffers, |src| {
                        function_definition(src, size)
                    })?,
                })
            },
        )?,
        struct_layouts: module_data(
            &data_vectors,
            9,
            || format::LenVec(Vec::new()),
            |mut data| length_encoded_vector(&mut data, size, |src| struct_layout(src, size)),
        )?,
        entry_point: module_data(
            &data_vectors,
            10,
            || None,
            |mut data| unsigned_index(&mut data, size).map(Some),
        )?,
    })
}

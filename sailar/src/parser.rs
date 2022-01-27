use crate::buffers;
use crate::format::{self, flags, instruction_set, numeric, type_system};
use std::io::Read;

#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum Error {
    #[error("the file magic indicates that it is not a valid binary module")]
    InvalidModuleMagic,
    #[error("{0:#02X} is not a valid integer size value")]
    InvalidIntegerSize(u8),
    #[error("The format version {}.{} is not supported", .0.major, .0.minor)]
    UnsupportedFormatVersion(format::FormatVersion),
    #[error("{0} is not a valid data vector count")]
    InvalidDataVectorCount(numeric::UInteger),
    #[error("expected to read {expected} bytes but got {actual}")]
    InvalidByteLength { expected: usize, actual: usize },
    #[error("{0} is not a valid number of fields for the module header")]
    InvalidHeaderFieldCount(numeric::UInteger),
    #[error(transparent)]
    InvalidIdentifierCharacter(#[from] std::str::Utf8Error),
    #[error("identifiers must not be empty")]
    EmptyIdentifier,
    #[error("{0:#02X} is not a valid namespace flags combination")]
    InvalidNamespaceFlags(u8),
    #[error(transparent)]
    InvalidTypeSignature(#[from] type_system::InvalidTagError),
    #[error("invalid primitive type, {0}")]
    InvalidPrimitiveType(#[from] type_system::TryFromTagError),
    #[error(transparent)]
    InvalidFixedIntegerType(#[from] type_system::InvalidFixedIntegerTypeError),
    #[error("{0:#02X} is not a valid combination of code block flags")]
    InvalidCodeBlockFlags(u8),
    #[error(transparent)]
    InvalidOpcode(#[from] instruction_set::InvalidOpcodeError),
    #[error("duplicate switch branch for value {0:?}")]
    DuplicateSwitchBranch(instruction_set::IntegerConstant),
    #[error("{0:#02X} is not a valid arithmetic flags combination")]
    InvalidArithmeticFlags(u8),
    #[error("{0:#02X} is not a valid numeric type")]
    InvalidNumericType(u8),
    #[error("{0:#02X} is not a valid overflow behavior")]
    InvalidOverflowBehavior(u8),
    #[error("{0:#02X} is not a valid comparison kind")]
    InvalidComparisonKind(u8),
    #[error("{0:#02X} is not a valid struct flags combination")]
    InvalidStructFlags(u8),
    #[error("{0:#02X} is not a valid field flags combination")]
    InvalidFieldFlags(u8),
    #[error("{0:#02X} is not a valid function flags combination")]
    InvalidFunctionFlags(u8),
    #[error("{0:#02X} is not a valid struct layout")]
    InvalidStructLayoutFlags(u8),
    #[error(transparent)]
    InputOutputError(#[from] std::io::Error),
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

fn type_tag<R: Read>(src: &mut R) -> Result<type_system::Tag> {
    Ok(type_system::Tag::try_from(byte(src)?)?)
}

fn fixed_integer_type<R: Read>(src: &mut R) -> Result<type_system::FixedInt> {
    Ok(type_system::FixedInt::try_from(
        type_system::Int::try_from(type_tag(src)?)?,
    )?)
}

fn type_signature<R: Read>(src: &mut R, size: numeric::IntegerSize) -> Result<type_system::Any> {
    #[allow(deprecated)]
    match type_tag(src)? {
        type_system::Tag::Unit => unreachable!(),
        type_system::Tag::Struct => Ok(type_system::Any::Struct(unsigned_index(src, size)?)),
        type_system::Tag::NativePointer => todo!("parse native pointer types"),
        tag => Ok(type_system::Any::Primitive(
            type_system::Primitive::try_from(tag)?,
        )),
    }
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

fn opcode<R: Read>(src: &mut R) -> Result<instruction_set::Opcode> {
    let mut opcode = 0u32;
    loop {
        let value = byte(src)?;
        opcode += u32::from(value);
        if value != instruction_set::Opcode::Continuation as u8 {
            return Ok(instruction_set::Opcode::try_from(opcode)?);
        }
    }
}

fn constant_integer<R: Read>(src: &mut R) -> Result<instruction_set::IntegerConstant> {
    use instruction_set::IntegerConstant;
    use type_system::FixedInt;

    Ok(match fixed_integer_type(src)? {
        FixedInt::U8 => IntegerConstant::U8(byte(src)?),
        FixedInt::S8 => IntegerConstant::S8(byte(src)? as i8),
        FixedInt::S16 => IntegerConstant::S16(i16::from_le_bytes(fixed_bytes(src)?)),
        FixedInt::U16 => IntegerConstant::U16(u16::from_le_bytes(fixed_bytes(src)?)),
        FixedInt::S32 => IntegerConstant::S32(i32::from_le_bytes(fixed_bytes(src)?)),
        FixedInt::U32 => IntegerConstant::U32(u32::from_le_bytes(fixed_bytes(src)?)),
        FixedInt::S64 => IntegerConstant::S64(i64::from_le_bytes(fixed_bytes(src)?)),
        FixedInt::U64 => IntegerConstant::U64(u64::from_le_bytes(fixed_bytes(src)?)),
    })
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

fn instruction<R: Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> Result<instruction_set::Instruction> {
    use instruction_set::{Instruction, Opcode};

    #[allow(deprecated)]
    match opcode(src)? {
        Opcode::Nop => Ok(Instruction::Nop),
        Opcode::Ret => Ok(Instruction::Ret(length_encoded_indices(src, size)?)),
        Opcode::Phi => unreachable!(),
        Opcode::Switch => {
            use instruction_set::IntegerConstant;
            use type_system::FixedInt;

            let comparison = unsigned_index(src, size)?;
            let comparison_type = fixed_integer_type(src)?;

            let default_target = unsigned_index(src, size)?;

            let mut target_lookup =
                instruction_set::SwitchLookupTable::with_capacity(unsigned_length(src, size)?);

            let next_value: fn(&mut R) -> Result<instruction_set::IntegerConstant> = {
                match comparison_type {
                    FixedInt::S8 => |src| Ok(IntegerConstant::S8(byte(src)? as i8)),
                    FixedInt::U8 => |src| Ok(IntegerConstant::U8(byte(src)?)),
                    FixedInt::S16 => {
                        |src| Ok(IntegerConstant::S16(i16::from_le_bytes(fixed_bytes(src)?)))
                    }
                    FixedInt::U16 => {
                        |src| Ok(IntegerConstant::U16(u16::from_le_bytes(fixed_bytes(src)?)))
                    }
                    FixedInt::S32 => {
                        |src| Ok(IntegerConstant::S32(i32::from_le_bytes(fixed_bytes(src)?)))
                    }
                    FixedInt::U32 => {
                        |src| Ok(IntegerConstant::U32(u32::from_le_bytes(fixed_bytes(src)?)))
                    }
                    FixedInt::S64 => {
                        |src| Ok(IntegerConstant::S64(i64::from_le_bytes(fixed_bytes(src)?)))
                    }
                    FixedInt::U64 => {
                        |src| Ok(IntegerConstant::U64(u64::from_le_bytes(fixed_bytes(src)?)))
                    }
                }
            };

            for _ in 0..target_lookup.len() {
                let value = next_value(src)?;
                let target = unsigned_index(src, size)?;
                if !target_lookup.insert(value, target) {
                    return Err(Error::DuplicateSwitchBranch(value));
                }
            }

            Ok(Instruction::Switch {
                comparison,
                comparison_type,
                default_target,
                target_lookup,
            })
        }
        Opcode::Br => Ok(Instruction::Br {
            target: unsigned_index(src, size)?,
            input_registers: length_encoded_indices(src, size)?,
        }),
        Opcode::BrIf => Ok(Instruction::BrIf {
            condition: unsigned_index(src, size)?,
            true_branch: unsigned_index(src, size)?,
            false_branch: unsigned_index(src, size)?,
            input_registers: length_encoded_indices(src, size)?,
        }),
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
        Opcode::ConvI => Ok(Instruction::ConvI {
            target_type: type_system::Int::try_from(type_tag(src)?)?,
            overflow: instruction_set::OverflowBehavior::try_from(byte(src)?)
                .map_err(Error::InvalidOverflowBehavior)?,
            operand: unsigned_index(src, size)?,
        }),
        Opcode::Cmp => Ok(Instruction::Cmp {
            x: unsigned_index(src, size)?,
            kind: instruction_set::ComparisonKind::try_from(byte(src)?)
                .map_err(Error::InvalidComparisonKind)?,
            y: unsigned_index(src, size)?,
        }),
        Opcode::Field => Ok(Instruction::Field {
            field: unsigned_index(src, size)?,
            object: unsigned_index(src, size)?,
        }),
        Opcode::Alloca => Ok(Instruction::Alloca {
            amount: unsigned_index(src, size)?,
            element_type: unsigned_index(src, size)?,
        }),
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

fn struct_import<R: Read>(src: &mut R, size: numeric::IntegerSize) -> Result<format::StructImport> {
    Ok(format::StructImport {
        module: unsigned_index(src, size)?,
        symbol: unsigned_index(src, size)?,
    })
}

//global_import

fn field_import<R: Read>(src: &mut R, size: numeric::IntegerSize) -> Result<format::FieldImport> {
    Ok(format::FieldImport {
        owner: unsigned_index(src, size)?,
        symbol: unsigned_index(src, size)?,
        signature: unsigned_index(src, size)?,
    })
}

fn function_import<R: Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> Result<format::FunctionImport> {
    Ok(format::FunctionImport {
        module: unsigned_index(src, size)?,
        symbol: unsigned_index(src, size)?,
        signature: unsigned_index(src, size)?,
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

fn field_definition<R: Read>(src: &mut R, size: numeric::IntegerSize) -> Result<format::Field> {
    let owner = unsigned_index(src, size)?;
    let name = unsigned_index(src, size)?;
    let flags = byte_flags(src, flags::Field::from_bits, Error::InvalidFieldFlags)?;

    Ok(format::Field {
        owner,
        name,
        is_export: flags.is_export(),
        symbol: unsigned_index(src, size)?,
        signature: unsigned_index(src, size)?,
    })
}

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
            |mut data| length_encoded_vector(&mut data, size, |src| type_signature(src, size)),
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
                    imported_structs: double_length_encoded(&mut data, size, &buffers, |src| {
                        struct_import(src, size)
                    })?,
                    imported_globals: double_length_encoded(
                        &mut data,
                        size,
                        &buffers,
                        |src| todo!(),
                    )?,
                    imported_fields: double_length_encoded(&mut data, size, &buffers, |src| {
                        field_import(src, size)
                    })?,
                    imported_functions: double_length_encoded(&mut data, size, &buffers, |src| {
                        function_import(src, size)
                    })?,
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
                    defined_fields: double_length_encoded(&mut data, size, &buffers, |src| {
                        field_definition(src, size)
                    })?,
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

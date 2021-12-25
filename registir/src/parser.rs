use crate::{
    buffers, format,
    format::{
        instruction_set,
        instruction_set::{Instruction, Opcode},
        numeric, structures, type_system,
    },
};

#[derive(Debug)]
#[non_exhaustive]
pub enum ParseError {
    InvalidModuleMagic,
    InvalidIntegerSize(u8),
    InvalidDataVectorCount(numeric::UInteger),
    InvalidHeaderFieldCount(numeric::UInteger),
    InvalidIdentifierCharacter(std::str::Utf8Error),
    EmptyIdentifier,
    InvalidTypeSignatureTag(u8),
    InvalidCodeBlockFlags(u8),
    InvalidOpcode(u32),
    InvalidVisibilityFlags(u8),
    InvalidTypeDefinitionFlags(u8),
    InvalidMethodFlags(u8),
    InvalidMethodImplementationFlags(u8),
    InvalidTypeLayoutFlags(u8),
    InputOutputError(std::io::Error),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidModuleMagic => {
                f.write_str("The file magic indicates that it is not a valid binary module")
            }
            Self::InvalidIntegerSize(value) => {
                write!(f, "{:#02X} is not a valid integer size value", value)
            }
            Self::InvalidDataVectorCount(count) => {
                write!(f, "{} is not a valid data vector count", count)
            }
            Self::InvalidHeaderFieldCount(count) => write!(
                f,
                "{} is not a valid number of fields for the module header",
                count
            ),
            Self::EmptyIdentifier => f.write_str("Identifiers must not be empty"),
            Self::InvalidIdentifierCharacter(error) => error.fmt(f),
            Self::InvalidTypeSignatureTag(tag) => {
                write!(f, "{:#02X} is not a valid type signature tag", tag)
            }
            Self::InvalidCodeBlockFlags(flags) => write!(
                f,
                "{:#02X} is not a valid combination of code block flags",
                flags
            ),
            Self::InvalidOpcode(opcode) => write!(f, "{} is not a valid opcode", opcode),
            Self::InvalidVisibilityFlags(flag) => {
                write!(f, "{:#02X} is not a valid visibility value", flag)
            }
            Self::InvalidTypeDefinitionFlags(flag) => write!(
                f,
                "{:#02X} is not a valid type definition flags combination",
                flag
            ),
            Self::InvalidMethodFlags(flag) => {
                write!(f, "{:#02X} is not a valid method flags combination", flag)
            }
            Self::InvalidMethodImplementationFlags(flag) => write!(
                f,
                "{:#02X} is not a valid method implementation flags combination",
                flag
            ),
            Self::InvalidTypeLayoutFlags(flag) => {
                write!(f, "{:#02X} is not a valid type layout", flag)
            }
            Self::InputOutputError(error) => error.fmt(f),
        }
    }
}

impl std::error::Error for ParseError {}

pub type ParseResult<T> = Result<T, ParseError>;

fn fill_buffer(src: &mut impl std::io::Read, buffer: &mut [u8]) -> ParseResult<()> {
    src.read_exact(buffer).map_err(ParseError::InputOutputError)
}

fn fixed_bytes<R: std::io::Read, const L: usize>(src: &mut R) -> ParseResult<[u8; L]> {
    let mut buffer = [0u8; L];
    fill_buffer(src, &mut buffer).map(|()| buffer)
}

fn byte<R: std::io::Read>(src: &mut R) -> ParseResult<u8> {
    fixed_bytes::<R, 1>(src).map(|bytes| bytes[0])
}

fn many_bytes<R: std::io::Read>(src: &mut R, length: usize) -> ParseResult<Vec<u8>> {
    let mut buffer = vec![0u8; length];
    fill_buffer(src, buffer.as_mut_slice()).map(|()| buffer)
}

fn unsigned_integer<R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> ParseResult<numeric::UInteger> {
    match size {
        numeric::IntegerSize::I1 => byte(src).map(numeric::UInteger::from),
        numeric::IntegerSize::I2 => fixed_bytes::<R, 2>(src)
            .map(|buffer| numeric::UInteger::from(u16::from_le_bytes(buffer))),
        numeric::IntegerSize::I4 => {
            fixed_bytes::<R, 4>(src).map(|buffer| numeric::UInteger(u32::from_le_bytes(buffer)))
        }
    }
}

fn unsigned_length<R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> ParseResult<usize> {
    unsigned_integer(src, size).map(|value| usize::try_from(value).unwrap())
}

fn unsigned_index<I: From<numeric::UInteger>, R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> ParseResult<I> {
    unsigned_integer(src, size).map(I::from)
}

fn identifier<R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
) -> ParseResult<format::Identifier> {
    let length = unsigned_length(src, size)?;
    let mut buffer = buffer_pool.rent_with_length(length);
    fill_buffer(src, &mut buffer)?;
    std::str::from_utf8(&buffer)
        .map_err(ParseError::InvalidIdentifierCharacter)
        .and_then(|s| format::Identifier::try_from(s).map_err(|_| ParseError::EmptyIdentifier))
}

fn length_encoded_vector<T, P: FnMut(&mut R) -> ParseResult<T>, R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
    mut parser: P,
) -> ParseResult<structures::LengthEncodedVector<T>> {
    let length = unsigned_length(src, size)?;
    let mut buffer = Vec::<T>::with_capacity(length);

    for _ in 0..length {
        let item = parser(src)?;
        buffer.push(item);
    }

    Ok(structures::LengthEncodedVector(buffer))
}

fn length_encoded_indices<I: From<numeric::UInteger>, R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> ParseResult<structures::LengthEncodedVector<I>> {
    length_encoded_vector(src, size, |src| unsigned_integer(src, size).map(I::from))
}

fn version_numbers<R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> ParseResult<format::VersionNumbers> {
    length_encoded_vector(src, size, |src| unsigned_integer(src, size)).map(format::VersionNumbers)
}

fn module_identifier<R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
) -> ParseResult<format::ModuleIdentifier> {
    Ok(format::ModuleIdentifier {
        name: identifier(src, size, buffer_pool)?,
        version: version_numbers(src, size)?,
    })
}

fn module_header<R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
) -> ParseResult<format::ModuleHeader> {
    let field_count = unsigned_integer(src, size)?;

    if field_count < format::MIN_HEADER_FIELD_COUNT || field_count > format::MAX_HEADER_FIELD_COUNT
    {
        return Err(ParseError::InvalidHeaderFieldCount(field_count));
    }

    let id = module_identifier(src, size, buffer_pool)?;

    Ok(format::ModuleHeader { identifier: id })
}

fn primitive_type(tag: type_system::TypeTag) -> Option<type_system::PrimitiveType> {
    match tag {
        type_system::TypeTag::S16 => Some(type_system::PrimitiveType::S16),
        type_system::TypeTag::U16 => Some(type_system::PrimitiveType::U16),
        type_system::TypeTag::S32 => Some(type_system::PrimitiveType::S32),
        type_system::TypeTag::U32 => Some(type_system::PrimitiveType::U32),
        type_system::TypeTag::S64 => Some(type_system::PrimitiveType::S64),
        type_system::TypeTag::U64 => Some(type_system::PrimitiveType::U64),
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

fn type_signature<R: std::io::Read>(src: &mut R) -> ParseResult<type_system::AnyType> {
    let tag: type_system::TypeTag = unsafe { std::mem::transmute(byte(src)?) }; // TODO: Define a conversion function going from u8 to TypeTag.
    primitive_type(tag)
        .map(|p| {
            Ok(type_system::AnyType::Heap(type_system::HeapType::Val(
                type_system::SimpleType::Primitive(p),
            )))
        })
        .unwrap_or_else(|| Err(ParseError::InvalidTypeSignatureTag(tag as u8)))
}

fn method_signature<R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> ParseResult<format::MethodSignature> {
    Ok(format::MethodSignature {
        return_types: length_encoded_indices(src, size)?,
        parameter_types: length_encoded_indices(src, size)?,
    })
}

fn opcode<R: std::io::Read>(src: &mut R) -> ParseResult<Opcode> {
    let mut opcode = 0u32;
    loop {
        let value = byte(src)?;
        opcode += u32::from(value);
        if value != Opcode::Continuation as u8 {
            break Opcode::try_from(opcode).map_err(|()| ParseError::InvalidOpcode(opcode));
        }
    }
}

fn instruction<R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> ParseResult<Instruction> {
    match opcode(src)? {
        Opcode::Nop => Ok(Instruction::Nop),
        Opcode::Ret => Ok(Instruction::Ret(length_encoded_indices(src, size)?)),
        Opcode::Continuation => unreachable!(),
    }
}

fn byte_flags<B, C: FnOnce(u8) -> Option<B>, E: FnOnce(u8) -> ParseError, R: std::io::Read>(
    src: &mut R,
    converter: C,
    error: E,
) -> ParseResult<B> {
    let bits = byte(src)?;
    converter(bits).ok_or_else(|| error(bits))
}

fn code_block<R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> ParseResult<format::CodeBlock> {
    let flags = byte_flags(
        src,
        format::CodeBlockFlags::from_bits,
        ParseError::InvalidCodeBlockFlags,
    )?;

    let input_register_count = unsigned_integer(src, size)?;

    Ok(format::CodeBlock {
        input_register_count,
        exception_handler: if flags.is_empty() { None } else { todo!() },
        instructions: {
            let length = unsigned_length(src, size)?;
            let buffer = many_bytes(src, length)?;
            structures::ByteLengthEncoded(length_encoded_vector(
                &mut buffer.as_slice(),
                size,
                |src| instruction(src, size),
            )?)
        },
    })
}

fn method_body<R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> ParseResult<format::Code> {
    Ok(format::Code {
        entry_block: code_block(src, size)?,
        blocks: length_encoded_vector(src, size, |src| code_block(src, size))?,
    })
}

fn data_array<R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> ParseResult<format::DataArray> {
    let length = unsigned_length(src, size)?;
    many_bytes(src, length).map(|data| format::DataArray(structures::LengthEncodedVector(data)))
}

fn byte_enum<B: TryFrom<u8>, E: FnOnce(u8) -> ParseError, R: std::io::Read>(
    src: &mut R,
    error: E,
) -> ParseResult<B> {
    byte_flags(src, |bits| B::try_from(bits).ok(), error)
}

fn type_definition<R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> ParseResult<format::Type> {
    Ok(format::Type {
        name: unsigned_index(src, size)?,
        namespace: unsigned_index(src, size)?,
        visibility: byte_enum(src, ParseError::InvalidVisibilityFlags)?,
        flags: byte_flags(
            src,
            format::TypeFlags::from_bits,
            ParseError::InvalidTypeDefinitionFlags,
        )?,
        layout: unsigned_index(src, size)?,
        inherited_types: length_encoded_indices(src, size)?,
        fields: length_encoded_indices(src, size)?,
        methods: length_encoded_indices(src, size)?,
        vtable: length_encoded_vector(src, size, |src| {
            Ok(format::MethodOverride {
                declaration: unsigned_index(src, size)?,
                implementation: unsigned_index(src, size)?,
            })
        })?,
    })
}

//field_definition

fn method_definition<R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> ParseResult<format::Method> {
    let owner = unsigned_index(src, size)?;
    let name = unsigned_index(src, size)?;
    let visibility = byte_enum(src, ParseError::InvalidVisibilityFlags)?;
    let flags = byte_flags(
        src,
        format::MethodFlags::from_bits,
        ParseError::InvalidMethodFlags,
    )?;
    let implementation_flags = byte_flags(
        src,
        format::MethodImplementationFlags::from_bits,
        ParseError::InvalidMethodImplementationFlags,
    )?;

    Ok(format::Method {
        owner,
        name,
        visibility,
        flags,
        signature: unsigned_index(src, size)?,
        // TODO: How to deal with EXTERNAL and NONE/ABSTRACT flags being set?
        body: if implementation_flags.contains(format::MethodImplementationFlags::NONE) {
            format::MethodBody::Abstract
        } else if implementation_flags.contains(format::MethodImplementationFlags::EXTERNAL) {
            format::MethodBody::External {
                library: unsigned_index(src, size)?,
                entry_point_name: unsigned_index(src, size)?,
            }
        } else {
            format::MethodBody::Defined(unsigned_index(src, size)?)
        },
    })
}

fn type_layout<R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> ParseResult<format::TypeLayout> {
    Ok(
        match byte_flags(
            src,
            |bits| format::TypeLayoutFlags::try_from(bits).ok(),
            ParseError::InvalidTypeLayoutFlags,
        )? {
            format::TypeLayoutFlags::Unspecified => format::TypeLayout::Unspecified,
            _ => todo!("Parsing of specific type layouts is not yet supported"),
        },
    )
}

fn magic_bytes<R: std::io::Read>(src: &mut R, magic: &[u8], error: ParseError) -> ParseResult<()> {
    let actual = many_bytes(src, magic.len())?;
    if actual == magic {
        Ok(())
    } else {
        Err(error)
    }
}

fn integer_size<R: std::io::Read>(src: &mut R) -> ParseResult<numeric::IntegerSize> {
    byte(src).and_then(|value| match value {
        0 => Ok(numeric::IntegerSize::I1),
        1 => Ok(numeric::IntegerSize::I2),
        2 => Ok(numeric::IntegerSize::I4),
        _ => Err(ParseError::InvalidIntegerSize(value)),
    })
}

fn double_length_encoded<T, F: FnMut(&mut &[u8]) -> ParseResult<T>, R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
    buffer_pool: &buffers::BufferPool,
    parser: F,
) -> ParseResult<structures::DoubleLengthEncodedVector<T>> {
    let length = unsigned_length(src, size)?;
    Ok(structures::ByteLengthEncoded(if length > 0usize {
        let mut buffer = buffer_pool.rent_with_length(length);
        fill_buffer(src, &mut buffer)?;
        let mut buffer_slice: &[u8] = &mut buffer;
        length_encoded_vector(&mut buffer_slice, size, parser)?
    } else {
        structures::LengthEncodedVector(Vec::new())
    }))
}

fn module_data<T, D: FnOnce() -> T, F: FnOnce(&[u8]) -> ParseResult<T>>(
    data_vectors: &[Vec<u8>],
    index: usize,
    default: D,
    parser: F,
) -> ParseResult<structures::ByteLengthEncoded<T>> {
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
        .map(structures::ByteLengthEncoded)
}

fn module_data_or_default<T: Default, F: FnOnce(&[u8]) -> ParseResult<T>>(
    data_vectors: &[Vec<u8>],
    index: usize,
    parser: F,
) -> ParseResult<structures::ByteLengthEncoded<T>> {
    module_data(data_vectors, index, T::default, parser)
}

/// Parses a binary module.
pub fn parse_module<R: std::io::Read>(input: &mut R) -> ParseResult<format::Module> {
    magic_bytes(input, format::MAGIC, ParseError::InvalidModuleMagic)?;
    let size = integer_size(input)?;
    let format_version = format::FormatVersion {
        major: unsigned_integer(input, size)?,
        minor: unsigned_integer(input, size)?,
    };

    let data_count = unsigned_integer(input, size)?;

    if data_count < format::MIN_MODULE_DATA_COUNT || data_count > format::MAX_MODULE_DATA_COUNT {
        return Err(ParseError::InvalidDataVectorCount(data_count));
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
        header: structures::ByteLengthEncoded(module_header(
            &mut data_vectors[0].as_slice(),
            size,
            &buffers,
        )?),
        identifiers: module_data(
            &data_vectors,
            1,
            || structures::LengthEncodedVector(Vec::new()),
            |mut data| {
                length_encoded_vector(&mut data, size, |src| identifier(src, size, &buffers))
            },
        )?,
        namespaces: module_data_or_default(&data_vectors, 2, |mut data| {
            length_encoded_vector(&mut data, size, |src| length_encoded_indices(src, size))
        })?,
        type_signatures: module_data(
            &data_vectors,
            3,
            || structures::LengthEncodedVector(Vec::new()),
            |mut data| length_encoded_vector(&mut data, size, |src| type_signature(src)),
        )?,
        method_signatures: module_data_or_default(&data_vectors, 4, |mut data| {
            length_encoded_vector(&mut data, size, |src| method_signature(src, size))
        })?,
        method_bodies: module_data(
            &data_vectors,
            5,
            || structures::LengthEncodedVector(Vec::new()),
            |mut data| length_encoded_vector(&mut data, size, |src| method_body(src, size)),
        )?,
        data_arrays: module_data_or_default(&data_vectors, 6, |mut data| {
            length_encoded_vector(&mut data, size, |src| data_array(src, size))
        })?,
        imports: module_data(
            &data_vectors,
            7,
            || format::ModuleImports {
                imported_modules: structures::ByteLengthEncoded(structures::LengthEncodedVector(
                    Vec::new(),
                )),
                imported_types: structures::ByteLengthEncoded(structures::LengthEncodedVector(
                    Vec::new(),
                )),
                imported_fields: structures::ByteLengthEncoded(structures::LengthEncodedVector(
                    Vec::new(),
                )),
                imported_methods: structures::ByteLengthEncoded(structures::LengthEncodedVector(
                    Vec::new(),
                )),
            },
            |mut data| {
                Ok(format::ModuleImports {
                    imported_modules: double_length_encoded(&mut data, size, &buffers, |src| {
                        module_identifier(src, size, &buffers)
                    })?,
                    imported_types: double_length_encoded(
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
                    imported_methods: double_length_encoded(
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
                defined_types: structures::ByteLengthEncoded(structures::LengthEncodedVector(
                    Vec::new(),
                )),
                defined_fields: structures::ByteLengthEncoded(structures::LengthEncodedVector(
                    Vec::new(),
                )),
                defined_methods: structures::ByteLengthEncoded(structures::LengthEncodedVector(
                    Vec::new(),
                )),
            },
            |mut data| {
                Ok(format::ModuleDefinitions {
                    defined_types: double_length_encoded(&mut data, size, &buffers, |src| {
                        type_definition(src, size)
                    })?,
                    defined_fields: double_length_encoded(
                        &mut data,
                        size,
                        &buffers,
                        |src| todo!(),
                    )?,
                    defined_methods: double_length_encoded(&mut data, size, &buffers, |src| {
                        method_definition(src, size)
                    })?,
                })
            },
        )?,
        entry_point: module_data(
            &data_vectors,
            9,
            || None,
            |mut data| unsigned_index(&mut data, size).map(Some),
        )?,
        type_layouts: module_data(
            &data_vectors,
            10,
            || structures::LengthEncodedVector(Vec::new()),
            |mut data| length_encoded_vector(&mut data, size, |src| type_layout(src, size)),
        )?,
    })
}

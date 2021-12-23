use crate::{
    format,
    format::{numeric, structures, type_system},
};

#[derive(Debug)]
#[non_exhaustive]
pub enum ParseError {
    InvalidModuleMagic,
    InvalidIntegerSize(u8),
    InvalidDataVectorCount(numeric::UInteger),
    InvalidHeaderFieldCount(numeric::UInteger),
    InvalidIdentifierCharacter(std::string::FromUtf8Error),
    EmptyIdentifier,
    InvalidTypeSignatureTag(u8),
    InputOutputError(std::io::Error),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidModuleMagic => {
                f.write_str("The file magic indicates that it is not a valid binary module")
            }
            Self::InputOutputError(error) => error.fmt(f),
        }
    }
}

impl std::error::Error for ParseError {}

pub type ParseResult<T> = Result<T, ParseError>;

fn fixed_bytes<R: std::io::Read, const L: usize>(src: &mut R) -> ParseResult<[u8; L]> {
    let mut buffer = [0u8; L];
    src.read_exact(&mut buffer)
        .map(|()| buffer)
        .map_err(ParseError::InputOutputError)
}

fn byte<R: std::io::Read>(src: &mut R) -> ParseResult<u8> {
    fixed_bytes::<R, 1>(src).map(|bytes| bytes[0])
}

fn many_bytes<R: std::io::Read>(src: &mut R, length: usize) -> ParseResult<Vec<u8>> {
    let mut buffer = vec![0u8; length];
    src.read_exact(buffer.as_mut_slice())
        .map(|()| buffer)
        .map_err(ParseError::InputOutputError)
}

fn uinteger<R: std::io::Read>(
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

fn ulength<R: std::io::Read>(src: &mut R, size: numeric::IntegerSize) -> ParseResult<usize> {
    uinteger(src, size).map(|value| usize::try_from(value).unwrap())
}

fn identifier<R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> ParseResult<format::Identifier> {
    let buffer = many_bytes(src, ulength(src, size)?)?;
    String::from_utf8(buffer)
        .map_err(ParseError::InvalidIdentifierCharacter)
        .and_then(|s| {
            format::Identifier::try_from(s.as_str()).map_err(|_| ParseError::EmptyIdentifier)
        })
}

fn length_encoded_vector<T, P: FnMut(&mut R) -> ParseResult<T>, R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
    parser: P,
) -> ParseResult<structures::LengthEncodedVector<T>> {
    let length = ulength(src, size)?;
    let buffer = Vec::<T>::with_capacity(length);

    for _ in 0..length {
        buffer.push(parser(src)?);
    }

    Ok(structures::LengthEncodedVector(buffer))
}

fn length_encoded_indices<I: From<numeric::UInteger>, R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> ParseResult<structures::LengthEncodedVector<I>> {
    length_encoded_vector(src, size, |src| uinteger(src, size).map(I::from))
}

fn version_numbers<R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> ParseResult<format::VersionNumbers> {
    length_encoded_vector(src, size, |src| uinteger(src, size)).map(format::VersionNumbers)
}

fn module_identifier<R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> ParseResult<format::ModuleIdentifier> {
    Ok(format::ModuleIdentifier {
        name: identifier(src, size)?,
        version: version_numbers(src, size)?,
    })
}

fn module_header<R: std::io::Read>(
    src: &mut R,
    size: numeric::IntegerSize,
) -> ParseResult<format::ModuleHeader> {
    let field_count = uinteger(src, size)?;

    if field_count < format::MIN_HEADER_FIELD_COUNT || field_count > format::MAX_HEADER_FIELD_COUNT
    {
        Err(ParseError::InvalidHeaderFieldCount(field_count))?;
    }

    let id = module_identifier(src, size)?;

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

fn module_data<T, D: FnOnce() -> T, F: FnOnce(&[u8]) -> ParseResult<T>>(
    data_vectors: &Vec<Vec<u8>>,
    index: usize,
    default: D,
    parser: F,
) -> ParseResult<structures::ByteLengthEncoded<T>> {
    data_vectors
        .get(index)
        .map(|data| parser(&data.as_slice()))
        .unwrap_or_else(|| Ok(default()))
        .map(structures::ByteLengthEncoded)
}

fn module_data_or_default<T: Default, F: FnOnce(&[u8]) -> ParseResult<T>>(
    data_vectors: &Vec<Vec<u8>>,
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
        major: uinteger(input, size)?,
        minor: uinteger(input, size)?,
    };

    let data_count = uinteger(input, size)?;

    if data_count < format::MIN_MODULE_DATA_COUNT || data_count > format::MAX_MODULE_DATA_COUNT {
        Err(ParseError::InvalidDataVectorCount(data_count))?;
    }

    let mut data_vectors: Vec<Vec<u8>> = Vec::with_capacity(data_count.try_into().unwrap());

    for _ in 0u32..(data_count.0) {
        let length = ulength(input, size)?;
        let buffer = many_bytes(input, length)?;
        data_vectors.push(buffer);
    }

    Ok(format::Module {
        integer_size: size,
        format_version,
        // Header is always present.
        header: structures::ByteLengthEncoded(module_header(
            &mut data_vectors[0].as_slice(),
            size,
        )?),
        identifiers: module_data_or_default(&data_vectors, 1, |mut data| {
            length_encoded_vector(&mut data, size, |src| identifier(src, size))
        })?,
        namespaces: module_data_or_default(&data_vectors, 2, |mut data| {
            length_encoded_vector(&mut data, size, |src| length_encoded_indices(src, size))
        })?,
        type_signatures: module_data(
            &data_vectors,
            3,
            || structures::LengthEncodedVector(Vec::new()),
            |mut data| length_encoded_vector(&mut data, size, |src| type_signature(src)),
        )?,
    })
}

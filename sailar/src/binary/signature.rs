//! Contains types that model the structure of signatures in the SAILAR binary format.

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[repr(u8)]
#[non_exhaustive]
pub enum TypeCode {
    U8 = 1,
    U16 = 2,
    U32 = 4,
    U64 = 8,
    S8 = 0x11,
    S16 = 0x12,
    S32 = 0x14,
    S64 = 0x18,
    F32 = 0xF4,
    F64 = 0xF8,
}

impl From<TypeCode> for u8 {
    #[inline]
    fn from(code: TypeCode) -> Self {
        code as u8
    }
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("{value:#02X} is not a valid type code")]
pub struct InvalidTypeCode {
    value: u8,
}

impl TryFrom<u8> for TypeCode {
    type Error = InvalidTypeCode;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Self::U8),
            2 => Ok(Self::U16),
            4 => Ok(Self::U32),
            8 => Ok(Self::U64),
            0x11 => Ok(Self::S8),
            0x12 => Ok(Self::S16),
            0x14 => Ok(Self::S32),
            0x18 => Ok(Self::S64),
            0xF4 => Ok(Self::F32),
            0xF8 => Ok(Self::F64),
            _ => Err(InvalidTypeCode { value }),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Function {
    return_types: Box<[usize]>,
    parameter_types: Box<[usize]>,
}

/// Represents a type signature
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type<'a> {
    /// Unsigned 8-bit integer.
    U8,
    /// Signed 8-bit integer.
    S8,
    /// Unsigned 16-bit integer.
    U16,
    /// Signed 16-bit integer.
    S16,
    /// Unsigned 32-bit integer.
    U32,
    /// Signed 32-bit integer.
    S32,
    /// Unsigned 64-bit integer.
    U64,
    /// Signed 64-bit integer.
    S64,
    /// Unsigned integer with the same size as a raw pointer.
    UPtr,
    /// Unsigned integer with the same size as a raw pointer.
    SPtr,
    RawPtr(Option<&'a Type<'a>>),
    /// Represents a pointer to a function.
    FuncPtr()
}

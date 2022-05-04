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
pub enum Type {}

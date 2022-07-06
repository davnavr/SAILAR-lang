//! Contains types that model the structure of signatures in the SAILAR binary format.

use crate::helper::borrow::CowBox;
use crate::index;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[repr(u8)]
#[non_exhaustive]
pub enum TypeCode {
    U8 = 1,
    U16 = 2,
    U32 = 4,
    U64 = 8,
    UAddr = 0xA,
    S8 = 0x11,
    S16 = 0x12,
    S32 = 0x14,
    S64 = 0x18,
    SAddr = 0x1A,
    RawPtr = 0xCA,
    VoidPtr = 0xCC,
    FuncPtr = 0xCF,
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
            0xA => Ok(Self::UAddr),
            0x11 => Ok(Self::S8),
            0x12 => Ok(Self::S16),
            0x14 => Ok(Self::S32),
            0x1A => Ok(Self::SAddr),
            0xCA => Ok(Self::RawPtr),
            0xCC => Ok(Self::VoidPtr),
            0xCF => Ok(Self::FuncPtr),
            0x18 => Ok(Self::S64),
            0xF4 => Ok(Self::F32),
            0xF8 => Ok(Self::F64),
            _ => Err(InvalidTypeCode { value }),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Function {
    types: Box<[index::TypeSignature]>,
    return_type_count: usize,
}

impl Function {
    /// Creates a function signature from a boxed slice of type signature indices, and a specified number of return types.
    ///
    /// The return types come first, followed by an inferred number of parameter types.
    ///
    /// # Panics
    ///
    /// Panics if the number of return types exceeds the total number of types.
    pub fn from_boxed_slice(types: Box<[index::TypeSignature]>, return_type_count: usize) -> Self {
        assert!(return_type_count <= types.len());

        Self {
            types,
            return_type_count,
        }
    }

    pub fn new<'a, P, R>(parameter_types: P, return_types: R) -> Self
    where
        P: Into<CowBox<'a, [index::TypeSignature]>>,
        R: Into<CowBox<'a, [index::TypeSignature]>>,
    {
        let parameter_types: CowBox<'a, _> = Into::into(parameter_types);
        let return_types: CowBox<'a, [index::TypeSignature]> = Into::into(return_types);
        let return_type_count = return_types.len();
        let types = 
        if parameter_types.is_empty() {
            return_types.into_boxed()
        } else if return_types.is_empty() {
            parameter_types.into_boxed()
        } else {
            return_types.iter().copied().chain(parameter_types.iter().copied()).collect()
        };

        Self::from_boxed_slice(types, return_type_count)
    }

    #[inline]
    pub(crate) fn types(&self) -> &[index::TypeSignature] {
        &self.types
    }

    pub(crate) fn return_type_len(&self) -> usize {
        self.return_type_count
    }

    pub fn return_types(&self) -> &[index::TypeSignature] {
        &self.types[0..self.return_type_count]
    }

    pub fn parameter_types(&self) -> &[index::TypeSignature] {
        &self.types[self.return_type_count..]
    }
}

/// Represents a type signature
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
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
    /// Unsigned integer with the same size as a raw pointer's address.
    UAddr,
    /// Signed integer with the same size as a raw pointer's address.
    SAddr,
    /// Single-precision floating point number.
    F32,
    /// Double-precision floating point number.
    F64,
    RawPtr(Option<index::TypeSignature>),
    /// Represents a pointer to a function.
    FuncPtr(index::FunctionSignature),
}

impl Type {
    pub fn code(&self) -> TypeCode {
        match self {
            Self::U8 => TypeCode::U8,
            Self::S8 => TypeCode::S8,
            Self::U16 => TypeCode::U16,
            Self::S16 => TypeCode::S16,
            Self::U32 => TypeCode::U32,
            Self::S32 => TypeCode::S32,
            Self::U64 => TypeCode::U64,
            Self::S64 => TypeCode::S64,
            Self::UAddr => TypeCode::UAddr,
            Self::SAddr => TypeCode::SAddr,
            Self::F32 => TypeCode::F32,
            Self::F64 => TypeCode::F64,
            Self::RawPtr(Some(_)) => TypeCode::RawPtr,
            Self::RawPtr(None) => TypeCode::VoidPtr,
            Self::FuncPtr(_) => TypeCode::FuncPtr,
        }
    }
}

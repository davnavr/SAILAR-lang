//! Contains types representing the SAILAR type system.

use crate::binary::signature::TypeCode;

/// Integer types with a fixed size.
///
/// These are the only valid types for constant integers since the size of pointer-sized integer types may change depending on
/// the target platform.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[repr(u8)]
pub enum FixedInt {
    U8,
    U16,
    U32,
    U64,
    S8,
    S16,
    S32,
    S64,
}

impl FixedInt {
    /// Gets a value indicating if the integer type is signed.
    pub const fn is_signed(self) -> bool {
        match self {
            Self::U8 | Self::U16 | Self::U32 | Self::U64 => false,
            Self::S8 | Self::S16 | Self::S32 | Self::S64 => true,
        }
    }

    pub const fn type_code(self) -> TypeCode {
        match self {
            Self::U8 => TypeCode::U8,
            Self::U16 => TypeCode::U16,
            Self::U32 => TypeCode::U32,
            Self::U64 => TypeCode::U64,
            Self::S8 => TypeCode::S8,
            Self::S16 => TypeCode::S16,
            Self::S32 => TypeCode::S32,
            Self::S64 => TypeCode::S64,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Int {
    Fixed(FixedInt),
}

impl Int {
    pub const fn type_code(self) -> TypeCode {
        match self {
            Self::Fixed(fixed) => fixed.type_code(),
        }
    }
}

crate::enum_case_from_impl!(Int, Fixed, FixedInt);

/// Floating-point numeric types.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[repr(u8)]
#[non_exhaustive]
pub enum Real {
    /// A 32-bit floating-point type, sometimes known as `single`.
    F32,
    /// A 64-bit floating-point type, sometimes known as `double`.
    F64,
}

impl Real {
    pub const fn type_code(self) -> TypeCode {
        match self {
            Self::F32 => TypeCode::F32,
            Self::F64 => TypeCode::F64,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Primitive {
    Int(Int),
    Real(Real),
}

impl Primitive {
    pub const fn type_code(self) -> TypeCode {
        match self {
            Self::Int(int) => int.type_code(),
            Self::Real(real) => real.type_code(),
        }
    }
}

crate::enum_case_from_impl!(Primitive, Int, Int);
crate::enum_case_from_impl!(Primitive, Real, Real);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Any {
    Primitive(Primitive),
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[repr(u8)]
#[non_exhaustive]
pub enum Tag {
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

impl Any {
    pub fn tag(&self) -> Tag {
        match self {
            Self::Primitive(primitive) => match primitive {
                Primitive::Int(Int::Fixed(FixedInt::U8)) => Tag::U8,
                Primitive::Int(Int::Fixed(FixedInt::S8)) => Tag::U8,
                Primitive::Int(Int::Fixed(FixedInt::U16)) => Tag::U16,
                Primitive::Int(Int::Fixed(FixedInt::S16)) => Tag::U16,
                Primitive::Int(Int::Fixed(FixedInt::U32)) => Tag::U32,
                Primitive::Int(Int::Fixed(FixedInt::S32)) => Tag::U32,
                Primitive::Int(Int::Fixed(FixedInt::U64)) => Tag::U64,
                Primitive::Int(Int::Fixed(FixedInt::S64)) => Tag::U64,
                Primitive::Real(Real::F32) => Tag::F32,
                Primitive::Real(Real::F64) => Tag::F64,
            },
        }
    }
}

crate::enum_case_from_impl!(Any, Primitive, Primitive);

impl From<FixedInt> for Any {
    fn from(fixed_integer_type: FixedInt) -> Self {
        Self::Primitive(Primitive::Int(Int::Fixed(fixed_integer_type)))
    }
}

impl From<Real> for Any {
    fn from(real: Real) -> Self {
        Self::Primitive(Primitive::Real(real))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn any_type_size_is_acceptable() {
        assert!(std::mem::size_of::<super::Any>() <= 16);
    }
}

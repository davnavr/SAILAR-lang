use crate::format::{indices, numeric};
use std::fmt::{Display, Formatter};

/// Integer types with a fixed size.
///
/// These are the only valid types for constant integers since `unative` and `snative` may change depending on where the code is
/// being executed.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum FixedInt {
    U8,
    S8,
    U16,
    S16,
    U32,
    S32,
    U64,
    S64,
}

impl FixedInt {
    pub const fn byte_size(self) -> std::num::NonZeroU8 {
        // Safe, there are no zero-sized integer types.
        unsafe {
            std::num::NonZeroU8::new_unchecked(match self {
                Self::U8 | Self::S8 => 1,
                Self::U16 | Self::S16 => 4,
                Self::U32 | Self::S32 => 2,
                Self::U64 | Self::S64 => 8,
            })
        }
    }

    pub const fn is_signed(self) -> bool {
        match self {
            Self::U8 | Self::U16 | Self::U32 | Self::U64 => false,
            Self::S8 | Self::S16 | Self::S32 | Self::S64 => true,
        }
    }
}

impl Display for FixedInt {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_str(match self {
            Self::U8 => "u8",
            Self::S8 => "s8",
            Self::U16 => "u16",
            Self::S16 => "s16",
            Self::U32 => "u32",
            Self::S32 => "s32",
            Self::U64 => "u64",
            Self::S64 => "s64",
        })
    }
}

/// Integer types, including pointer-sized integers.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Int {
    Fixed(FixedInt),
    UNative,
    SNative,
}

impl From<FixedInt> for Int {
    fn from(fixed_type: FixedInt) -> Self {
        Self::Fixed(fixed_type)
    }
}

impl Display for Int {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Fixed(fixed_type) => Display::fmt(fixed_type, f),
            Self::UNative => f.write_str("unative"),
            Self::SNative => f.write_str("snative"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, thiserror::Error)]
#[non_exhaustive]
#[error("{integer_type} is not a valid fixed-length integer type")]
pub struct InvalidFixedIntegerTypeError {
    pub integer_type: Int,
}

impl TryFrom<Int> for FixedInt {
    type Error = InvalidFixedIntegerTypeError;

    fn try_from(integer_type: Int) -> Result<Self, Self::Error> {
        match integer_type {
            Int::Fixed(fixed_type) => Ok(fixed_type),
            _ => Err(InvalidFixedIntegerTypeError { integer_type }),
        }
    }
}

/// Floating-point numeric types.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum Real {
    F32,
    F64,
}

impl Display for Real {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::F32 => f.write_str("f32"),
            Self::F64 => f.write_str("f64"),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Primitive {
    Int(Int),
    Real(Real),
}
macro_rules! primitive_fixed_integer_type {
    ($helper_name: ident, $case_name: ident) => {
        pub fn $helper_name() -> Self {
            Self::Int(Int::Fixed(FixedInt::$case_name))
        }
    };
}

impl Primitive {
    primitive_fixed_integer_type!(s8, S8);
    primitive_fixed_integer_type!(u8, U8);
    primitive_fixed_integer_type!(s16, S16);
    primitive_fixed_integer_type!(u16, U16);
    primitive_fixed_integer_type!(s32, S32);
    primitive_fixed_integer_type!(u32, U32);
    primitive_fixed_integer_type!(s64, S64);
    primitive_fixed_integer_type!(u64, U64);
}

impl From<FixedInt> for Primitive {
    fn from(fixed_type: FixedInt) -> Self {
        Self::Int(Int::Fixed(fixed_type))
    }
}

impl From<Int> for Primitive {
    fn from(integer_type: Int) -> Self {
        Self::Int(integer_type)
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Int(integer_type) => Display::fmt(integer_type, f),
            Self::Real(float_type) => Display::fmt(float_type, f),
        }
    }
}

/// Type representing a fixed-length contiguous sequence of elements of a specified type.
/// # Structure
/// - `length`
/// - `element_type`
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub struct FixedArray {
    element_type: Any,
    length: numeric::UInteger,
}

impl FixedArray {
    pub fn new<L: Into<u32>>(element_type: Any, length: L) -> Self {
        Self {
            element_type,
            length: numeric::UInteger(length.into()),
        }
    }

    pub fn length(&self) -> numeric::UInteger {
        self.length
    }

    pub fn element_type(&self) -> &Any {
        &self.element_type
    }
}

impl Display for FixedArray {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "[{} * {}]", self.element_type, self.length)
    }
}

/// Represents the type of a parameter or a method return type.
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum Any {
    Primitive(Primitive),
    Struct(indices::Struct),
    /// A native pointer to an instance of the specified type.
    NativePointer(Box<Any>),
    FixedArray(Box<FixedArray>),
}

impl std::fmt::Display for Any {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Primitive(primitive) => write!(f, "{}", primitive),
            Self::Struct(index) => write!(f, "struct {}", index),
            Self::NativePointer(pointee_type) => write!(f, "*{}", pointee_type),
            Self::FixedArray(array_type) => write!(f, "{}", array_type),
        }
    }
}

impl From<FixedArray> for Any {
    fn from(array_type: FixedArray) -> Self {
        Self::FixedArray(Box::new(array_type))
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum Tag {
    /// Reserved, the type of things that have no value.
    #[deprecated = "Reserved, no use case for the unit type currently exists"]
    Unit = 0,
    U8 = 1,
    U16 = 2,
    U32 = 4,
    U64 = 8,
    UNative = 0xA,
    S8 = 0x11,
    S16 = 0x12,
    S32 = 0x14,
    S64 = 0x18,
    SNative = 0x1A,
    /// A native pointer type pointing to an instance of the following type.
    NativePointer = 0xCC,
    /// A struct specified by a struct definition index passed by value.
    Struct = 0xDE,
    F32 = 0xF4,
    F64 = 0xF8,
    /// Indicates an array type. Followed by an unsigned integer length, and element type.
    FixedArray = 0xFA,
}

#[derive(Clone, Debug, PartialEq, thiserror::Error)]
#[non_exhaustive]
#[error("invalid type signature {tag:#02X}")]
pub struct InvalidTagError {
    pub tag: u8,
}

impl TryFrom<u8> for Tag {
    type Error = InvalidTagError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(
                #[allow(deprecated)]
                Self::Unit,
            ),
            1 => Ok(Self::U8),
            2 => Ok(Self::U16),
            4 => Ok(Self::U32),
            8 => Ok(Self::U64),
            0xA => Ok(Self::UNative),
            0x11 => Ok(Self::S8),
            0x12 => Ok(Self::S16),
            0x14 => Ok(Self::S32),
            0x18 => Ok(Self::S64),
            0x1A => Ok(Self::SNative),
            0xCC => Ok(Self::NativePointer),
            0xDE => Ok(Self::Struct),
            0xF4 => Ok(Self::F32),
            0xF8 => Ok(Self::F64),
            tag => Err(InvalidTagError { tag }),
        }
    }
}

impl FixedInt {
    pub fn tag(self) -> Tag {
        match self {
            Self::U8 => Tag::U8,
            Self::S8 => Tag::S8,
            Self::U16 => Tag::U16,
            Self::S16 => Tag::S16,
            Self::U32 => Tag::U32,
            Self::S32 => Tag::S32,
            Self::U64 => Tag::U64,
            Self::S64 => Tag::S64,
        }
    }
}

impl Int {
    pub fn tag(self) -> Tag {
        match self {
            Self::Fixed(fixed_type) => fixed_type.tag(),
            Self::UNative => Tag::UNative,
            Self::SNative => Tag::SNative,
        }
    }
}

impl Real {
    pub fn tag(self) -> Tag {
        match self {
            Self::F32 => Tag::F32,
            Self::F64 => Tag::F64,
        }
    }
}

impl Primitive {
    pub fn tag(self) -> Tag {
        match self {
            Self::Int(integer_type) => integer_type.tag(),
            Self::Real(float_type) => float_type.tag(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, thiserror::Error)]
#[non_exhaustive]
#[error("attempt to convert from invalid tag {tag:?}")]
pub struct TryFromTagError {
    pub tag: Tag,
}

impl TryFrom<Tag> for Int {
    type Error = TryFromTagError;

    fn try_from(tag: Tag) -> Result<Self, Self::Error> {
        match tag {
            Tag::S8 => Ok(Self::Fixed(FixedInt::S8)),
            Tag::U8 => Ok(Self::Fixed(FixedInt::U8)),
            Tag::S16 => Ok(Self::Fixed(FixedInt::S16)),
            Tag::U16 => Ok(Self::Fixed(FixedInt::U16)),
            Tag::S32 => Ok(Self::Fixed(FixedInt::S32)),
            Tag::U32 => Ok(Self::Fixed(FixedInt::U32)),
            Tag::S64 => Ok(Self::Fixed(FixedInt::S64)),
            Tag::U64 => Ok(Self::Fixed(FixedInt::U64)),
            Tag::UNative => Ok(Self::UNative),
            Tag::SNative => Ok(Self::SNative),
            _ => Err(TryFromTagError { tag }),
        }
    }
}

impl TryFrom<Tag> for Real {
    type Error = TryFromTagError;

    fn try_from(tag: Tag) -> Result<Self, Self::Error> {
        match tag {
            Tag::F32 => Ok(Self::F32),
            Tag::F64 => Ok(Self::F64),
            _ => Err(TryFromTagError { tag }),
        }
    }
}

impl TryFrom<Tag> for Primitive {
    type Error = TryFromTagError;

    fn try_from(tag: Tag) -> Result<Self, Self::Error> {
        if let Ok(integer_type) = Int::try_from(tag) {
            Ok(Self::Int(integer_type))
        } else if let Ok(real_type) = Real::try_from(tag) {
            Ok(Self::Real(real_type))
        } else {
            Err(TryFromTagError { tag })
        }
    }
}

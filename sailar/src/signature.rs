//! Contains types that model the structure of signatures in the SAILAR binary format.

use crate::helper::borrow::CowBox;
use crate::index;
use crate::num;
use std::fmt::{Debug, Display, Formatter};

macro_rules! type_code {
    ($($(#[$meta:meta])* $name:ident = $value:literal,)*) => {
        /// Tag that indicates the actual type of a type signature.
        ///
        /// Common integer types (s32, u32, s64, etc.) are represented as special cases for space efficiency.
        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
        #[repr(u8)]
        #[non_exhaustive]
        pub enum TypeCode {
            $($(#[$meta])* $name = $value,)*
        }

        impl TryFrom<u8> for TypeCode {
            type Error = InvalidTypeCode;

            fn try_from(value: u8) -> Result<Self, Self::Error> {
                match value {
                    $(_ if value == $value => Ok(Self::$name),)*
                    _ => Err(InvalidTypeCode { value }),
                }
            }
        }
    };
}

type_code! {
    U8 = 1,
    U16 = 2,
    U32 = 3,
    U64 = 4,
    U128 = 5,
    U256 = 6,
    UAddr = 7,
    /// An arbitrary sized unsigned integer type.
    UInt = 8,
    S8 = 9,
    S16 = 0xA,
    S32 = 0xB,
    S64 = 0xC,
    S128 = 0xD,
    S256 = 0xE,
    SAddr = 0xF,
    /// An arbitrary sized signed integer type.
    SInt = 0x10,
    RawPtr = 0xCA,
    VoidPtr = 0xCC, // TODO: Replace void pointer with u8 pointer?
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

#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct Function {
    pub types: Box<[index::TypeSignature]>,
    pub return_type_count: usize,
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
        let types = if parameter_types.is_empty() {
            return_types.into_boxed()
        } else if return_types.is_empty() {
            parameter_types.into_boxed()
        } else {
            return_types.iter().copied().chain(parameter_types.iter().copied()).collect()
        };

        Self::from_boxed_slice(types, return_type_count)
    }

    /// The function signature's return types followed by the parameter types.
    pub fn types(&self) -> &[index::TypeSignature] {
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

/// Represents the integer type sizes supported by SAILAR.
#[derive(Copy, Clone, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct IntegerSize(u8);

impl IntegerSize {
    /// The minimum bit size of an integer type.
    ///
    /// # Examples
    ///
    /// ```
    /// # use sailar::signature::IntegerSize;
    /// assert_eq!(IntegerSize::MIN.bit_size().get(), 1);
    /// ```
    pub const MIN: Self = Self(0);

    /// The maximum bit size of an integer type.
    ///
    /// # Examples
    ///
    /// ```
    /// # use sailar::signature::IntegerSize;
    /// assert_eq!(IntegerSize::MAX.bit_size().get(), 256);
    /// ```
    pub const MAX: Self = Self(u8::MAX);

    /// The size of a byte.
    ///
    /// # Examples
    ///
    /// ```
    /// # use sailar::signature::IntegerSize;
    /// assert_eq!(IntegerSize::I8.bit_size().get(), 8);
    /// ```
    pub const I8: Self = Self(7);

    /// The size of a 16-bit integer.
    ///
    /// # Examples
    ///
    /// ```
    /// # use sailar::signature::IntegerSize;
    /// assert_eq!(IntegerSize::I16.bit_size().get(), 16);
    /// ```
    pub const I16: Self = Self(15);

    /// The size of a 32-bit integer.
    ///
    /// # Examples
    ///
    /// ```
    /// # use sailar::signature::IntegerSize;
    /// assert_eq!(IntegerSize::I32.bit_size().get(), 32);
    /// ```
    pub const I32: Self = Self(31);

    /// The size of a 64-bit integer.
    ///
    /// # Examples
    ///
    /// ```
    /// # use sailar::signature::IntegerSize;
    /// assert_eq!(IntegerSize::I64.bit_size().get(), 64);
    /// ```
    pub const I64: Self = Self(63);

    /// The size of a 128-bit integer.
    ///
    /// # Examples
    ///
    /// ```
    /// # use sailar::signature::IntegerSize;
    /// assert_eq!(IntegerSize::I128.bit_size().get(), 128);
    /// ```
    pub const I128: Self = Self(127);

    /// The size of a 256-bit integer.
    ///
    /// # Examples
    ///
    /// ```
    /// # use sailar::signature::IntegerSize;
    /// assert_eq!(IntegerSize::I256.bit_size().get(), 256);
    /// ```
    pub const I256: Self = Self::MAX;

    pub const fn new(bit_size: std::num::NonZeroU8) -> Self {
        Self(bit_size.get() - 1)
    }

    /// Gets the size of the integer, in bits.
    pub const fn bit_size(self) -> std::num::NonZeroU16 {
        unsafe {
            // Safety: Size is guaranteed to never be zero.
            std::num::NonZeroU16::new_unchecked(self.0 as u16 + 1)
        }
    }
}

impl Debug for IntegerSize {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Debug::fmt(&self.bit_size(), f)
    }
}

impl Display for IntegerSize {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Display::fmt(&self.bit_size(), f)
    }
}

impl From<IntegerSize> for num::VarU28 {
    fn from(size: IntegerSize) -> Self {
        Self::from_u16(size.bit_size().get())
    }
}

impl TryFrom<num::VarU28> for IntegerSize {
    type Error = InvalidIntegerSizeError;

    fn try_from(value: num::VarU28) -> Result<Self, Self::Error> {
        u8::try_from(value)
            .and_then(std::num::NonZeroU8::try_from)
            .map_err(|_| InvalidIntegerSizeError(value))
            .map(Self::new)
    }
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("{0} is not a valid integer size")]
pub struct InvalidIntegerSizeError(num::VarU28);

/// Indicates whether an integer type is signed or unsigned.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum IntegerSign {
    Signed,
    Unsigned,
}

/// Represents a fixed width integer type.
#[derive(Copy, Clone, Eq, Hash, PartialEq)]
pub struct IntegerType {
    sign: IntegerSign,
    size: IntegerSize,
}

impl IntegerType {
    pub const fn new(sign: IntegerSign, size: IntegerSize) -> Self {
        Self { sign, size }
    }

    pub const S8: Self = Self::new(IntegerSign::Signed, IntegerSize::I8);
    pub const U8: Self = Self::new(IntegerSign::Unsigned, IntegerSize::I8);
    pub const S16: Self = Self::new(IntegerSign::Signed, IntegerSize::I16);
    pub const U16: Self = Self::new(IntegerSign::Unsigned, IntegerSize::I16);
    /// The signed 32-bit integer type.
    ///
    /// # Examples
    ///
    /// ```
    /// # use sailar::signature::IntegerType;
    /// assert_eq!(IntegerType::S32.to_string(), "s32");
    /// ```
    pub const S32: Self = Self::new(IntegerSign::Signed, IntegerSize::I32);
    pub const U32: Self = Self::new(IntegerSign::Unsigned, IntegerSize::I32);
    pub const S64: Self = Self::new(IntegerSign::Signed, IntegerSize::I64);
    pub const U64: Self = Self::new(IntegerSign::Unsigned, IntegerSize::I64);
    pub const S128: Self = Self::new(IntegerSign::Signed, IntegerSize::I128);
    pub const U128: Self = Self::new(IntegerSign::Unsigned, IntegerSize::I128);
    pub const S256: Self = Self::new(IntegerSign::Signed, IntegerSize::I256);
    pub const U256: Self = Self::new(IntegerSign::Unsigned, IntegerSize::I256);

    pub const fn sign(self) -> IntegerSign {
        self.sign
    }

    /// Gets the size of the integer, in bits.
    pub const fn size(self) -> IntegerSize {
        self.size
    }
}

impl Debug for IntegerType {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_tuple("IntegerType").field(&self.sign).field(&self.size).finish()
    }
}

impl Display for IntegerType {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        std::fmt::Write::write_char(
            f,
            match self.sign {
                IntegerSign::Signed => 's',
                IntegerSign::Unsigned => 'u',
            },
        )?;
        Display::fmt(&self.size, f)
    }
}

/// Represents a type signature.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type {
    /// An integer type with a fixed size.
    FixedInteger(IntegerType),
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

impl From<IntegerType> for Type {
    fn from(ty: IntegerType) -> Self {
        Self::FixedInteger(ty)
    }
}

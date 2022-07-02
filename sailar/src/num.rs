//! Numeric types in the SAILAR binary format.
//!
//! # Variable-Length Encoding
//!
//! The encoding of variable-width integers in the SAILAR binary format is similar to the encoding used in UTF-8 codepoints. In
//! SAILAR, the high bits indicate the number of bytes needed to contain the integer value.
//!
//! | Actual Value                          |Integer Length (bytes)|Integer Size (bits)|
//! |---------------------------------------|----------------------|-------------------|
//! | `0XXXXXXX`                            | `1`                  | `7`               |
//! | `10XXXXXX XXXXXXXX`                   | `2`                  | `14`              |
//! | `110XXXXX XXXXXXXX XXXXXXXX`          | `3`                  | `21`              |
//! | `1110XXXX XXXXXXXX XXXXXXXX XXXXXXXX` | `4`                  | `32`              |
//!
//! For simplicity, the SAILAR binary format currently only allows a maximum length of `4` for all integers.

use std::num::NonZeroU32;

#[derive(Debug, thiserror::Error)]
#[error("integers of byte length {length} are not supported by SAILAR")]
pub struct IntegerLengthError {
    length: u8,
}

/// An unsigned integer represented in a SAILAR binary as a 1, 2, 3, or 4 byte long integer.
///
/// For more details, see the documentation for the this module.
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct VarU28(NonZeroU32); // Actual value stored in bits 1-28, allows null pointer optimization

impl VarU28 {
    /// Creates a new unsigned integer without checking that the value can fit in 28 bits.
    ///
    /// # Safety
    ///
    /// Callers should ensure that the value is small enough to fit in 28 bits.
    pub const unsafe fn new_unchecked(value: u32) -> Self {
        Self(NonZeroU32::new_unchecked(1u32 | (value << 1)))
    }

    pub const MIN: Self = unsafe {
        // Safety: 0 is a valid value
        Self::new_unchecked(0)
    };

    /// The largest value that is allowed to be encoded.
    ///
    /// # Examples
    ///
    /// ```
    /// # use sailar::num::VarU28;
    /// assert_eq!(VarU28::MAX.get() >> VarU28::BITS, 0);
    /// ```
    pub const MAX: Self = unsafe {
        // Safety: Maximum value is always valid
        Self::new_unchecked(0x0FFF_FFFF)
    };

    pub const BITS: u32 = 28u32;

    pub const fn get(self) -> u32 {
        self.0.get() >> 1
    }

    /// Creates a new unsigned integer, returning `None` if the value is too large to be represented in 28 bits.
    pub const fn new(value: u32) -> Option<Self> {
        if value & Self::MAX.get() == 0 {
            None
        } else {
            unsafe {
                // Safety: Validation above ensures value is valid
                Some(Self::new_unchecked(value))
            }
        }
    }

    pub const fn from_u8(value: u8) -> Self {
        unsafe {
            // Safety: Single byte can always be encoded
            Self::new_unchecked(value as u32)
        }
    }

    /// The maximum value that can be encoded in 1 byte.
    ///
    /// # Examples
    ///
    /// ```
    /// # use sailar::num::VarU28;
    /// assert!(VarU28::MIN < VarU28::MAX_1);
    /// ```
    pub const MAX_1: Self = Self::from_u8(0x7F);

    pub const fn from_u16(value: u16) -> Self {
        unsafe {
            // Safety: 28-bit integer can contain 16-bit integer
            Self::new_unchecked(value as u32)
        }
    }

    /// The maximum value that can be encoded in 2 bytes.
    ///
    /// # Examples
    ///
    /// ```
    /// # use sailar::num::VarU28;
    /// assert!(VarU28::MAX_2 < VarU28::MAX_3);
    /// ```
    pub const MAX_2: Self = Self::from_u16(0x3FF);

    /// The maximum value that can be encoded in 3 bytes.
    ///
    /// # Examples
    ///
    /// ```
    /// # use sailar::num::VarU28;
    /// assert!(VarU28::MAX_3 < VarU28::MAX_4);
    /// ```
    pub const MAX_3: Self = unsafe {
        // Safety: 28-bit integer can contain 24-bit integer
        Self::new_unchecked(0x1FFFFF)
    };

    /// The maximum value that can be encoded in 4 bytes.
    pub const MAX_4: Self = Self::MAX;

    /// Gets the number of bytes needed to contain this integer value.
    ///
    /// # Examples
    ///
    /// ```
    /// # use sailar::num::VarU28;
    /// assert_eq!(VarU28::from_u8(1).byte_length().get(), 1);
    /// assert_eq!(VarU28::MAX_1.byte_length().get(), 1);
    /// assert_eq!(VarU28::from_u8(u8::MAX).byte_length().get(), 2);
    /// assert_eq!(VarU28::from_u16(u16::MAX).byte_length().get(), 3);
    /// assert_eq!(VarU28::MAX.byte_length().get(), 4);
    /// ```
    pub fn byte_length(self) -> std::num::NonZeroU8 {
        unsafe {
            // Safety: All byte lengths are never zero
            std::num::NonZeroU8::new_unchecked(if self <= Self::MAX_1 {
                1u8
            } else if self <= Self::MAX_2 {
                2
            } else if self <= Self::MAX_3 {
                3
            } else if self <= Self::MAX_4 {
                4
            } else {
                unreachable!()
            })
        }
    }

    /// Reads a variable-length integer value.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # use sailar::num::VarU28;
    /// assert!(matches!(VarU28::read_from([42u8].as_slice()), Ok(Ok(n)) if n.get() == 42));
    /// assert!(matches!(VarU28::read_from([0x80u8].as_slice()), Err(_)));
    /// ```
    pub fn read_from<R: std::io::Read>(mut source: R) -> std::io::Result<Result<Self, IntegerLengthError>> {
        let leading_byte = {
            let mut buffer = [0u8];
            source.read_exact(&mut buffer)?;
            buffer[0]
        };

        match leading_byte.leading_ones() {
            0 => Ok(Ok(Self::from_u8(leading_byte))),
            1 => {
                let mut buffer = [0u8];
                source.read_exact(&mut buffer)?;
                let high_bits = (buffer[0] as u16) >> 2;
                Ok(Ok(Self::from_u16(((0x3Fu8 & leading_byte) as u16) | high_bits)))
            }
            2 => {
                let mut buffer = [0u8; 2];
                source.read_exact(&mut buffer)?;
                let high_bits = (u16::from_le_bytes(buffer) as u32) >> 3;
                Ok(Ok(unsafe {
                    // Safety: All 24-bit integers are valid
                    Self::new_unchecked(((0x1Fu8 & leading_byte) as u32) | high_bits)
                }))
            }
            3 => {
                let mut buffer = [0u8; 4];
                source.read_exact(&mut buffer[1..])?;
                Ok(Ok(unsafe {
                    // Safety: All 28-bit integers are valid
                    Self::new_unchecked(((0xFu8 & leading_byte) as u32) | (u32::from_le_bytes(buffer) >> 4))
                }))
            }
            byte_length => Ok(Err(IntegerLengthError {
                length: byte_length.try_into().unwrap(),
            })),
        }
    }
}

macro_rules! integer_format_trait_impl {
    ($trait: ty) => {
        impl $trait for VarU28 {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                <u32 as $trait>::fmt(&self.get(), f)
            }
        }
    };
}

integer_format_trait_impl!(std::fmt::Debug);
integer_format_trait_impl!(std::fmt::Display);
integer_format_trait_impl!(std::fmt::Binary);
integer_format_trait_impl!(std::fmt::UpperHex);
integer_format_trait_impl!(std::fmt::LowerHex);
integer_format_trait_impl!(std::fmt::Octal);

macro_rules! other_from_integer_trait_impl {
    ($destination: ty) => {
        impl From<VarU28> for $destination {
            fn from(value: VarU28) -> Self {
                Self::from(value.get())
            }
        }
    };
}

other_from_integer_trait_impl!(u32);
other_from_integer_trait_impl!(i64);
other_from_integer_trait_impl!(u64);
other_from_integer_trait_impl!(i128);
other_from_integer_trait_impl!(u128);

macro_rules! integer_from_other_trait_impl {
    ($source: ty, $constructor: ident) => {
        impl From<$source> for VarU28 {
            #[inline]
            fn from(value: $source) -> Self {
                Self::$constructor(value)
            }
        }
    };
}

integer_from_other_trait_impl!(u8, from_u8);
integer_from_other_trait_impl!(u16, from_u16);

#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum VarIntSize {
    One = 0u8,
    Two = 1,
    Four = 2,
}

impl VarIntSize {
    pub fn maximum_length_value(self) -> usize {
        match self {
            Self::One => u8::MAX.into(),
            Self::Two => u16::MAX.into(),
            Self::Four => u32::MAX.try_into().unwrap_or(usize::MAX),
        }
    }

    pub const fn byte_count(self) -> u8 {
        match self {
            Self::One => 1,
            Self::Two => 2,
            Self::Four => 4,
        }
    }

    pub fn pick_largest(&mut self, other: VarIntSize) {
        if other > *self {
            *self = other;
        }
    }

    /// Updates the length size value if the specified length cannot fit it.
    ///
    /// # Panics
    ///
    /// As indices are currently limited to four bytes, this panics if the `length` is greater than [`u32::MAX`].
    pub fn resize_to_fit(&mut self, length: usize) {
        match self {
            Self::One if length > u8::MAX.into() => *self = Self::Two,
            Self::One => (),
            Self::Two if length > u16::MAX.into() => *self = Self::Four,
            Self::Two => (),
            Self::Four if length <= u32::MAX.try_into().unwrap_or(usize::MAX) => (),
            Self::Four => panic!("length value too large: {}", length),
        }
    }

    pub fn resize_to_fit_many<T, I: std::iter::IntoIterator<Item = T>, F: FnMut(T) -> usize>(
        &mut self,
        items: I,
        mut length_source: F,
    ) {
        for item in items.into_iter() {
            self.resize_to_fit(length_source(item))
        }
    }
}

impl From<VarIntSize> for u8 {
    #[inline]
    fn from(value: VarIntSize) -> u8 {
        value as u8
    }
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("{0:#02X} is not a valid length size value")]
pub struct InvalidVarIntSize(u8);

impl TryFrom<u8> for VarIntSize {
    type Error = InvalidVarIntSize;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::One),
            1 => Ok(Self::Two),
            2 => Ok(Self::Four),
            _ => Err(InvalidVarIntSize(value)),
        }
    }
}

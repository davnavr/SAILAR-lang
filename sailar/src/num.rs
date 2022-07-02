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

#[derive(Clone, Debug, thiserror::Error)]
#[error("integers of byte length {length} are not supported by SAILAR")]
pub struct IntegerLengthError {
    length: u8,
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("integer too large to be encoded")]
pub struct IntegerEncodingError(());

impl From<std::num::TryFromIntError> for IntegerEncodingError {
    fn from(_: std::num::TryFromIntError) -> Self {
        Self(())
    }
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

    /// The smallest value that can be encoded.
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

    /// Gets the value of this integer.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # use sailar::num::VarU28;
    /// assert_eq!(VarU28::from_u16(99).get(), 99);
    /// ```
    pub const fn get(self) -> u32 {
        self.0.get() >> 1
    }

    /// Creates a new unsigned integer, returning `None` if the value is too large to be represented in 28 bits.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # use sailar::num::VarU28;
    /// assert_eq!(VarU28::new(99), Some(VarU28::from_u16(99)));
    /// assert_eq!(VarU28::new(0x1000_0000), None);
    /// assert_eq!(VarU28::new(u32::MAX), None);
    /// ```
    pub const fn new(value: u32) -> Option<Self> {
        if value & !Self::MAX.get() != 0 {
            None
        } else {
            unsafe {
                // Safety: Validation above ensures value is valid
                Some(Self::new_unchecked(value))
            }
        }
    }

    /// Creates an unsigned integer from an unsigned byte value.
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

    /// Creates an unsigned integer from an unsigned 16-bit integer.
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
                unreachable!("value above maximum is not valid")
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

    /// Writes a variable-length integer value.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # use sailar::num::VarU28;
    /// let mut buffer = [0u8; 4];
    /// VarU28::from_u8(86).write_to(buffer.as_mut_slice()).unwrap();
    /// assert_eq!(buffer[0], 86);
    /// VarU28::from_u8(128).write_to(buffer.as_mut_slice()).unwrap();
    /// assert_eq!(&buffer[..2], &[0x80, 0x01]);
    /// ```
    pub fn write_to<W: std::io::Write>(self, mut destination: W) -> std::io::Result<()> {
        let value = self.get();
        match self.byte_length().get() {
            1 => destination.write_all(&[value as u8]),
            2 => {
                let value = value as u16;
                let mut buffer = (value << 1).to_le_bytes();
                buffer[0] = (value.to_le_bytes()[0] & 0x3Fu8) | 0x80u8;
                destination.write_all(&buffer)
            }
            3 => {
                let mut buffer = (value << 2).to_le_bytes();
                buffer[0] = (value.to_le_bytes()[0] & 0x1Fu8) | 0b1100_0000u8;
                destination.write_all(&buffer[..3])
            }
            4 => {
                let mut buffer = (value << 3).to_le_bytes();
                buffer[0] = (value.to_le_bytes()[0] & 0xFu8) | 0b1110_0000u8;
                destination.write_all(&buffer)
            }
            _ => unreachable!("unsupported byte length"),
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

impl TryFrom<u32> for VarU28 {
    type Error = IntegerEncodingError;

    fn try_from(value: u32) -> Result<Self, IntegerEncodingError> {
        Self::new(value).ok_or(IntegerEncodingError(()))
    }
}

macro_rules! integer_try_from_trait_impl {
    ($source: ty) => {
        impl TryFrom<$source> for VarU28 {
            type Error = IntegerEncodingError;

            fn try_from(value: $source) -> Result<Self, IntegerEncodingError> {
                Self::try_from(u32::try_from(value)?)
            }
        }
    };
}

integer_try_from_trait_impl!(i8);
integer_try_from_trait_impl!(i16);
integer_try_from_trait_impl!(i32);
integer_try_from_trait_impl!(u64);
integer_try_from_trait_impl!(i64);
integer_try_from_trait_impl!(u128);
integer_try_from_trait_impl!(i128);
integer_try_from_trait_impl!(usize);
integer_try_from_trait_impl!(isize);

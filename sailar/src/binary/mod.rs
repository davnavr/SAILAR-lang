//! Contains types to abstract over the binary representation of the SAILAR format.

pub mod buffer;
pub mod signature;

/// The magic number that is the start of all SAILAR module files.
pub const MAGIC: &[u8; 6] = b"SAILAR";

/// Represents an array of bytes that make up a SAILAR module.
#[derive(Clone)]
pub struct RawModule {
    contents: Vec<u8>,
}

impl RawModule {
    pub(crate) fn from_vec(contents: Vec<u8>) -> Self {
        Self { contents }
    }

    pub fn bytes(&self) -> &[u8] {
        &self.contents
    }
}

impl std::ops::Deref for RawModule {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        self.bytes()
    }
}

impl std::fmt::Debug for RawModule {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "RawModule({:?})", buffer::ByteDebug::from(&self.contents))
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum LengthSize {
    One = 0u8,
    Two = 1,
    Four = 2,
}

impl LengthSize {
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

impl From<LengthSize> for u8 {
    #[inline]
    fn from(value: LengthSize) -> u8 {
        value as u8
    }
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("{0:#02X} is not a valid length size value")]
pub struct InvalidLengthSize(u8);

impl TryFrom<u8> for LengthSize {
    type Error = InvalidLengthSize;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::One),
            1 => Ok(Self::Two),
            2 => Ok(Self::Four),
            _ => Err(InvalidLengthSize(value)),
        }
    }
}

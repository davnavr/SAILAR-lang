//! Numeric types in the SAILAR binary format.

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

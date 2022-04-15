//! Contains types to abstract over the binary representation of the SAILAR format.

pub mod buffer;

/// The magic number that is the start of all SAILAR module files.
pub const MAGIC: &[u8; 7] = b"SAILAR\0";

/// Represents an array of bytes that make up a SAILAR module.
#[derive(Clone, Debug)]
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

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum LengthSize {
    One = 0u8,
    Two = 1,
    Four = 2,
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

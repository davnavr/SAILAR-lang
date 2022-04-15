//! Contains types to abstract over the binary representation of the SAILAR format.

pub mod buffer;

/// The magic number that is the start of all SAILAR module files.
pub static MAGIC: &[u8; 7] = b"SAILAR\0";

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

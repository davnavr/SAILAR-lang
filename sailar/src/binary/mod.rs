//! Contains types to abstract over the binary representation of the SAILAR format.

mod buffer;
mod num;
mod writer;

pub mod builder;
pub mod index;
pub mod reader;
pub mod record;
pub mod signature;

pub use builder::Builder;
pub use num::{InvalidVarIntSize, VarIntSize};

/// The magic number that is the start of all SAILAR module files.
pub const MAGIC: &[u8; 6] = b"SAILAR";

/// Represents an array of bytes that make up a syntactically correct and valid SAILAR module.
#[derive(Clone)]
pub struct RawModule {
    contents: Vec<u8>,
}

impl RawModule {
    /// Creates a module from a vector of bytes.
    ///
    /// # Safety
    ///
    /// Callers should ensure that the content of the vector is a syntactically correct SAILAR module.
    pub unsafe fn from_vec_unchecked(contents: Vec<u8>) -> Self {
        Self { contents }
    }

    pub fn bytes(&self) -> &[u8] {
        &self.contents
    }

    /// Dumps the raw contents of this module in neatly-formatted hexadecimal.
    pub fn hex_dump<W: std::fmt::Write>(&self, output: &mut W) -> std::fmt::Result {
        buffer::hex_dump(&self.contents, output)
    }

    pub fn hex_dump_to_string(&self) -> String {
        let mut output = String::new();
        self.hex_dump(&mut output)
            .expect("unexpected error occured while dumping contents");
        output
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

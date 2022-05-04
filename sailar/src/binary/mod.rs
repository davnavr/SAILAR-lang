//! Contains types to abstract over the binary representation of the SAILAR format.

mod num;

pub mod buffer;
pub mod module;
pub mod signature;

pub use num::{InvalidVarIntSize, VarIntSize};

/// The magic number that is the start of all SAILAR module files.
pub const MAGIC: &[u8; 6] = b"SAILAR";

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum RecordType {
    Array = 1,
    Identifier = 2,
    TypeSignature = 3,
    FunctionSignature = 4,
    Data = 5,
    Code = 6,
    ModuleImport = 7,
    FunctionImport = 8,
    StructureImport = 9,
    GlobalImport = 10,
    FunctionDefinition = 11,
    StructureDefinition = 12,
    GlobalDefinition = 13,
    FunctionInstantiation = 14,
    StructureInstantiation = 15,
    Namespace = 16,
    //ExceptionClassImport = 17,
    //ExceptionClassDefinition = 18,
    //AnnotationClassImport = 19,
    //AnnotationClassDefinition = 20,
    //DebuggingInformation = 21,
}

impl From<RecordType> for u8 {
    fn from(value: RecordType) -> u8 {
        value as u8
    }
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("{value:#02X} is not a valid record type")]
pub struct InvalidRecordTypeError {
    value: u8,
}

impl TryFrom<u8> for RecordType {
    type Error = InvalidRecordTypeError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value <= 16 {
            Ok(unsafe { std::mem::transmute::<u8, Self>(value) })
        } else {
            Err(InvalidRecordTypeError { value })
        }
    }
}

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

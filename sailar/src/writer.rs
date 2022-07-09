//! Low-level internal API for writing the contents of a SAILAR binary module.

use crate::identifier::Id;
use crate::num::VarU28;
use std::io::Write;

pub type Result = std::io::Result<()>;

#[derive(Debug)]
pub struct Writer<W> {
    destination: W,
}

pub type VecWriter<'a> = Writer<&'a mut Vec<u8>>;

impl<W: Write> Writer<W> {
    pub fn new(destination: W) -> Self {
        Self { destination }
    }

    pub fn write_byte(&mut self, value: u8) -> Result {
        self.destination.write_all(std::slice::from_ref(&value))
    }

    pub fn write_unsigned_integer<I: Into<VarU28>>(&mut self, value: I) -> Result {
        value.into().write_to(&mut self.destination)
    }

    pub fn write_length<I>(&mut self, value: I) -> Result
    where
        I: TryInto<VarU28>,
        I::Error: Into<Box<dyn std::error::Error + Send + Sync>>,
    {
        match value.try_into() {
            Ok(value) => self.write_unsigned_integer(value),
            Err(err) => Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, err)),
        }
    }

    pub fn write_identifier(&mut self, identifier: &Id) -> Result {
        let bytes = identifier.as_bytes();
        self.write_length(bytes.len())?;
        self.write_all(bytes)
    }
}

impl<W> std::ops::Deref for Writer<W> {
    type Target = W;

    fn deref(&self) -> &W {
        &self.destination
    }
}

impl<W> std::ops::DerefMut for Writer<W> {
    fn deref_mut(&mut self) -> &mut W {
        &mut self.destination
    }
}

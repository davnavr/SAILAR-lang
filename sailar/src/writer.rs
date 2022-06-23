//! Low-level internal API for writing the contents of a SAILAR binary module.

use crate::identifier::Id;
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

    pub fn write_integer<V: Into<usize>>(&mut self, value: V) -> Result {
        match u32::try_from(value.into()) {
            Ok(value) => self.write_all(&value.to_le_bytes()),
            Err(err) => unreachable!("integer too large: {}", err),
        }
    }

    pub fn write_identifier(&mut self, identifier: &Id) -> Result {
        let bytes = identifier.as_bytes();
        self.write_integer(bytes.len())?;
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

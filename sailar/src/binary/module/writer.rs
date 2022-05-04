use crate::binary::VarIntSize;
use std::io::{Error, ErrorKind, Write};

pub type Result = std::io::Result<()>;

pub type IntegerWriter<W> = fn(&mut Writer<W>, usize) -> Result;

macro_rules! integer_writer {
    ($integer_type: ty) => {
        |out: &mut Writer<W>, value: usize| match <$integer_type>::try_from(value) {
            Ok(value) => out.write_all(&value.to_le_bytes()),
            Err(err) => Err(Error::new(ErrorKind::InvalidInput, err)),
        }
    };
}

fn select_integer_writer<W: Write>(integer_size: VarIntSize) -> IntegerWriter<W> {
    match integer_size {
        VarIntSize::One => integer_writer!(u8),
        VarIntSize::Two => integer_writer!(u16),
        VarIntSize::Four => integer_writer!(u32),
    }
}

pub struct Writer<W> {
    destination: W,
    integer_size: VarIntSize,
    integer_writer: IntegerWriter<W>,
}

pub type VecWriter<'a> = Writer<&'a mut Vec<u8>>;

impl<W: Write> Writer<W> {
    pub fn new(destination: W, integer_size: VarIntSize) -> Self {
        Self {
            destination,
            integer_size,
            integer_writer: select_integer_writer(integer_size),
        }
    }

    #[inline]
    pub fn write_integer<V: Into<usize>>(&mut self, value: V) -> Result {
        (self.integer_writer)(self, value.into())
    }

    pub fn write_identifier(&mut self, identifier: &crate::Id) -> Result {
        let bytes = identifier.as_bytes();
        self.write_integer(bytes.len())?;
        self.write_all(bytes)
    }

    pub fn derive_from<O: Write>(&self, other: O) -> Writer<O> {
        Writer {
            destination: other,
            integer_size: self.integer_size,
            integer_writer: select_integer_writer(self.integer_size),
        }
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

impl<W: std::fmt::Debug> std::fmt::Debug for Writer<W> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Writer")
            .field("destination", &self.destination)
            .field("integer_size", &self.integer_size)
            .finish()
    }
}

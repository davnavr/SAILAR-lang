//! Low-level API containing types that represent the contents of a SAILAR module binary.

use crate::binary::VarIntSize;
use crate::{FormatVersion, Id};

#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum Record<'a> {
    Identifier(&'a Id),
}

/// Represents the content of a SAILAR module.
#[derive(Clone, Debug)]
pub struct Module<'a> {
    format_version: FormatVersion,
    integer_size: VarIntSize,
    records: Vec<Record<'a>>,
}

impl<'a> Module<'a> {
    pub fn new() -> Self {
        Self {
            format_version: FormatVersion::CURRENT.clone(),
            integer_size: VarIntSize::One,
            records: Vec::default(),
        }
    }

    pub fn add_record(&mut self, record: Record<'a>) {
        self.records.push(record);
        self.integer_size.resize_to_fit(self.records.len());
    }

    pub fn write_to<W: std::io::Write>(&self, mut destination: W) -> std::io::Result<()> {
        use std::io::{Error, ErrorKind, Write};

        type Result = std::io::Result<()>;

        destination.write_all(crate::binary::MAGIC)?;
        destination.write_all(&[self.format_version.major, self.format_version.minor, self.integer_size.into()])?;

        macro_rules! integer_writer {
            ($integer_type: ty) => {
                |destination: &mut W, value: usize| match <$integer_type>::try_from(value) {
                    Ok(value) => destination.write_all(&value.to_le_bytes()),
                    Err(err) => Err(Error::new(ErrorKind::InvalidInput, err)),
                }
            };
        }

        let write_integer: fn(&mut W, usize) -> Result = match self.integer_size {
            VarIntSize::One => integer_writer!(u8),
            VarIntSize::Two => integer_writer!(u16),
            VarIntSize::Four => integer_writer!(u32),
        };

        Ok(())
    }
}

//! Allows the reuse of byte buffers.

use std::fmt::{Debug, Formatter, Write as _};

#[repr(transparent)]
pub struct ByteDebug<'a>(&'a [u8]);

impl<'a> From<&'a [u8]> for ByteDebug<'a> {
    fn from(bytes: &'a [u8]) -> Self {
        Self(bytes)
    }
}

impl<'a, const L: usize> From<&'a [u8; L]> for ByteDebug<'a> {
    fn from(bytes: &'a [u8; L]) -> Self {
        Self(bytes.as_slice())
    }
}

impl<'a> From<&'a Vec<u8>> for ByteDebug<'a> {
    fn from(bytes: &'a Vec<u8>) -> Self {
        Self(bytes.as_slice())
    }
}

impl Debug for ByteDebug<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_char('[')?;
        for (index, value) in self.0.iter().enumerate() {
            if index > 0 && index < self.0.len() - 1 {
                f.write_char(',')?;
            }

            write!(f, "{:#02X}", value)?;
        }
        f.write_char(']')
    }
}

pub fn hex_dump<W: std::fmt::Write>(bytes: &[u8], out: &mut W) -> std::fmt::Result {
    //00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ........ ........
    const HEX_LENGTH: usize = 48;
    const ASCII_LENGTH: usize = 17;

    let mut offset = 0usize;
    let mut hex_buffer = String::with_capacity(HEX_LENGTH);
    let mut ascii_buffer = String::with_capacity(ASCII_LENGTH);

    out.write_str("         00 01 02 03 04 05 06 07  08 09 0A 0B 0C 0D 0E 0F\n")?;

    for line in bytes.chunks(16) {
        hex_buffer.clear();
        ascii_buffer.clear();

        for (index, value) in line.iter().copied().enumerate() {
            if index > 0 {
                hex_buffer.push(' ');
            }

            if index == 8 {
                hex_buffer.push(' ');
                ascii_buffer.push(' ');
            }

            write!(&mut hex_buffer, "{:02X}", value)?;

            ascii_buffer.push(if (b'!'..=b'~').contains(&value) { value as char } else { '.' });
        }

        for _ in 0..(HEX_LENGTH - hex_buffer.len()) {
            hex_buffer.push(' ');
        }

        write!(out, "{:08X} ", offset)?;
        out.write_str(&hex_buffer)?;
        out.write_str("  ")?;
        out.write_str(&ascii_buffer)?;
        out.write_char('\n')?;

        offset += line.len();
    }

    Ok(())
}

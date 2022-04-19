//! Allows the reuse of byte buffers.

use std::cell::RefCell;
use std::fmt::Write;

#[derive(Clone, Debug, Default)]
pub struct Pool {
    // TODO: Use a collection that keeps it sorted by capacity, to allow requests of a certain capacity to return a vec that meets it.
    buffers: RefCell<Vec<Vec<u8>>>,
}

pub struct Rented<'a> {
    pool: &'a Pool,
    buffer: Vec<u8>,
}

impl Pool {
    pub fn rent(&self) -> Rented<'_> {
        Rented {
            pool: self,
            buffer: match self.buffers.borrow_mut().pop() {
                Some(mut buffer) => {
                    buffer.clear();
                    buffer
                }
                None => Vec::new(),
            },
        }
    }

    /// Rents a buffer with the specified `capacity`, meaning that it will have space reserved for at least `capacity` bytes.
    pub fn rent_with_capacity(&self, capacity: usize) -> Rented<'_> {
        let mut buffer = self.rent();
        if buffer.capacity() < capacity {
            // The buffer is empty, so reserve will ensure the capacity is at least as many bytes as requested.
            buffer.reserve(capacity);
        }
        buffer
    }

    /// Rents a buffer with the specified `length`, filling the buffer with zeroes.
    pub fn rent_with_length(&self, length: usize) -> Rented<'_> {
        let mut rented = self.rent_with_capacity(length);
        rented.resize(length, 0);
        rented
    }

    pub(crate) fn existing_or_default(pool: Option<&Self>) -> std::borrow::Cow<'_, Self> {
        match pool {
            Some(pool) => std::borrow::Cow::Borrowed(pool),
            None => std::borrow::Cow::Owned(Self::default()),
        }
    }
}

impl Rented<'_> {
    #[inline]
    pub fn to_vec(&self) -> Vec<u8> {
        self.buffer.clone()
    }

    #[inline]
    pub fn as_vec(&self) -> &Vec<u8> {
        &self.buffer
    }

    #[inline]
    pub fn as_mut_vec(&mut self) -> &mut Vec<u8> {
        &mut self.buffer
    }
}

impl std::fmt::Debug for Rented<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Rented").field("buffer", &ByteDebug(&self.buffer)).finish()
    }
}

impl<'a> std::ops::Deref for Rented<'a> {
    type Target = Vec<u8>;

    #[inline]
    fn deref(&self) -> &Vec<u8> {
        self.as_vec()
    }
}

impl<'a> std::ops::DerefMut for Rented<'a> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut_vec()
    }
}

impl<'a> Drop for Rented<'a> {
    fn drop(&mut self) {
        self.pool.buffers.borrow_mut().push(std::mem::take(&mut self.buffer));
    }
}

#[derive(Debug)]
pub enum RentedOrOwned<'a> {
    Rented(Rented<'a>),
    Owned(Vec<u8>),
}

impl<'a> RentedOrOwned<'a> {
    pub fn with_capacity(capacity: usize, pool: Option<&'a Pool>) -> Self {
        match pool {
            None => Self::Owned(Vec::with_capacity(capacity)),
            Some(pool) => Self::Rented(pool.rent_with_capacity(capacity)),
        }
    }

    pub fn as_slice(&self) -> &[u8] {
        match self {
            Self::Rented(rented) => rented,
            Self::Owned(owned) => owned,
        }
    }

    pub fn as_mut_slice(&mut self) -> &mut [u8] {
        match self {
            Self::Rented(rented) => rented,
            Self::Owned(ref mut owned) => owned,
        }
    }

    pub fn as_vec(&self) -> &Vec<u8> {
        match self {
            Self::Rented(rented) => &rented.buffer,
            Self::Owned(owned) => owned,
        }
    }

    pub fn as_mut_vec(&mut self) -> &mut Vec<u8> {
        match self {
            Self::Rented(rented) => &mut rented.buffer,
            Self::Owned(owned) => owned,
        }
    }

    pub fn into_vec(self) -> Vec<u8> {
        match self {
            Self::Rented(rented) => rented.clone(),
            Self::Owned(owned) => owned,
        }
    }
}

crate::enum_case_from_impl!(RentedOrOwned<'_>, Owned, Vec<u8>);

impl<'a> From<Rented<'a>> for RentedOrOwned<'a> {
    fn from(rented: Rented<'a>) -> Self {
        Self::Rented(rented)
    }
}

impl<'a> std::ops::Deref for RentedOrOwned<'a> {
    type Target = Vec<u8>;

    #[inline]
    fn deref(&self) -> &Vec<u8> {
        self.as_vec()
    }
}

impl<'a> std::ops::DerefMut for RentedOrOwned<'a> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Vec<u8> {
        self.as_mut_vec()
    }
}

#[repr(transparent)]
pub(crate) struct ByteDebug<'a>(&'a [u8]);

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

impl std::fmt::Debug for ByteDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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

pub(crate) fn hex_dump<W: std::fmt::Write>(bytes: &[u8], out: &mut W) -> std::fmt::Result {
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

            ascii_buffer.push(if value >= b'!' && value <= b'~' { value as char } else { '.' });
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

#[cfg(test)]
mod tests {
    use crate::binary::buffer;

    #[test]
    fn buffer_rent_test() {
        let pool = buffer::Pool::default();

        {
            let mut buffer = pool.rent();
            buffer.push(1);
            buffer.push(2);
        }

        assert_eq!(pool.rent().len(), 0);
        assert!(pool.rent().capacity() > 0);
    }
}

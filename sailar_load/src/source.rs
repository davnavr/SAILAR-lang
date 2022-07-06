//! Contains the trait used to read the contents of a module during loading.

use crate::module::Record;
use sailar::reader::{self, Reader};
use std::io::Read;

pub trait Source {
    type Error;

    fn iter_records<F: FnMut(Record)>(self, f: F) -> Result<(), Self::Error>;
}

impl<E, I: std::iter::Iterator<Item = Result<Record, E>>> Source for I {
    type Error = E;

    #[inline]
    fn iter_records<F: FnMut(Record)>(self, mut f: F) -> Result<(), E> {
        for value in self {
            match value {
                Ok(record) => f(record),
                Err(e) => return Err(e),
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub struct ReaderSource<S>(Reader<S>);

impl<S: Read> From<Reader<S>> for ReaderSource<S> {
    #[inline]
    fn from(reader: Reader<S>) -> Self {
        Self(reader)
    }
}

impl<S: Read> Source for ReaderSource<S> {
    type Error = reader::Error;

    fn iter_records<F: FnMut(Record)>(self, mut f: F) -> Result<(), Self::Error> {
        let (_, _, mut record_reader) = self.0.to_record_reader()?;
        while let Some(value) = record_reader.next_record() {
            match value {
                Ok(record) => f(record),
                Err(e) => return Err(e),
            }
        }
        record_reader.finish()
    }
}

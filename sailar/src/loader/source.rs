//! Contains the trait used to read the contents of a module during loading.

use crate::binary;
use crate::loader::module::Record;

pub trait Source {
    type Error;

    fn iter_records<F: FnMut(Record)>(self, f: F) -> Result<(), Self::Error>;
}

impl<E, I: std::iter::Iterator<Item = Result<Record, E>>> Source for I {
    type Error = E;

    #[inline]
    fn iter_records<F: FnMut(Record)>(self, f: F) -> Result<(), E> {
        for value in self {
            match value {
                Ok(record) => f(record),
                Err(e) => return Err(e),
            }
        }

        Ok(())
    }
}

impl<S: std::io::Read> Source for binary::reader::Reader<S> {
    type Error = binary::reader::Error;

    fn iter_records<F: FnMut(Record)>(self, f: F) -> Result<(), Self::Error> {
        let (_, _, mut record_reader) = self.to_record_reader()?;
        record_reader.iter_records(f)?;
        record_reader.finish()
    }
}

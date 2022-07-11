//! Contains the trait used to read the contents of a module during loading.

use crate::error::GenericError;
use crate::module::Record;
use sailar::reader;
use std::convert::Infallible;
use std::io::Read;

pub trait Source {
    type Error;
    type Records: Iterator<Item = Result<Record, Self::Error>>;

    fn source(self) -> Result<Self::Records, Self::Error>;
}

impl<S: Source> Source for Box<S> {
    type Error = S::Error;
    type Records = S::Records;

    fn source(self) -> Result<Self::Records, Self::Error> {
        S::source(*self)
    }
}

#[derive(Copy, Clone, Debug, Default)]
#[non_exhaustive]
pub struct Empty;

impl Source for Empty {
    type Error = Infallible;
    type Records = std::iter::Empty<Result<Record, Infallible>>;

    fn source(self) -> Result<Self::Records, Self::Error> {
        Ok(std::iter::empty())
    }
}

/// A module source yielding an empty sequence of records.
#[must_use]
pub fn empty() -> Empty {
    Empty
}

#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct RecordIterator<I>(I);

impl<I: Iterator<Item = Record>> From<I> for RecordIterator<I> {
    fn from(iterator: I) -> Self {
        Self(iterator)
    }
}

impl<I: Iterator<Item = Record>> Iterator for RecordIterator<I> {
    type Item = Result<Record, Infallible>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(Ok)
    }
}

impl<I: Iterator<Item = Record>> Source for RecordIterator<I> {
    type Error = Infallible;
    type Records = Self;

    fn source(self) -> Result<Self, Infallible> {
        Ok(self)
    }
}

impl<'s> Source for &'s mut Vec<Record> {
    type Error = Infallible;
    type Records = RecordIterator<std::vec::Drain<'s, Record>>;

    fn source(self) -> Result<Self::Records, Infallible> {
        Ok(self.drain(..).into())
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub struct ReaderSource<S>(reader::Reader<S>);

impl<S: Read> From<reader::Reader<S>> for ReaderSource<S> {
    fn from(reader: reader::Reader<S>) -> Self {
        Self(reader)
    }
}

impl<S: Read> Source for ReaderSource<S> {
    type Error = reader::Error;
    type Records = reader::RecordReader<S>;

    fn source(self) -> Result<Self::Records, Self::Error> {
        self.0.to_record_reader().map(|(_, records)| records)
    }
}

pub type BoxedRecords = Box<dyn Iterator<Item = Result<Record, GenericError>>>;

#[repr(transparent)]
struct BoxedRecordsInternals<I, E>(I)
where
    I: Iterator<Item = Result<Record, E>>;

impl<I, E> Iterator for BoxedRecordsInternals<I, E>
where
    I: Iterator<Item = Result<Record, E>>,
    E: std::error::Error + 'static,
{
    type Item = Result<Record, GenericError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|result| result.map_err(GenericError::new))
    }
}

#[repr(transparent)]
pub struct BoxedSource(Box<dyn FnOnce() -> Result<BoxedRecords, GenericError>>);

impl Source for BoxedSource {
    type Error = GenericError;
    type Records = BoxedRecords;

    fn source(self) -> Result<Self::Records, Self::Error> {
        (self.0)()
    }
}

pub fn boxed<S>(source: S) -> BoxedSource
where
    S: Source + 'static,
    S::Error: std::error::Error,
{
    BoxedSource(Box::new(|| match source.source() {
        Ok(s) => Ok(Box::new(BoxedRecordsInternals(s))),
        Err(e) => Err(GenericError::new(e)),
    }))
}

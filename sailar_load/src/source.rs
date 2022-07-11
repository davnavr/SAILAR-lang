//! Contains the trait used to read the contents of a module during loading.

use crate::module::Record;
use sailar::reader;
use std::convert::Infallible;
use std::io::Read;

pub trait Source {
    type Error;
    type Records: Iterator<Item = Result<Record, Self::Error>>;

    fn source(self) -> Result<Self::Records, Self::Error>;
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

pub type BoxedRecords = Box<dyn Iterator<Item = Result<Record, Box<dyn std::error::Error>>>>;

#[repr(transparent)]
struct BoxedRecordsInternals<I, E>(I)
where
    I: Iterator<Item = Result<Record, E>>;

impl<I, E> Iterator for BoxedRecordsInternals<I, E>
where
    I: Iterator<Item = Result<Record, E>>,
    E: std::error::Error + 'static,
{
    type Item = Result<Record, Box<dyn std::error::Error>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|result| result.map_err(Box::from))
    }
}

pub type BoxedSource = Box<dyn Source<Error = Box<dyn std::error::Error>, Records = BoxedRecords>>;

#[repr(transparent)]
struct BoxedSourceInternals<S>(S);

impl<S> Source for BoxedSourceInternals<S>
where
    S: Source + 'static,
    S::Error: std::error::Error + 'static,
{
    type Error = Box<dyn std::error::Error>;
    type Records = BoxedRecords;

    fn source(self) -> Result<Self::Records, Self::Error> {
        match self.0.source() {
            Ok(s) => Ok(Box::new(BoxedRecordsInternals(s))),
            Err(e) => Err(Box::from(e)),
        }
    }
}

pub fn boxed<S>(source: S) -> BoxedSource
where
    S: Source + 'static,
    S::Error: std::error::Error,
{
    Box::new(BoxedSourceInternals(source))
}

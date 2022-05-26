//! Module for interacting with SAILAR binary modules.

use crate::binary;
use crate::binary::record;
use crate::identifier::Identifier;

pub type Record = record::Record<'static>;

#[derive(Debug)]
pub struct Module {
    identifiers: Vec<Identifier>,
}

impl Module {
    pub(crate) fn from_source<S: crate::loader::Source>(source: S) -> Result<Box<Self>, S::Error> {
        let mut module = Box::new(Self {
            identifiers: Vec::default(),
        });

        source.iter_records(|record| todo!("record {:?}", record))?;

        Ok(module)
    }

    #[inline]
    pub fn identifiers(&self) -> &[Identifier] {
        &self.identifiers
    }
}

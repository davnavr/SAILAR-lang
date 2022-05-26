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
    #[inline]
    pub fn identifiers(&self) -> &[Identifier] {
        &self.identifiers
    }

    pub(crate) fn from_record_iter<R: std::iter::Iterator<Item = binary::reader::Result<Record>>>(records: R) -> Result<Self, ()> {
        todo!("process records")
    }
}

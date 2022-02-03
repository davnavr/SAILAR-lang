use crate::builder::counter;
use crate::format::{self, Identifier};
use std::collections::hash_map;

type Index = format::indices::Identifier;

pub struct Identifiers {
    index: counter::Cell<Index>,
    lookup: hash_map::HashMap<Identifier, Index>,
}

impl Identifiers {
    pub fn new() -> Self {
        Self {
            index: counter::Cell::new(),
            lookup: hash_map::HashMap::new(),
        }
    }

    pub fn insert_or_get(&mut self, identifier: Identifier) -> Index {
        match self.lookup.entry(identifier) {
            hash_map::Entry::Occupied(occupied) => *occupied.get(),
            hash_map::Entry::Vacant(vacant) => *vacant.insert(self.index.next()),
        }
    }
}

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

    pub(super) fn build(&mut self) -> Vec<Identifier> {
        let mut identifiers = Vec::new();
        identifiers.resize_with(
            self.lookup.len(),
            std::mem::MaybeUninit::<Identifier>::zeroed,
        );

        for (id, index) in self.lookup.drain() {
            identifiers[usize::try_from(index).unwrap()].write(id);
        }

        // All identifiers should be initialized, since there are no duplicate or skipped indices.
        unsafe { std::mem::transmute::<_, Vec<Identifier>>(identifiers) }
    }
}

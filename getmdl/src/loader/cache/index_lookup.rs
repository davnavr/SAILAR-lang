use registir::hashing::IntegerHashBuilder;
use std::cell::RefCell;
use std::collections::hash_map;

pub struct IndexLookup<'a, I, T> {
    lookup: RefCell<hash_map::HashMap<I, &'a T, IntegerHashBuilder>>,
    items: typed_arena::Arena<T>,
}

impl<'a, I, T> IndexLookup<'a, I, T> {
    pub fn new() -> Self {
        Self {
            lookup: RefCell::new(hash_map::HashMap::with_hasher(IntegerHashBuilder::default())),
            items: typed_arena::Arena::new(),
        }
    }
}

impl<I, T> std::default::Default for IndexLookup<'_, I, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, I, T> IndexLookup<'a, I, T>
where
    I: Copy + Eq + std::hash::Hash,
{
    pub fn insert_or_get<E, F: FnOnce(I) -> Result<T, E>>(
        &'a self,
        index: I,
        value: F,
    ) -> Result<&'a T, E> {
        match self.lookup.borrow_mut().entry(index) {
            hash_map::Entry::Occupied(occupied) => Ok(occupied.get()),
            hash_map::Entry::Vacant(vacant) => {
                value(index).map(|value: T| *vacant.insert(self.items.alloc(value)))
            }
        }
    }
}

impl<'a, I, T> std::fmt::Debug for &'a IndexLookup<'a, I, T>
where
    I: std::fmt::Debug,
    &'a T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut map = f.debug_map();
        for (key, value) in self.lookup.borrow().iter() {
            map.entry(key, value);
        }
        map.finish()
    }
}

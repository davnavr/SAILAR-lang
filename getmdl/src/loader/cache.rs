use super::{hash_map, RefCell, TypedArena};

pub(crate) struct IndexLookup<'a, I, T> {
    lookup: RefCell<hash_map::HashMap<I, &'a T>>,
    items: TypedArena<T>,
}

impl<'a, I, T> IndexLookup<'a, I, T>
where
    I: Copy + Eq + std::hash::Hash,
{
    pub(crate) fn new() -> Self {
        Self {
            lookup: RefCell::new(hash_map::HashMap::new()),
            items: TypedArena::new(),
        }
    }

    pub(crate) fn insert_or_get<E, F: FnOnce(I) -> Result<T, E>>(
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

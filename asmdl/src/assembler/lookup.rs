use crate::ast;
use registir::format::indices;
use std::{collections::hash_map, marker::PhantomData};

pub struct IndexedMap<'a, I, V> {
    values: Vec<V>,
    lookup: hash_map::HashMap<&'a ast::Identifier, usize>,
    phantom: PhantomData<I>,
}

impl<'a, I, V> IndexedMap<'a, I, V> {
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
            lookup: hash_map::HashMap::new(),
            phantom: PhantomData,
        }
    }

    pub fn values(&self) -> &Vec<V> {
        &self.values
    }

    pub fn into_vec(&mut self) -> Vec<V> {
        self.lookup.clear();
        std::mem::replace(&mut self.values, Vec::new())
    }

    pub fn clear(&mut self) {
        self.values.clear();
        self.lookup.clear()
    }
}

impl<'a, I, V> IndexedMap<'a, I, V>
where
    usize: TryInto<I>,
    <usize as TryInto<I>>::Error: std::fmt::Debug,
{
    pub fn insert(&mut self, key: &'a ast::Identifier, value: V) -> Result<I, &V> {
        let index = self.values.len();
        match self.lookup.insert(key, index) {
            None => {
                self.values.push(value);
                Ok(index.try_into().unwrap())
            }
            Some(existing_index) => Err(&self.values[existing_index]),
        }
    }
}

impl<'a, I, V> IndexedMap<'a, I, (&'a ast::Identifier, V)> {
    /// Removes the element associated with the specified identifier, and replaces it with the last value.
    pub fn swap_remove(&mut self, key: &'a ast::Identifier) -> Option<V> {
        match self.lookup.entry(key) {
            hash_map::Entry::Occupied(mut target) => {
                let target_index = *target.get();
                let last_index = self.values.len() - 1;
                let last_name = self.values[last_index].0;
                self.values.swap(target_index, last_index);

                // Target item is moved to last index, so lookup has to be adjusted.
                let previous_target_index = target.insert(last_index);
                target.remove();
                *self.lookup.get_mut(last_name).unwrap() = previous_target_index;

                Some(self.values.swap_remove(last_index).1)
            }
            hash_map::Entry::Vacant(_) => None,
        }
    }
}

pub struct IndexedSet<I, T> {
    lookup: hash_map::HashMap<T, usize>,
    phantom: PhantomData<I>,
}

impl<I, T> IndexedSet<I, T> {
    pub fn new() -> Self {
        Self {
            lookup: hash_map::HashMap::new(),
            phantom: PhantomData,
        }
    }
}

impl<I, T> IndexedSet<I, T>
where
    T: std::hash::Hash + Eq,
    usize: TryInto<I>,
    <usize as TryInto<I>>::Error: std::fmt::Debug,
{
    pub fn insert_or_get(&mut self, value: T) -> I {
        let index = self.lookup.len();
        self.lookup
            .insert(value, index)
            .unwrap_or(index)
            .try_into()
            .unwrap()
    }

    pub fn into_vec(&mut self) -> Vec<T> {
        let mut buffer = Vec::new();
        buffer.resize_with(self.lookup.len(), std::mem::MaybeUninit::<T>::zeroed);

        for (item, index) in self.lookup.drain() {
            buffer[index] = std::mem::MaybeUninit::new(item);
        }
        // MaybeUninit<T> and T have the same size, so reinterpreting the buffer should be safe.
        // Also, all elements should have been properly initialized.
        unsafe { std::mem::transmute(buffer) }
    }
}

/// Specialized map for associating identifiers with registers in code blocks.
pub struct RegisterMap<'a> {
    input_count: u32,
    temporary_count: u32,
    lookup: hash_map::HashMap<&'a ast::Identifier, (indices::Register, ast::Position)>,
}

impl<'a> RegisterMap<'a> {
    pub fn new() -> Self {
        Self {
            input_count: 0,
            temporary_count: 0,
            lookup: hash_map::HashMap::new(),
        }
    }

    pub fn clear(&mut self) {
        self.input_count = 0;
        self.temporary_count = 0;
        self.lookup.clear()
    }

    fn insert(
        &mut self,
        name: &'a ast::RegisterSymbol,
        is_input: bool,
    ) -> Result<indices::Register, ast::Position> {
        match self.lookup.entry(name.identifier()) {
            hash_map::Entry::Occupied(existing) => Err(existing.get().1.clone()),
            hash_map::Entry::Vacant(entry) => {
                let index;
                if is_input {
                    index =
                        indices::Register::Input(indices::InputRegister::from(self.input_count));
                    self.input_count += 1;
                } else {
                    index = indices::Register::Temporary(indices::TemporaryRegister::from(
                        self.temporary_count,
                    ));
                    self.input_count += 1;
                }

                entry.insert((index, name.location().clone()));
                Ok(index)
            }
        }
    }

    pub fn insert_input(
        &mut self,
        name: &'a ast::RegisterSymbol,
    ) -> Result<indices::Register, ast::Position> {
        self.insert(name, true)
    }

    pub fn insert_temporary(
        &mut self,
        name: &'a ast::RegisterSymbol,
    ) -> Result<indices::Register, ast::Position> {
        self.insert(name, false)
    }
}

#[cfg(test)]
mod tests {
    use crate::{assembler::lookup, ast};

    #[test]
    fn indexed_set_can_convert_to_vector() {
        let mut set = lookup::IndexedSet::<u32, String>::new();
        set.insert_or_get(String::from("this"));
        set.insert_or_get(String::from("is"));
        set.insert_or_get(String::from("a"));
        set.insert_or_get(String::from("test"));
        assert_eq!(
            set.into_vec(),
            vec![
                String::from("this"),
                String::from("is"),
                String::from("a"),
                String::from("test")
            ]
        );
    }

    #[test]
    fn indexed_map_swap_remove_is_correct() {
        let mut map = lookup::IndexedMap::<u32, (&'_ ast::Identifier, u32)>::new();
        let key_1 = ast::Identifier::try_from("foo").unwrap();
        map.insert(&key_1, (&key_1, 42)).unwrap();
        let key_2 = ast::Identifier::try_from("entry").unwrap();
        map.insert(&key_2, (&key_2, 2)).unwrap();
        let key_3 = ast::Identifier::try_from("bar").unwrap();
        map.insert(&key_3, (&key_3, 255)).unwrap();
        let key_4 = ast::Identifier::try_from("baz").unwrap();
        map.insert(&key_4, (&key_4, 0xFFFF)).unwrap();

        assert_eq!(Some(2), map.swap_remove(&key_2));
        let ignored_value = (&key_1, 9999);
        let mut get_value = |key| map.insert(key, ignored_value).map_err(|(_, value)| *value);
        assert_eq!(Err(42), get_value(&key_1));
        assert_eq!(Err(255), get_value(&key_3));
        assert_eq!(Err(0xFFFF), get_value(&key_4));
    }
}

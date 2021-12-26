use crate::ast;
use registir::format::indices;

#[derive(Copy, Debug)]
struct IndexCounter<I, V> {
    next_value: V,
    phantom: std::marker::PhantomData<I>,
}

impl<I, V: Clone> std::clone::Clone for IndexCounter<I, V> {
    fn clone(&self) -> Self {
        Self {
            next_value: self.next_value.clone(),
            phantom: std::marker::PhantomData,
        }
    }
}

impl<I, V: Default> IndexCounter<I, V> {
    fn new() -> Self {
        Self {
            next_value: V::default(),
            phantom: std::marker::PhantomData,
        }
    }

    fn reset(&mut self) {
        *self = Self::new();
    }
}

impl<I: From<V>, V: std::ops::AddAssign<u32> + Copy> IndexCounter<I, V> {
    fn with_next(mut self) -> (Self, I) {
        let index = I::from(self.next_value);
        self.next_value += 1u32;
        (self, index)
    }

    fn next(&mut self) -> I {
        let (incremented, index) = self.clone().with_next();
        *self = incremented;
        index
    }
}

#[derive(Debug)]
pub struct SymbolMap<'a, S, I, T> {
    values: Vec<T>,
    counter: IndexCounter<I, u32>,
    lookup: std::collections::HashMap<&'a ast::Identifier, I>,
    phantom: std::marker::PhantomData<&'a S>,
}

impl<'a, S, I, T> SymbolMap<'a, S, I, T> {
    pub fn items(&self) -> &[T] {
        &self.values
    }

    pub fn new() -> Self {
        Self {
            values: Vec::new(),
            counter: IndexCounter::new(),
            lookup: std::collections::HashMap::new(),
            phantom: std::marker::PhantomData,
        }
    }

    pub fn clear(&mut self) {
        self.values.clear();
        self.counter.reset();
        self.lookup.clear()
    }
}

impl<'a, S, I: From<u32> + Copy, T> SymbolMap<'a, S, I, T>
where
    &'a S: Into<&'a ast::Identifier>,
{
    pub fn try_add_with<F: FnOnce(I) -> T>(&mut self, symbol: &'a S, f: F) -> Option<I> {
        let (incremented, index) = self.counter.with_next();
        match self.lookup.insert(symbol.into(), index) {
            None => {
                self.values.push(f(index));
                self.counter = incremented;
                Some(index)
            }
            Some(_) => None,
        }
    }

    pub fn try_add(&mut self, symbol: &'a S, value: T) -> Option<I> {
        self.try_add_with(symbol, |_| value)
    }

    pub fn index_of(&self, symbol: &'a S) -> Option<I> {
        self.lookup.get(symbol.into()).copied()
    }
}

#[derive(Debug, Eq)]
struct SetKey<T> {
    items: std::rc::Rc<std::cell::RefCell<Vec<T>>>,
    index: usize,
}

impl<T> SetKey<T> {
    fn get(&self) -> std::cell::Ref<'_, T> {
        std::cell::Ref::map(self.items.borrow(), |items| items.get(self.index).unwrap())
    }
}

impl<T> std::cmp::PartialEq for SetKey<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
            && Vec::as_ptr(&self.items.borrow()) == Vec::as_ptr(&other.items.borrow())
    }
}

impl<T: std::hash::Hash> std::hash::Hash for SetKey<T> {
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        std::hash::Hash::hash(std::ops::Deref::deref(&self.get()), state)
    }
}

#[derive(Debug)]
pub struct Set<I, T> {
    items: std::rc::Rc<std::cell::RefCell<Vec<T>>>,
    lookup: std::collections::HashMap<SetKey<T>, I>,
}

impl<I: indices::SimpleIndex + Copy, T: Eq + std::hash::Hash> Set<I, T>
where
    <I as TryFrom<usize>>::Error: std::error::Error,
{
    pub fn new() -> Self {
        Self {
            items: std::rc::Rc::new(std::cell::RefCell::new(Vec::new())),
            lookup: std::collections::HashMap::new(),
        }
    }

    pub fn take_items(&mut self) -> Vec<T> {
        self.lookup.clear();
        self.items.replace(Vec::new())
    }

    pub fn add(&mut self, value: T) -> I {
        use std::collections::hash_map::Entry;

        let index = self.items.borrow().len();
        self.items.borrow_mut().push(value);
        match self.lookup.entry(SetKey {
            items: self.items.clone(),
            index,
        }) {
            Entry::Occupied(entry) => {
                self.items.borrow_mut().pop();
                *(entry.get())
            }
            Entry::Vacant(entry) => *entry.insert(I::try_from(index).unwrap()),
        }
    }
}

#[derive(Debug)]
pub struct RegisterLookup<'a> {
    input_counter: IndexCounter<indices::InputRegister, u32>,
    temporary_counter: IndexCounter<indices::TemporaryRegister, u32>,
    lookup: std::collections::HashMap<&'a ast::Identifier, indices::Register>,
}

impl<'a> RegisterLookup<'a> {
    pub fn new() -> Self {
        Self {
            input_counter: IndexCounter::new(),
            temporary_counter: IndexCounter::new(),
            lookup: std::collections::HashMap::new(),
        }
    }

    pub fn clear(&mut self) {
        self.input_counter.reset();
        self.temporary_counter.reset();
        self.lookup.clear()
    }

    fn add(
        &mut self,
        symbol: &'a ast::RegisterSymbol,
        is_input_register: bool,
    ) -> Option<indices::Register> {
        use std::collections::hash_map::Entry;

        match self.lookup.entry(&symbol.0.value) {
            Entry::Occupied(_) => None,
            Entry::Vacant(entry) => {
                if is_input_register {
                    Some(indices::Register::Input(self.input_counter.next()))
                } else {
                    Some(indices::Register::Temporary(self.temporary_counter.next()))
                }
            }
        }
    }

    pub fn add_input(&mut self, symbol: &'a ast::RegisterSymbol) -> Option<indices::Register> {
        self.add(symbol, true)
    }

    pub fn add_temporary(&mut self, symbol: &'a ast::RegisterSymbol) -> Option<indices::Register> {
        self.add(symbol, false)
    }

    pub fn get(&self, symbol: &'a ast::RegisterSymbol) -> Option<indices::Register> {
        self.lookup.get(&symbol.0.value).copied()
    }

    pub fn ignore_temporary(&mut self) {
        self.temporary_counter.next();
    }
}

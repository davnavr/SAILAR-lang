use crate::ast;
use registir::format::indices::SimpleIndex;

#[derive(Debug)]
pub struct SymbolMap<'a, S, I, T> {
    values: Vec<T>,
    lookup: std::collections::HashMap<&'a ast::Identifier, I>,
    phantom_1: std::marker::PhantomData<&'a S>,
    phantom_2: std::marker::PhantomData<I>,
}

impl<'a, S, I, T> SymbolMap<'a, S, I, T> {
    pub fn items(&self) -> &[T] {
        &self.values
    }

    pub fn new() -> Self {
        Self {
            values: Vec::new(),
            lookup: std::collections::HashMap::new(),
            phantom_1: std::marker::PhantomData,
            phantom_2: std::marker::PhantomData,
        }
    }
}

impl<'a, S, I: SimpleIndex + Copy, T> SymbolMap<'a, S, I, T>
where
    &'a S: Into<&'a ast::Identifier>,
    <I as TryFrom<usize>>::Error: std::error::Error,
{
    pub fn try_add_with<F: FnOnce(I) -> T>(&mut self, symbol: &'a S, f: F) -> Option<I> {
        let index = I::try_from(self.values.len()).unwrap();
        match self.lookup.insert(symbol.into(), index) {
            None => {
                self.values.push(f(index));
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

impl<I: SimpleIndex + Copy, T: Eq + std::hash::Hash> Set<I, T>
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

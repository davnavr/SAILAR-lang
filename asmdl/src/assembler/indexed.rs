use crate::ast;

#[derive(Debug)]
pub struct SymbolMap<'a, S, I, T> {
    values: Vec<T>,
    lookup: std::collections::HashMap<&'a ast::Identifier, I>,
    phantom_1: std::marker::PhantomData<&'a S>,
    phantom_2: std::marker::PhantomData<I>,
}

impl<'a, S, I, T> SymbolMap<'a, S, I, T> {
    pub fn items(&self) -> &Vec<T> {
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

impl<'a, S, I: TryFrom<usize> + Copy, T> SymbolMap<'a, S, I, T>
where
    &'a S: Into<&'a ast::Identifier>,
    <I as TryFrom<usize>>::Error: std::error::Error,
{
    pub fn try_add(&mut self, symbol: &'a S, value: T) -> Option<I> {
        let index = I::try_from(self.values.len()).unwrap();
        match self.lookup.insert(symbol.into(), index) {
            None => {
                self.values.push(value);
                Some(index)
            }
            Some(_) => None,
        }
    }
}

#[derive(Debug)]
pub struct Set<I, T> {
    values: std::collections::HashMap<T, I>,
}

impl<I: TryFrom<usize> + Copy, T: Eq + std::hash::Hash> Set<I, T>
where
    <I as TryFrom<usize>>::Error: std::error::Error,
{
    pub fn new() -> Self {
        Self {
            values: std::collections::HashMap::new(),
        }
    }

    pub fn add(&mut self, value: T) -> I {
        let index = I::try_from(self.values.len()).unwrap();
        match self.values.insert(value, index) {
            None => index,
            Some(existing) => existing,
        }
    }
}

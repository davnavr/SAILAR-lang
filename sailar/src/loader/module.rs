//! Module for interacting with SAILAR binary modules.

use crate::binary::record;
use crate::identifier::Identifier;
use crate::loader;
use std::sync::{Arc, Weak};

pub type Record = record::Record<'static>;

//pub struct ModuleIdentifierReference

#[derive(Debug)]
pub struct Module {
    loader: Weak<loader::State>,
    identifiers: Vec<Identifier>,
}

impl Module {
    pub(crate) fn from_source<S: loader::Source>(source: S) -> Result<Self, S::Error> {
        let mut module = Self {
            loader: Default::default(),
            identifiers: Vec::default(),
        };

        source.iter_records(|record| todo!("record {:?}", record))?;

        Ok(module)
    }

    pub(crate) fn set_loader(&mut self, loader: &Arc<loader::State>) {
        self.loader = Arc::downgrade(loader);
    }

    /// Gets a weak reference to the loader.
    /// 
    /// Since this reference is most likely to exist, consider using [`loader`] instead.
    pub fn loader_weak(&self) -> &Weak<loader::State> {
        &self.loader
    }

    /// Gets a reference to the loader.
    /// 
    /// # Panics
    /// 
    /// Panics if the loader was dropped. Code that uses the SAILAR loader should ensure that the loader outlives all loaded
    /// modules.
    pub fn loader(&self) -> Arc<loader::State> {
        match self.loader.upgrade() {
            Some(loader) => loader,
            None => panic!("loader was dropped"),
        }
    }

    #[inline]
    pub fn identifiers(&self) -> &[Identifier] {
        &self.identifiers
    }
}

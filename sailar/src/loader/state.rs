//! Module for managing loader state.

use crate::loader::{Module, ModuleIdentifier};
use std::collections::hash_map;
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, Mutex};

type ModuleLookup = rustc_hash::FxHashMap<ModuleIdentifier, Arc<Module>>;

#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ModuleLoadError<E> {
    #[error(transparent)]
    SourceError(E),
    #[error("a module corresponding to the name and version was already loaded")]
    DuplicateModule(ModuleIdentifier),
}

pub type ModuleLoadResult<E> = Result<Arc<Module>, ModuleLoadError<E>>;

pub struct State {
    modules: Mutex<Vec<Arc<Module>>>,
    module_lookup: Mutex<ModuleLookup>,
}

impl State {
    pub fn new() -> Arc<Self> {
        Arc::new(Self {
            modules: Default::default(),
            module_lookup: Default::default(),
        })
    }

    pub fn force_load_module<S: crate::loader::Source>(self: &Arc<Self>, source: S) -> ModuleLoadResult<S::Error> {
        let mut module = Module::from_source(source).map_err(ModuleLoadError::SourceError)?;
        module.set_loader(self);

        let allocated_module = Arc::new(module);
        if !allocated_module.is_anonymous() {
            match self.module_lookup.lock().unwrap().entry(allocated_module.module_identifier()) {
                hash_map::Entry::Occupied(existing) => return Err(ModuleLoadError::DuplicateModule(existing.key().clone())),
                hash_map::Entry::Vacant(vacant) => {
                    vacant.insert(allocated_module.clone());
                }
            }
        }

        self.modules.lock().unwrap().push(allocated_module.clone());
        Ok(allocated_module)
    }
}

impl Debug for State {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("State").field("modules", &self.modules).finish()
    }
}

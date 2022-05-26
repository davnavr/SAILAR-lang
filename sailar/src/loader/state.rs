//! Module for managing loader state.

use crate::loader::Module;
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, Mutex};

//type ModuleLookup<'s> = elsa::FrozenMap<ModuleIdentifierReference, Arc<Module>, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;

#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ModuleLoadError<E> {
    #[error(transparent)]
    SourceError(E),
}

pub type ModuleLoadResult<E> = Result<Arc<Module>, ModuleLoadError<E>>;

pub struct State {
    modules: Mutex<Vec<Arc<Module>>>,
    //module_lookup: Mutex<ModuleLookup>,
}

impl State {
    pub fn new() -> Arc<Self> {
        Arc::new(Self {
            modules: Default::default(),
            //module_lookup: Default::default(),
        })
    }

    pub fn force_load_module<S: crate::loader::Source>(self: &Arc<Self>, source: S) -> ModuleLoadResult<S::Error> {
        let mut module = Module::from_source(source).map_err(ModuleLoadError::SourceError)?;

        // TODO: Check module lookup to see if module with same name is already loaded

        module.set_loader(self);
        let allocated_module = Arc::new(module);
        self.modules.lock().unwrap().push(allocated_module.clone());
        Ok(allocated_module)
    }
}

impl Debug for State {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("State").field("modules", &self.modules).finish()
    }
}

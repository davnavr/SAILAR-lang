//! Module for managing loader state.

use crate::identifier;
use crate::loader::Module;
use std::fmt::{Debug, Formatter};
use std::sync::Mutex;

type ModuleLookup<'s> = elsa::FrozenMap<&'s identifier::Id, &'s Module, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;

#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ModuleLoadError<E> {
    #[error(transparent)]
    SourceError(E),
}

pub type ModuleLoadResult<'state, E> = Result<&'state Module, ModuleLoadError<E>>;

pub struct State<'state> {
    modules: Mutex<elsa::FrozenVec<Box<Module>>>,
    module_lookup: Mutex<ModuleLookup<'state>>,
}

impl<'state> State<'state> {
    pub fn force_load_module<S: crate::loader::Source>(&self, source: S) -> ModuleLoadResult<'state, S::Error> {
        let module = Module::from_source(source).map_err(ModuleLoadError::SourceError)?;

        // TODO: Check module lookup to see if module with same name is already loaded

        let mut module_list = self.modules.lock().unwrap();
        Ok(module_list.push_get(module))
    }
}

struct ModuleDebug<'s>(&'s Mutex<elsa::FrozenVec<Box<Module>>>);

impl Debug for ModuleDebug<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self.0.try_lock() {
            Ok(fields) => f.debug_list().entries(fields.iter()).finish(),
            Err(error) => Debug::fmt(&error, f),
        }
    }
}

impl Debug for State<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("State")
            .field("modules", &ModuleDebug(&self.modules))
            .finish()
    }
}

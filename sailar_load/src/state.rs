//! Module for managing loader state.

use crate::module;
use crate::resolver::{self, Resolver};
use crate::source;
use std::collections::hash_map;
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, Mutex};

type ModuleLookup = rustc_hash::FxHashMap<Arc<module::ModuleIdentifier>, Arc<module::Module>>;

#[derive(Debug, Default)]
struct ModuleArena {
    named_modules: ModuleLookup,
    anonymous_modules: Vec<Arc<module::Module>>,
}

pub struct State {
    // Each individual module will cache its imported modules, so accessing this lookup should rarely happen
    modules: Mutex<ModuleArena>,
    resolver: Mutex<resolver::BoxedResolver>,
}

impl State {
    pub fn with_resolver<R>(resolver: R) -> Arc<Self>
    where
        R: Resolver + Send + 'static,
        R::Error: std::error::Error,
    {
        Arc::new(Self {
            modules: Default::default(),
            resolver: Mutex::new(resolver::boxed(resolver)),
        })
    }

    /// Creates a new [`State`] with no loaded modules and no import resolver. New modules can only be loaded by calling
    /// [`force_load_module`].
    ///
    /// [`force_load_module`]: State::force_load_module
    #[inline]
    pub fn new() -> Arc<Self> {
        Self::with_resolver(resolver::unsuccessful())
    }

    pub fn force_load_module<S: crate::Source>(self: &Arc<Self>, source: S) -> Result<Option<Arc<module::Module>>, S::Error> {
        let module = module::Module::from_source(source, Arc::downgrade(self))?;
        let mut arena = self.modules.lock().unwrap();
        if let Some(id) = module.module_identifier() {
            match arena.named_modules.entry(id.clone()) {
                hash_map::Entry::Occupied(_) => return Ok(None),
                hash_map::Entry::Vacant(vacant) => {
                    vacant.insert(module.clone());
                }
            }
        } else {
            arena.anonymous_modules.push(module.clone());
        }

        Ok(Some(module))
    }

    /// Gets the loaded module corresponding to the identifier or loads a module using the import resolver.
    pub fn get_or_load_module(
        self: &Arc<Self>,
        module_identifier: Arc<module::ModuleIdentifier>,
    ) -> Result<Option<Arc<module::Module>>, resolver::Error<Box<dyn std::error::Error + 'static>>> {
        let mut arena = self.modules.lock().unwrap();
        match arena.named_modules.entry(module_identifier) {
            hash_map::Entry::Occupied(occupied) => Ok(Some(occupied.get().clone())),
            hash_map::Entry::Vacant(vacant) => match self.resolver.lock().unwrap().load_from_identifier(vacant.key()) {
                Ok(None) => Ok(None),
                Err(e) => Err(resolver::Error::RetrievalError(e)),
                Ok(Some(source)) => {
                    let module =
                        module::Module::from_source(source::ReaderSource::from(source), Arc::downgrade(self)).map_err(|e| {
                            let a: Box<dyn std::error::Error + 'static> = Box::from(e);
                            resolver::Error::RetrievalError(a)
                        })?;

                    // TODO: Return an error on identifier mismatch.
                    assert!(Some(vacant.key()) == module.module_identifier());
                    Ok(Some(module))
                }
            },
        }
    }
}

impl Debug for State {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("State").field("modules", &self.modules).finish()
    }
}

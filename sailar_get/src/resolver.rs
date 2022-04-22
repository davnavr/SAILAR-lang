//! Contains code for analyzing modules and retrieving definitions from references.

use sailar::module::ModuleIdentifier;
use std::collections::hash_map;
use std::fmt::{Debug, Display, Formatter};
use std::sync::{Arc, Mutex};

/// The size of a pointer, in bytes.
pub type PointerSize = std::num::NonZeroU8;

pub const HOST_POINTER_SIZE: PointerSize = unsafe { PointerSize::new_unchecked(std::mem::size_of::<usize>() as u8) };

#[derive(Clone, Debug)]
pub struct ModuleAlreadyLoadedError {
    identifier: Arc<ModuleIdentifier>,
}

impl ModuleAlreadyLoadedError {
    #[inline]
    pub fn identifier(&self) -> &Arc<ModuleIdentifier> {
        &self.identifier
    }
}

impl Display for ModuleAlreadyLoadedError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "the module \"{}\", version {:?} was already loaded",
            self.identifier.name(),
            self.identifier.version()
        )
    }
}

impl std::error::Error for ModuleAlreadyLoadedError {}

type ModuleLookup = rustc_hash::FxHashMap<Arc<sailar::module::ModuleIdentifier>, Arc<crate::ResolvedModule>>;

#[derive(Debug)]
struct State {
    modules: ModuleLookup,
}

#[derive(Debug)]
pub struct Resolver {
    pointer_size: PointerSize,
    //loader: Loader,
    state: Mutex<State>,
}

impl Resolver {
    pub fn new(pointer_size: PointerSize) -> Self {
        Self {
            pointer_size,
            state: Mutex::new(State {
                modules: Default::default(),
            }),
        }
    }

    #[inline]
    pub fn pointer_size(&self) -> PointerSize {
        self.pointer_size
    }

    pub fn force_load_module(
        &self,
        module: sailar::ModuleDefinition,
    ) -> Result<Arc<crate::ResolvedModule>, ModuleAlreadyLoadedError> {
        match self.state.lock().unwrap().modules.entry(module.identifier().clone()) {
            hash_map::Entry::Vacant(vacant) => Ok(vacant.insert(crate::ResolvedModule::from_definition(module)).clone()),
            hash_map::Entry::Occupied(occupied) => Err(ModuleAlreadyLoadedError {
                identifier: occupied.get().identifier().clone(),
            }),
        }
    }
}

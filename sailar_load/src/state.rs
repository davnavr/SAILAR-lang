//! Module for managing loader state.

use crate::module;
use sailar::validation::ValidModule;
use std::sync::{Arc, Mutex};

/// Indicates the size of pointer addresses.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct AddressSize(std::num::NonZeroU16);

impl AddressSize {
    pub const fn with_byte_size(size: std::num::NonZeroU16) -> Self {
        Self(size)
    }

    pub const fn byte_size(self) -> std::num::NonZeroU16 {
        self.0
    }

    pub const fn bit_size(self) -> std::num::NonZeroU32 {
        unsafe {
            // Safety: Address size is guaranteed to never be zero.
            std::num::NonZeroU32::new_unchecked((self.0.get() as u32) * 8)
        }
    }

    pub const NATIVE: Self = unsafe {
        // Safety: Size of pointers in Rust is assumed to never be zero.
        Self::with_byte_size(std::num::NonZeroU16::new_unchecked(std::mem::size_of::<usize>() as u16))
    };
}

impl Default for AddressSize {
    fn default() -> Self {
        Self::NATIVE
    }
}

/// Used to configure the properties of the loader [`State`].
#[derive(Debug)]
pub struct Configuration {
    address_size: AddressSize,
}

impl Configuration {
    /// Generates the default loader configuration using the native pointer address size.
    pub fn new() -> Self {
        Self {
            address_size: AddressSize::NATIVE,
        }
    }

    /// Sets the byte size used for pointer addresses.
    pub fn address_size(mut self, size: AddressSize) -> Self {
        self.address_size = size;
        self
    }

    pub fn create_state(self) -> Arc<State> {
        Arc::new(State {
            address_size: self.address_size,
            modules: Default::default(),
        })
    }
}

impl Default for Configuration {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Default)]
struct Modules {
    lookup: rustc_hash::FxHashMap<module::ModuleIdentifier, usize>,
    modules: Vec<Arc<module::Module>>,
}

#[derive(Debug)]
pub struct State {
    // Each individual module will cache its imported modules, so accessing this lookup should rarely happen
    modules: Mutex<Modules>,
    address_size: AddressSize,
}

impl State {
    /// Loads a module. If a module corresponding to the same identifiers are already loaded, returns `Err`; otherwise, returns `Ok`.
    pub fn load_module(self: &Arc<Self>, module: ValidModule<'static>) -> Result<Arc<module::Module>, Arc<module::Module>> {
        let mut arena = self.modules.lock().unwrap();
        let module_names = module.contents().module_identifiers.clone();

        for name in module_names.iter() {
            if let Some(existing) = arena.lookup.get(name) {
                return Err(arena.modules[*existing].clone());
            }
        }

        let loaded = module::Module::from_contents(module.into_contents(), Arc::downgrade(self));
        let arena_index = arena.modules.len();

        for name in module_names.into_iter() {
            match arena.lookup.insert(name, arena_index) {
                None => (),
                Some(_) => unreachable!(),
            }
        }

        arena.modules.push(loaded.clone());

        Ok(loaded)
    }

    /// Gets the loaded module corresponding to the identifier.
    pub fn get_module(self: &Arc<Self>, identifier: &module::ModuleIdentifier) -> Option<Arc<module::Module>> {
        let modules = self.modules.lock().unwrap();
        modules.lookup.get(identifier).map(|index| modules.modules[*index].clone())
    }

    /// Gets the size of pointer addresses for all modules loaded by this [`State`].
    pub fn address_size(&self) -> AddressSize {
        self.address_size
    }
}

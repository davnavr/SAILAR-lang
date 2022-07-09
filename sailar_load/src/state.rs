//! Module for managing loader state.

use crate::module;
use crate::resolver::{self, Resolver};
use crate::source;
use std::collections::hash_map;
use std::fmt::{Debug, Formatter};
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
    address_size: AddressSize,
}

pub struct Builder {
    resolver: Option<resolver::BoxedResolver>,
    address_size: AddressSize,
}

impl Builder {
    /// The default loader configuration using the native pointer address size. New modules can
    /// only be loaded by calling [`force_load_module`].
    ///
    /// [`force_load_module`]: State::force_load_module
    pub fn new() -> Self {
        Self {
            resolver: None,
            address_size: AddressSize::NATIVE,
        }
    }

    /// Sets the resolver used to retrieve the modules corresponding to module imports.
    pub fn resolver<R>(self, resolver: R) -> Self
    where
        R: Resolver + Send + 'static,
        R::Error: std::error::Error,
    {
        Self {
            resolver: Some(resolver::boxed(resolver)),
            ..self
        }
    }

    /// Sets the byte size used for pointer addresses.
    pub fn address_size(self, size: AddressSize) -> Self {
        Self {
            address_size: size,
            ..self
        }
    }

    pub fn create(self) -> Arc<State> {
        Arc::new(State {
            modules: Default::default(),
            resolver: Mutex::new(self.resolver.unwrap_or_else(|| resolver::boxed(resolver::unsuccessful()))),
            address_size: self.address_size,
        })
    }
}

impl Default for Builder {
    fn default() -> Self {
        Self::new()
    }
}

impl State {
    /// Loads a module, attempting to associate its name and version (if present) with the loaded module. Returns `Err` if a
    /// module corresponding to the same name is already loaded.
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

    /// Gets the size of pointer addresses for all modules loaded by this [`State`].
    pub fn address_size(&self) -> AddressSize {
        self.address_size
    }
}

impl Debug for State {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("State").field("modules", &self.modules).finish()
    }
}

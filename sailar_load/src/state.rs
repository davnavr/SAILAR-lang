//! Module for managing loader state.

use crate::resolver::{self, Resolver};
use std::collections::hash_map;
use std::fmt::{Debug, Formatter};
use std::sync::Mutex;

#[derive(Debug, Default)]
struct ModuleArena<'s> {
    modules: Vec<Box<crate::Module<'s>>>,
    lookup: rustc_hash::FxHashMap<crate::ModuleIdentifier, &'s crate::Module<'s>>,
}

unsafe fn push_get_module_unchecked<'s>(
    modules: &mut Vec<Box<crate::Module<'s>>>,
    module: Box<crate::Module<'s>>,
) -> &'s crate::Module<'s> {
    let module_ptr = std::borrow::Borrow::borrow(&module) as *const crate::Module<'s>;
    modules.push(module);
    &*module_ptr
}

pub struct State<'s> {
    // Each individual module will cache its imported modules, so accessing this lookup should rarely happen
    module_lookup: Mutex<ModuleArena<'s>>,
    resolver: Mutex<resolver::BoxedResolver<'s>>,
}

impl<'s> State<'s> {
    pub fn with_resolver<R>(resolver: R) -> Self
    where
        R: Resolver<'s> + Send + 's,
        R::Error: std::error::Error + 's,
    {
        Self {
            module_lookup: Default::default(),
            resolver: Mutex::new(resolver::boxed(resolver)),
        }
    }

    /// Creates a new [`State`] with no loaded modules and no import resolver. New modules can only be loaded by calling
    /// [`force_load_modules`].
    #[inline]
    pub fn new() -> Self {
        Self::with_resolver(resolver::unsuccessful())
    }

    fn with_module_arena<R>(&'s self, action: impl FnOnce(&mut ModuleArena<'s>) -> R) -> R {
        let mut module_lookup_guard = self.module_lookup.lock().unwrap();
        // Workaround since splitting borrow with Mutex does not work
        let mut module_lookup = std::mem::take::<ModuleArena<'s>>(&mut module_lookup_guard);
        let result = action(&mut module_lookup);
        *module_lookup_guard = module_lookup;
        result
    }

    pub fn force_load_module<S: crate::Source>(&'s self, source: S) -> Result<Option<&'s crate::Module<'s>>, S::Error> {
        let mut module_identifier = None;
        let mut module = crate::Module::from_source(source, self, &mut module_identifier)?;
        self.with_module_arena(|module_lookup| {
            let module_ref = if let Some(id) = module_identifier {
                match module_lookup.lookup.entry(id) {
                    hash_map::Entry::Occupied(_) => return Ok(None),
                    hash_map::Entry::Vacant(vacant) => unsafe {
                        let id_ptr = vacant.key() as *const crate::ModuleIdentifier;
                        module.module_identifier = Some(&*id_ptr);
                        let module_ref = push_get_module_unchecked(&mut module_lookup.modules, module);
                        vacant.insert(module_ref)
                    },
                }
            } else {
                unsafe { push_get_module_unchecked(&mut module_lookup.modules, module) }
            };

            Ok(Some(module_ref))
        })
    }

    /// Gets the loaded module corresponding to the identifier or loads a module using the import resolver.
    pub fn get_or_load_module(
        &'s self,
        module_identifier: crate::ModuleIdentifier,
    ) -> Result<Option<&'s crate::Module<'s>>, resolver::Error<Box<dyn std::error::Error + 's>>> {
        self.with_module_arena(|module_lookup| {
            match module_lookup.lookup.entry(module_identifier) {
                hash_map::Entry::Occupied(occupied) => Ok(Some(*occupied.get())),
                hash_map::Entry::Vacant(vacant) => match self.resolver.lock().unwrap().load_from_identifier(vacant.key()) {
                    Ok(None) => Ok(None),
                    Err(e) => Err(resolver::Error::RetrievalError(e)),
                    Ok(Some(source)) => {
                        let mut module_identifier = None;
                        let mut module =
                            crate::Module::from_source(crate::source::ReaderSource::from(source), self, &mut module_identifier)
                                .map_err(|e| {
                                let a: Box<dyn std::error::Error + 's> = Box::from(e);
                                resolver::Error::RetrievalError(a)
                            })?;

                        // TODO: Avoid code duplication with force_load_module for vacant insert case.
                        let id_ptr = vacant.key() as *const crate::ModuleIdentifier;
                        module.module_identifier = Some(unsafe { &*id_ptr });
                        let module_ref = unsafe { push_get_module_unchecked(&mut module_lookup.modules, module) };
                        Ok(Some(*vacant.insert(module_ref)))
                    }
                },
            }
        })
    }
}

impl Debug for State<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("State").field("modules", &self.module_lookup).finish()
    }
}

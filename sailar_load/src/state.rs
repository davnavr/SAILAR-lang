//! Module for managing loader state.

use std::collections::hash_map;
use std::fmt::{Debug, Formatter};
use std::sync::Mutex;

#[derive(Debug, Default)]
struct ModuleArena<'s> {
    modules: Vec<Box<crate::Module<'s>>>,
    lookup: rustc_hash::FxHashMap<crate::ModuleIdentifier, &'s crate::Module<'s>>,
}

pub struct State<'s> {
    // Each individual module will cache its imported modules, so accessing this lookup should rarely happen
    module_lookup: Mutex<ModuleArena<'s>>,
}

impl<'s> State<'s> {
    pub fn new() -> Self {
        Self {
            module_lookup: Default::default(),
        }
    }

    pub fn force_load_module<S: crate::Source>(&'s self, source: S) -> Result<Option<&'s crate::Module<'s>>, S::Error> {
        let mut module_identifier = None;
        let mut module = crate::Module::from_source(source, self, &mut module_identifier)?;
        let mut module_lookup_guard = self.module_lookup.lock().unwrap();
        let mut module_lookup = std::mem::take::<ModuleArena<'s>>(&mut module_lookup_guard);

        unsafe fn push_get_module_unchecked<'s>(
            modules: &mut Vec<Box<crate::Module<'s>>>,
            module: Box<crate::Module<'s>>,
        ) -> &'s crate::Module<'s> {
            let module_ptr = std::borrow::Borrow::borrow(&module) as *const crate::Module<'s>;
            modules.push(module);
            &*module_ptr
        }

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

        *module_lookup_guard = module_lookup;

        Ok(Some(module_ref))
    }
}

impl Debug for State<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("State").field("modules", &self.module_lookup).finish()
    }
}

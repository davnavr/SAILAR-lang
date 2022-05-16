//! Code for retrieving module definitions from module names and versions.

use sailar::{module::ModuleIdentifier, ModuleDefinition};

pub trait ModuleLoader {
    fn load(&mut self, identifier: &ModuleIdentifier) -> Option<ModuleDefinition>;
}

impl<F: FnMut(&sailar::module::ModuleIdentifier) -> Option<ModuleDefinition>> ModuleLoader for F {
    fn load(&mut self, identifier: &ModuleIdentifier) -> Option<ModuleDefinition> {
        (self)(identifier)
    }
}

impl ModuleLoader for () {
    #[inline]
    fn load(&mut self, _: &ModuleIdentifier) -> Option<ModuleDefinition> {
        None
    }
}

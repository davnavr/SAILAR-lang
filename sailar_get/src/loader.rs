//! Code for retrieving module definitions from module names and versions.

pub trait ModuleLoader {
    fn load(identifier: &sailar::module::ModuleIdentifier) -> Option<sailar::ModuleDefinition>;
}

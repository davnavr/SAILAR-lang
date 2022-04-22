//! The [`ResolvedModule`] type wraps a [`sailar::ModuleDefinition`] to allow easy resolution of imports.

use std::sync::Arc;

#[derive(Debug)]
pub struct ResolvedModule {
    definition: sailar::ModuleDefinition,
}

impl ResolvedModule {
    pub(crate) fn from_definition(definition: sailar::ModuleDefinition) -> Arc<Self> {
        Arc::new(Self { definition })
    }

    #[inline]
    pub fn identifier(&self) -> &Arc<sailar::module::ModuleIdentifier> {
        self.definition.identifier()
    }

    #[inline]
    pub fn definition(&self) -> &sailar::ModuleDefinition {
        &self.definition
    }
}

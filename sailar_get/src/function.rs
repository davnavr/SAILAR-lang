//! Types for loading functions.

use std::sync::Arc;

#[derive(Debug)]
pub struct ResolvedFunction {
    function: Arc<sailar::module::DefinedFunction>,
}

impl ResolvedFunction {
    pub(crate) fn new(function: Arc<sailar::module::DefinedFunction>) -> Arc<Self> {
        Arc::new(Self { function })
    }

    #[inline]
    pub fn definition(&self) -> &Arc<sailar::module::DefinedFunction> {
        &self.function
    }
}

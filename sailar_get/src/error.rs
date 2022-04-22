//! Contains types that describes errors that can occur during import resolution.

use std::fmt::{Debug, Display, Formatter};
use std::sync::Arc;

/// Error used when a definition corresponding to a symbol could not be found.
#[derive(Clone)]
pub struct SymbolNotFoundError {
    symbol: sailar::Identifier,
    module: Arc<crate::ResolvedModule>,
}

impl SymbolNotFoundError {
    pub(crate) fn new(symbol: sailar::Identifier, module: Arc<crate::ResolvedModule>) -> Self {
        Self { symbol, module }
    }

    #[inline]
    pub fn module(&self) -> &Arc<crate::ResolvedModule> {
        &self.module
    }

    #[inline]
    pub fn symbol(&self) -> &sailar::Id {
        self.symbol.as_id()
    }
}

impl Debug for SymbolNotFoundError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("SymbolNotFoundError")
            .field("symbol", &self.symbol)
            .field("module", self.module.identifier())
            .finish()
    }
}

impl Display for SymbolNotFoundError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "a definition corresponding to the symbol \"{}\" could not be found in the module \"{}\"",
            self.symbol,
            self.module.identifier().name()
        )
    }
}

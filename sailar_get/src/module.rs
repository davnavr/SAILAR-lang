//! The [`ResolvedModule`] type wraps a [`sailar::ModuleDefinition`] to allow easy resolution of imports.

use crate::error::SymbolNotFoundError;
use std::sync::{Arc, Mutex};

#[derive(Debug)]
pub enum ResolvedDefinition {
    Function(crate::ResolvedFunction),
}

type SymbolLookup = rustc_hash::FxHashMap<sailar::module::DefinedSymbol, Arc<ResolvedDefinition>>;

#[derive(Debug)]
struct State {
    symbols: SymbolLookup,
}

#[derive(Debug)]
pub struct ResolvedModule {
    definition: sailar::ModuleDefinition,
    state: Mutex<State>,
}

impl ResolvedModule {
    pub(crate) fn from_definition(definition: sailar::ModuleDefinition) -> Arc<Self> {
        Arc::new(Self {
            definition,
            state: Mutex::new(State {
                symbols: Default::default(),
            }),
        })
    }

    #[inline]
    pub fn identifier(&self) -> &Arc<sailar::module::ModuleIdentifier> {
        self.definition.identifier()
    }

    #[inline]
    pub fn definition(&self) -> &sailar::ModuleDefinition {
        &self.definition
    }

    pub fn get_definition(&self) -> Result<ResolvedDefinition, SymbolNotFoundError> {
        todo!("search")
    }
}

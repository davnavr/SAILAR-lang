//! The [`ResolvedModule`] type wraps a [`sailar::ModuleDefinition`] to allow easy resolution of imports.

use crate::error::SymbolNotFoundError;
use std::collections::hash_map;
use std::sync::{Arc, Mutex};

#[derive(Debug)]
pub enum ResolvedDefinition {
    Function(Arc<crate::ResolvedFunction>),
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

    pub fn get_definition(self: &Arc<Self>, symbol: &sailar::Id) -> Result<Arc<ResolvedDefinition>, SymbolNotFoundError> {
        match self.definition().get_defined_symbol(symbol) {
            Some(definition) => Ok(self
                .state
                .lock()
                .unwrap()
                .symbols
                .entry(definition.clone())
                .or_insert_with_key(|definition| {
                    Arc::new(match definition {
                        sailar::module::DefinedSymbol::Function(function) => {
                            ResolvedDefinition::Function(crate::ResolvedFunction::new(function.clone()))
                        }
                    })
                })
                .clone()),
            None => Err(SymbolNotFoundError::new(symbol.to_identifier(), self.clone())),
        }
    }
}

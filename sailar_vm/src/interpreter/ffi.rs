//! Contains code for handling calls to internal and external functions made by the interpreter.

use crate::interpreter;
use sailar::format::Identifier;
use std::borrow::Borrow as _;
use std::collections::hash_map;

pub trait Handler {
    fn call<'l>(
        &self,
        inputs: &[interpreter::Register],
    ) -> Result<Vec<interpreter::Register>, Box<dyn std::error::Error>>;
}

#[derive(thiserror::Error, Debug)]
#[error("no handler defined for {symbol} in {library}")]
pub struct MissingHandlerError {
    library: Identifier,
    symbol: Identifier,
}

impl MissingHandlerError {
    pub fn library(&self) -> &Identifier {
        &self.library
    }

    pub fn symbol(&self) -> &Identifier {
        &self.symbol
    }
}

pub struct Library {
    handlers: hash_map::HashMap<Identifier, Box<dyn Handler>>,
}

impl Library {
    pub fn add_handler(
        &mut self,
        symbol: Identifier,
        handler: Box<dyn Handler>,
    ) -> Result<&dyn Handler, Identifier> {
        match self.handlers.entry(symbol) {
            hash_map::Entry::Occupied(occupied) => Err(occupied.key().clone()),
            hash_map::Entry::Vacant(vacant) => Ok(Box::borrow(vacant.insert(handler))),
        }
    }
}

pub struct HandlerLookup {
    lookup: hash_map::HashMap<Identifier, Library>,
}

impl HandlerLookup {
    pub fn add_library_handler(&mut self, name: Identifier) -> Result<&mut Library, Identifier> {
        match self.lookup.entry(name) {
            hash_map::Entry::Occupied(occupied) => Err(occupied.key().clone()),
            hash_map::Entry::Vacant(vacant) => Ok(vacant.insert(Library {
                handlers: hash_map::HashMap::new(),
            })),
        }
    }

    pub fn get(
        &mut self,
        library: &Identifier,
        symbol: &Identifier,
    ) -> Result<Option<&dyn Handler>, MissingHandlerError> {
        match self.lookup.get(library) {
            Some(library_handler) => library_handler
                .handlers
                .get(symbol)
                .map(|handler| Some(Box::borrow(handler)))
                .ok_or_else(|| MissingHandlerError {
                    library: library.clone(),
                    symbol: symbol.clone(),
                }),
            None => Ok(None),
        }
    }
}

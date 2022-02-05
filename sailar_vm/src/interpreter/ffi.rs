//! Contains code for handling calls to internal and external functions made by the interpreter.

use crate::interpreter;
use sailar::format::Identifier;
use std::borrow::Borrow as _;
use std::collections::hash_map;

pub type HandlerResult = Result<Vec<interpreter::Register>, Box<dyn std::error::Error>>;

pub trait Handler {
    fn call(&self, inputs: &[interpreter::Register]) -> HandlerResult;
}

impl<F: Fn(&[interpreter::Register]) -> HandlerResult> Handler for F {
    fn call(&self, inputs: &[interpreter::Register]) -> HandlerResult {
        (self)(inputs)
    }
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

impl Default for HandlerLookup {
    fn default() -> Self {
        let mut internal_handler = hash_map::HashMap::<_, Box<dyn Handler>>::new();

        internal_handler.insert(
            Identifier::try_from("print").unwrap(),
            Box::new(|inputs: &[interpreter::Register]| {
                use std::io::Write as _;

                let address = match &inputs[0] {
                    interpreter::Register::Pointer(pointer) => pointer.address(),
                    bad => todo!("invalid address type {:?}", bad),
                };

                let count = usize::try_from(&inputs[1])?;

                // Safety is decided by the calling interpreted code, so the following may not always be valid.
                let message = unsafe { std::slice::from_raw_parts(address, count) };

                let mut out = std::io::stdout();
                out.write_all(message)?;
                out.flush()?;
                Ok(Vec::new())
            }),
        );

        Self {
            lookup: {
                let mut lookup = hash_map::HashMap::new();
                lookup.insert(
                    Identifier::try_from("saili").unwrap(),
                    Library {
                        handlers: internal_handler,
                    },
                );
                lookup
            },
        }
    }
}

//! Module for interacting with SAILAR function definitions and instantiations.

use crate::binary::record;
use crate::helper::borrow::CowBox;
use crate::loader;
use std::sync::{Arc, Weak};

#[derive(Debug)]
pub struct Instantiation;

type DefinitionRecord = CowBox<'static, record::FunctionDefinition<'static>>;

#[derive(Debug)]
pub struct Definition {
    definition: DefinitionRecord,
    module: Weak<loader::Module>,
}

impl Definition {
    pub(crate) fn new(definition: DefinitionRecord, module: Weak<loader::Module>) -> Arc<Self> {
        Arc::new(Self { definition, module })
    }
}

#[derive(Clone)]
pub struct Symbol(Arc<Definition>);

impl Symbol {
    pub fn as_ref(&self) -> Option<&crate::Id> {
        todo!("if something is marked as export (public), make a symbol mandatory, but allow symbol to be omitted if somethng is marked as private")
    }
}

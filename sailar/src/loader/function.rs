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

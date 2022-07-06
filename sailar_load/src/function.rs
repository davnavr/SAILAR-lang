//! Module for interacting with SAILAR function definitions and instantiations.

use crate::binary::record;
use crate::helper::borrow::CowBox;
use crate::identifier::Id;
use crate::loader;
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, Weak};

#[derive(Clone, Debug)]
pub enum Template {
    Definition(Arc<Definition>),
    //Import()
}

impl Template {
    pub fn as_definition(&self) -> Result<&Arc<Definition>, std::convert::Infallible> {
        match self {
            Self::Definition(definition) => Ok(definition),
        }
    }
}

type InstantiationRecord = CowBox<'static, record::FunctionInstantiation>;

#[derive(Debug)]
pub struct Instantiation {
    instantiation: InstantiationRecord,
    //template: Mutex<Template>,
    module: Weak<loader::Module>,
}

impl Instantiation {
    pub(crate) fn new(instantiation: InstantiationRecord, module: Weak<loader::Module>) -> Arc<Self> {
        Arc::new(Self { instantiation, module })
    }
}

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

    // TODO: if something is marked as export (public), make a symbol mandatory, but allow symbol to be omitted if somethng is marked as private
    pub fn symbol_ref(&self) -> Option<&Id> {
        Some(self.definition.symbol())
    }

    pub fn symbol_shared(self: &Arc<Self>) -> Symbol {
        Symbol(self.clone())
    }
}

#[derive(Clone)]
#[repr(transparent)]
pub struct Symbol(Arc<Definition>);

impl Symbol {
    #[inline]
    pub fn function(&self) -> &Arc<Definition> {
        &self.0
    }

    #[inline]
    pub fn as_ref(&self) -> Option<&Id> {
        self.0.symbol_ref()
    }
}

impl std::cmp::PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl std::cmp::Eq for Symbol {}

impl std::hash::Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_tuple("Symbol").field(&self.as_ref()).finish()
    }
}

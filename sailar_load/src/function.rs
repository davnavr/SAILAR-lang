//! Module for interacting with SAILAR function definitions and instantiations.

use crate::error;
use crate::module;
use crate::type_system;
use sailar::helper::borrow::CowBox;
use sailar::record;
use sailar::signature;
use std::borrow::{Borrow, Cow};
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

type SignatureRecord = Cow<'static, signature::Function>;

pub struct Signature {
    signature: SignatureRecord,
    types: lazy_init::Lazy<Result<Box<[Arc<type_system::Signature>]>, error::TypeSignatureNotFoundError>>,
    module: Weak<module::Module>,
}

impl Signature {
    pub(crate) fn new(signature: SignatureRecord, module: Weak<module::Module>) -> Arc<Self> {
        Arc::new(Self {
            signature,
            types: Default::default(),
            module,
        })
    }

    pub fn record(&self) -> &signature::Function {
        &self.signature
    }

    pub fn module(&self) -> &Weak<module::Module> {
        &self.module
    }

    /// Returns the function signature's return types and parameter types.
    pub fn types(&self) -> Result<&[Arc<type_system::Signature>], error::TypeSignatureNotFoundError> {
        self.types
            .get_or_create(|| match self.module.upgrade() {
                Some(module) => {
                    let mut types = Vec::with_capacity(self.signature.types().len());
                    for index in self.signature.types().iter().copied() {
                        types.push(module.get_type_signature(index)?.clone());
                    }
                    Ok(types.into_boxed_slice())
                }
                None => todo!("handle missing module"),
            })
            .as_ref()
            .map(|types| types.borrow())
            .map_err(|err| err.clone())
    }
}

impl Debug for Signature {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Signature").field("record", &self.signature).finish()
    }
}

type InstantiationRecord = CowBox<'static, record::FunctionInstantiation>;

pub struct Instantiation {
    instantiation: InstantiationRecord,
    //template: Mutex<Template>,
    module: Weak<module::Module>,
}

impl Instantiation {
    pub(crate) fn new(instantiation: InstantiationRecord, module: Weak<module::Module>) -> Arc<Self> {
        Arc::new(Self { instantiation, module })
    }

    pub fn record(&self) -> &record::FunctionInstantiation {
        &self.instantiation
    }

    pub fn module(&self) -> &Weak<module::Module> {
        &self.module
    }
}

impl Debug for Instantiation {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Instantiation").field("record", &self.instantiation).finish()
    }
}

type DefinitionRecord = CowBox<'static, record::FunctionDefinition<'static>>;

pub struct Definition {
    definition: DefinitionRecord,
    module: Weak<module::Module>,
}

impl Definition {
    pub(crate) fn new(definition: DefinitionRecord, module: Weak<module::Module>) -> Arc<Self> {
        Arc::new(Self { definition, module })
    }

    pub fn module(&self) -> &Weak<module::Module> {
        &self.module
    }

    pub fn record(&self) -> &record::FunctionDefinition<'static> {
        &self.definition
    }

    pub fn to_symbol(self: &Arc<Self>) -> Option<Symbol> {
        Symbol::new(self.clone())
    }
}

impl Debug for Definition {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Definition").field("definition", &self.definition).finish()
    }
}

crate::symbol_wrapper!(pub struct Symbol(Definition));

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

#[derive(Clone)]
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

impl Debug for Template {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Definition(definition) => f.debug_tuple("Definition").field(&DefinitionDebug(definition)).finish(),
        }
    }
}

impl From<Arc<Definition>> for Template {
    fn from(definition: Arc<Definition>) -> Self {
        Self::Definition(definition)
    }
}

type SignatureRecord = Cow<'static, signature::Function>;

pub struct Signature {
    signature: SignatureRecord,
    types: type_system::LazySignatureList,
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
    pub fn types(&self) -> Result<&[Arc<type_system::Signature>], error::LoaderError> {
        self.types
            .get_or_initialize(&self.module, self.signature.types().iter().copied())
    }

    pub fn return_types(&self) -> Result<&[Arc<type_system::Signature>], error::LoaderError> {
        self.types().map(|types| &types[0..self.record().return_types().len()])
    }
}

impl Debug for Signature {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Signature")
            .field("record", &self.signature)
            .field("types", &self.types)
            .finish()
    }
}

type InstantiationRecord = record::FunctionInstantiation;

pub struct Instantiation {
    instantiation: Box<InstantiationRecord>,
    template: lazy_init::Lazy<Result<Template, error::LoaderError>>,
    module: Weak<module::Module>,
}

impl Instantiation {
    pub(crate) fn new(instantiation: Box<InstantiationRecord>, module: Weak<module::Module>) -> Arc<Self> {
        Arc::new(Self {
            instantiation,
            template: Default::default(),
            module,
        })
    }

    pub fn record(&self) -> &record::FunctionInstantiation {
        &self.instantiation
    }

    pub fn module(&self) -> &Weak<module::Module> {
        &self.module
    }

    pub fn template(&self) -> Result<&Template, error::LoaderError> {
        self.template
            .get_or_create(|| {
                module::Module::upgrade_weak(&self.module)
                    .and_then(|module| module.get_function_template(self.record().template()))
            })
            .as_ref()
            .map_err(Clone::clone)
    }
}

impl Debug for Instantiation {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Instantiation")
            .field("record", &self.instantiation)
            .field("template", &self.template)
            .finish()
    }
}

type DefinitionRecord = CowBox<'static, record::FunctionDefinition<'static>>;

/// Represents a function body.
#[derive(Clone, Debug)]
pub enum Body {
    Defined(Arc<crate::code_block::Code>),
}

/// Represents a function definition
pub struct Definition {
    definition: DefinitionRecord,
    body: lazy_init::Lazy<Result<Body, error::LoaderError>>,
    module: Weak<module::Module>,
}

impl Definition {
    pub(crate) fn new(definition: DefinitionRecord, module: Weak<module::Module>) -> Arc<Self> {
        Arc::new(Self {
            definition,
            body: Default::default(),
            module,
        })
    }

    pub fn module(&self) -> &Weak<module::Module> {
        &self.module
    }

    pub fn record(&self) -> &record::FunctionDefinition<'static> {
        &self.definition
    }

    pub fn body(&self) -> Result<Body, error::LoaderError> {
        self.body
            .get_or_create(|| match self.definition.body() {
                record::FunctionBody::Definition(code) => {
                    let module = module::Module::upgrade_weak(&self.module)?;
                    Ok(Body::Defined(module.get_code_block(*code)?.clone()))
                }
                record::FunctionBody::Foreign { .. } => todo!("foreign function bodies not yet supported"),
            })
            .as_ref()
            .cloned()
            .map_err(Clone::clone)
    }

    pub fn to_symbol(self: &Arc<Self>) -> Option<Symbol> {
        Symbol::new(self.clone())
    }

    pub fn to_template(self: &Arc<Self>) -> Template {
        Template::from(self.clone())
    }
}

impl Debug for Definition {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Definition")
            .field("definition", &self.definition)
            .field("body", &self.body)
            .finish()
    }
}

crate::symbol_wrapper!(pub struct Symbol(Definition));

/// Helepr struct that provides a shortened [`Debug`] representation of [`Definition`].
#[repr(transparent)]
pub struct DefinitionDebug<'a>(&'a Definition);

impl<'a, T: Borrow<Definition>> From<&'a T> for DefinitionDebug<'a> {
    fn from(reference: &'a T) -> Self {
        Self(reference.borrow())
    }
}

impl Debug for DefinitionDebug<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Definition")
            .field("export", &self.0.definition.export())
            .finish()
    }
}

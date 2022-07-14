//! Module for interacting with SAILAR function definitions and instantiations.

use crate::error;
use crate::module;
use crate::type_system;
use sailar::record;
use sailar::signature;
use std::fmt::{Debug, Display, Formatter};
use std::sync::{Arc, Weak};

#[derive(Clone, Debug)]
pub enum Template {
    Definition(Arc<Definition>),
    //Import()
}

impl Template {
    pub fn as_definition(&self) -> Result<&Arc<Definition>, error::LoaderError> {
        match self {
            Self::Definition(definition) => Ok(definition),
        }
    }
}

impl From<Arc<Definition>> for Template {
    fn from(definition: Arc<Definition>) -> Self {
        Self::Definition(definition)
    }
}

pub struct Signature {
    return_type_count: usize,
    index: sailar::index::FunctionSignature,
    types: type_system::LazySignatureList,
    module: Weak<module::Module>,
}

impl Signature {
    pub(crate) fn new(
        signature: std::borrow::Cow<'static, signature::Function>,
        index: sailar::index::FunctionSignature,
        module: Weak<module::Module>,
    ) -> Arc<Self> {
        Arc::new(Self {
            index,
            return_type_count: signature.return_type_count,
            types: type_system::LazySignatureList::new(signature.into_owned().types),
            module,
        })
    }

    pub fn index(&self) -> sailar::index::FunctionSignature {
        self.index
    }

    pub fn module(&self) -> &Weak<module::Module> {
        &self.module
    }

    /// Returns the function signature's return types and parameter types.
    pub fn types(&self) -> Result<&[Arc<type_system::Signature>], error::LoaderError> {
        self.types.get_or_initialize(&self.module)
    }

    pub fn return_types(&self) -> Result<&[Arc<type_system::Signature>], error::LoaderError> {
        self.types().map(|types| &types[0..self.return_type_count])
    }

    pub fn parameter_types(&self) -> Result<&[Arc<type_system::Signature>], error::LoaderError> {
        self.types().map(|types| &types[self.return_type_count..])
    }
}

impl Debug for Signature {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Signature").field("types", &self.types).finish()
    }
}

impl Display for Signature {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match (self.return_types(), self.parameter_types()) {
            (Ok(return_types), Ok(parameter_types)) => {
                use std::fmt::Write;

                fn fmt_types(types: &[Arc<type_system::Signature>], f: &mut Formatter) -> std::fmt::Result {
                    for (i, ty) in types.iter().enumerate() {
                        if i > 0 {
                            f.write_str(", ")?;
                        }

                        Display::fmt(ty, f)?;
                    }

                    Ok(())
                }

                f.write_char('(')?;
                fmt_types(parameter_types, f)?;
                f.write_str(") -> (")?;
                fmt_types(return_types, f)?;
                f.write_char(')')
            }
            _ => write!(f, "#{}", usize::from(self.index)),
        }
    }
}

impl std::cmp::PartialEq for Signature {
    fn eq(&self, other: &Self) -> bool {
        if let (Ok(x), Ok(y)) = (self.types(), other.types()) {
            x == y
        } else {
            false
        }
    }
}

pub struct Instantiation {
    template: lazy_init::LazyTransform<sailar::index::FunctionTemplate, Result<Template, error::LoaderError>>,
    export: module::Export,
    index: sailar::index::FunctionInstantiation,
    module: Weak<module::Module>,
}

impl Instantiation {
    pub(crate) fn new(
        instantiation: record::FunctionInstantiation<'static>,
        index: sailar::index::FunctionInstantiation,
        module: Weak<module::Module>,
    ) -> Arc<Self> {
        Arc::new(Self {
            template: lazy_init::LazyTransform::new(instantiation.template),
            export: instantiation.export,
            index,
            module,
        })
    }

    pub fn module(&self) -> &Weak<module::Module> {
        &self.module
    }

    pub fn index(&self) -> sailar::index::FunctionInstantiation {
        self.index
    }

    pub fn export(&self) -> &module::Export {
        &self.export
    }

    pub fn template(&self) -> Result<&Template, error::LoaderError> {
        self.template
            .get_or_create(|template| {
                module::Module::upgrade_weak(&self.module).and_then(|module| module.get_function_template(template))
            })
            .as_ref()
            .map_err(Clone::clone)
    }

    pub fn to_symbol(self: &Arc<Self>) -> Option<Symbol> {
        Symbol::new(self.clone())
    }

    pub fn signature(&self) -> Result<&Arc<Signature>, error::LoaderError> {
        match self.template()? {
            Template::Definition(definition) => definition.signature(),
        }
    }
}

impl Debug for Instantiation {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Instantiation")
            .field("export", &self.export)
            .field("template", &self.template.get())
            .finish()
    }
}

/// Represents a function body.
#[derive(Clone, Debug)]
pub enum Body {
    Defined(Arc<crate::code_block::Code>),
}

/// Represents a function definition.
pub struct Definition {
    index: usize,
    body: lazy_init::LazyTransform<record::FunctionBody<'static>, Result<Body, error::LoaderError>>,
    signature: lazy_init::LazyTransform<sailar::index::FunctionSignature, Result<Arc<Signature>, error::LoaderError>>,
    module: Weak<module::Module>,
}

impl Definition {
    pub(crate) fn new(definition: record::FunctionDefinition<'static>, index: usize, module: Weak<module::Module>) -> Arc<Self> {
        Arc::new(Self {
            index,
            body: lazy_init::LazyTransform::new(definition.body),
            signature: lazy_init::LazyTransform::new(definition.signature),
            module,
        })
    }

    pub fn module(&self) -> &Weak<module::Module> {
        &self.module
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn body(&self) -> Result<&Body, error::LoaderError> {
        self.body
            .get_or_create(|body| match body {
                record::FunctionBody::Definition(code) => {
                    let module = module::Module::upgrade_weak(&self.module)?;
                    Ok(Body::Defined(module.get_code_block(code)?.clone()))
                }
                record::FunctionBody::Foreign { .. } => todo!("foreign function bodies not yet supported"),
            })
            .as_ref()
            .map_err(Clone::clone)
    }

    pub fn signature(&self) -> Result<&Arc<Signature>, error::LoaderError> {
        self.signature
            .get_or_create(|signature| {
                module::Module::upgrade_weak(&self.module)?
                    .get_function_signature(signature)
                    .cloned()
            })
            .as_ref()
            .map_err(Clone::clone)
    }

    pub fn to_template(self: &Arc<Self>) -> Template {
        Template::from(self.clone())
    }
}

impl Debug for Definition {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Definition")
            .field("signature", &self.signature.get())
            .field("body", &self.body.get())
            .finish()
    }
}

crate::symbol_wrapper!(pub struct Symbol(Instantiation));

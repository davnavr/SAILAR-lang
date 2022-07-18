//! Module for interacting with SAILAR function definitions and instantiations.

use crate::error;
use crate::module;
use crate::type_system;
use sailar::record;
use sailar::signature;
use std::fmt::{Debug, Display, Formatter};
use std::sync::{Arc, Weak};

/// Represents a defined or imported function template.
#[derive(Clone, Debug)]
pub enum Template {
    Defined(Arc<DefinedTemplate>),
    //Import()
}

impl Template {
    pub fn as_definition(&self) -> Result<&Arc<DefinedTemplate>, error::LoaderError> {
        match self {
            Self::Defined(definition) => Ok(definition),
        }
    }

    //pub fn export
}

impl From<Arc<DefinedTemplate>> for Template {
    fn from(definition: Arc<DefinedTemplate>) -> Self {
        Self::Defined(definition)
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
        signature: signature::Function,
        index: sailar::index::FunctionSignature,
        module: Weak<module::Module>,
    ) -> Arc<Self> {
        Arc::new(Self {
            index,
            return_type_count: signature.return_type_count,
            types: type_system::LazySignatureList::new(signature.types),
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

/// Represents a function instantiation.
///
/// Instances of `Function` represent the targets of `call` instructions or the entry points of programs.
pub struct Function {
    template: lazy_init::LazyTransform<sailar::index::FunctionTemplate, Result<Template, error::LoaderError>>,
    index: sailar::index::Function,
    module: Weak<module::Module>,
}

impl Function {
    pub(crate) fn new(
        instantiation: record::Function<'static>,
        index: sailar::index::Function,
        module: Weak<module::Module>,
    ) -> Arc<Self> {
        Arc::new(Self {
            template: lazy_init::LazyTransform::new(instantiation.template),
            index,
            module,
        })
    }

    pub fn module(&self) -> &Weak<module::Module> {
        &self.module
    }

    pub fn index(&self) -> sailar::index::Function {
        self.index
    }

    pub fn template(&self) -> Result<&Template, error::LoaderError> {
        self.template
            .get_or_create(|template| {
                module::Module::upgrade_weak(&self.module).map(|module| module.index_function_template(template))
            })
            .as_ref()
            .map_err(Clone::clone)
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Instantiation")
            .field("index", &self.index)
            .field("template", &self.template.get())
            .finish()
    }
}

type DefinedBody = Arc<crate::code_block::Code>;

/// Represents a defined function template, containing SAILAR code as its body.
pub struct DefinedTemplate {
    index: sailar::index::FunctionTemplate,
    export: module::Export,
    entry_block: lazy_init::LazyTransform<sailar::index::CodeBlock, Result<DefinedBody, error::LoaderError>>,
    signature: lazy_init::LazyTransform<sailar::index::FunctionSignature, Result<Arc<Signature>, error::LoaderError>>,
    module: Weak<module::Module>,
}

impl DefinedTemplate {
    pub(crate) fn new(
        template: record::FunctionTemplate<'static>,
        index: sailar::index::FunctionTemplate,
        module: Weak<module::Module>,
    ) -> Arc<Self> {
        Arc::new(Self {
            index,
            export: template.export,
            entry_block: lazy_init::LazyTransform::new(template.entry_block),
            signature: lazy_init::LazyTransform::new(template.signature),
            module,
        })
    }

    pub fn module(&self) -> &Weak<module::Module> {
        &self.module
    }

    pub fn index(&self) -> sailar::index::FunctionTemplate {
        self.index
    }

    pub fn export(&self) -> &module::Export {
        &self.export
    }

    pub fn entry_block(&self) -> Result<&DefinedBody, error::LoaderError> {
        self.entry_block
            .get_or_create(|entry_block| {
                module::Module::upgrade_weak(&self.module).map(|module| module.code_blocks()[usize::from(entry_block)].clone())
            })
            .as_ref()
            .map_err(Clone::clone)
    }

    pub fn signature(&self) -> Result<&Arc<Signature>, error::LoaderError> {
        self.signature
            .get_or_create(|signature| {
                module::Module::upgrade_weak(&self.module)
                    .map(|module| module.function_signatures()[usize::from(signature)].clone())
            })
            .as_ref()
            .map_err(Clone::clone)
    }

    pub fn to_template(self: &Arc<Self>) -> Template {
        Template::from(self.clone())
    }

    pub fn to_symbol(self: &Arc<Self>) -> Option<Symbol> {
        Symbol::new(self.clone())
    }
}

impl Debug for DefinedTemplate {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Definition")
            .field("index", &self.index)
            .field("export", &self.export)
            .field("signature", &self.signature.get())
            .field("entry_block", &self.entry_block.get())
            .finish()
    }
}

crate::symbol_wrapper!(pub struct Symbol(DefinedTemplate));

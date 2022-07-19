//! Module for interacting with SAILAR binary modules.

use crate::code_block;
use crate::error;
use crate::function;
use crate::state::State;
use crate::type_system;
use sailar::identifier::Id;
use sailar::record;
use std::borrow::{Borrow, Cow};
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, Weak};

pub type ModuleIdentifier = record::ModuleIdentifier<'static>;

pub type Export = record::Export<'static>;

pub struct Module {
    loader: Weak<State>,
    module_identifiers: sailar::validation::ModuleIdentifierSet<'static>,
    entry_point: Option<Arc<function::Function>>,
    symbols: crate::symbol::Lookup,
    identifiers: Vec<Cow<'static, Id>>,
    type_signatures: Box<[Arc<type_system::Signature>]>,
    function_signatures: Box<[Arc<function::Signature>]>,
    code_blocks: Box<[Arc<code_block::Code>]>,
    defined_function_templates: Box<[Arc<function::DefinedTemplate>]>,
    functions: Box<[Arc<function::Function>]>,
}

impl Module {
    pub(crate) fn from_contents(contents: sailar::validation::ModuleContents<'static>, loader: Weak<State>) -> Arc<Self> {
        Arc::new_cyclic(|this| {
            let functions: Box<[_]> = contents
                .functions
                .into_iter()
                .enumerate()
                .map(|(index, function)| function::Function::new(function, index.into(), this.clone()))
                .collect();

            let mut symbols = crate::symbol::Lookup::new();

            Self {
                loader,
                module_identifiers: contents.module_identifiers,
                entry_point: contents.entry_point.map(|index| functions[usize::from(index)].clone()),
                identifiers: contents.identifiers,
                type_signatures: contents
                    .type_signatures
                    .into_iter()
                    .enumerate()
                    .map(|(index, signature)| type_system::Signature::new(signature, index.into(), this.clone()))
                    .collect(),
                function_signatures: contents
                    .function_signatures
                    .into_iter()
                    .enumerate()
                    .map(|(index, signature)| function::Signature::new(signature, index.into(), this.clone()))
                    .collect(),
                code_blocks: contents
                    .code
                    .into_iter()
                    .enumerate()
                    .map(|(index, code)| code_block::Code::new(code, index.into(), this.clone()))
                    .collect(),
                defined_function_templates: contents
                    .function_templates
                    .into_iter()
                    .enumerate()
                    // TODO: Add number of imported function templates to index
                    .map(|(index, template)| {
                        let a = function::DefinedTemplate::new(template, index.into(), this.clone());
                        if let Some(symbol) = a.to_symbol() {
                            symbols.insert(symbol);
                        }
                        a
                    })
                    .collect(),
                functions,
                symbols,
            }
        })
    }

    /// Attempts to upgrade a [`Weak`] pointer to a [`Module`], returning a [`LoaderError`] if the module was dropped.
    ///
    /// Use this function when a reference to a module is required to do something.
    ///
    /// [`Weak`]: std::sync::Weak
    /// [`LoaderError`]: error::LoaderError
    pub fn upgrade_weak(this: &Weak<Self>) -> Result<Arc<Self>, error::LoaderError> {
        this.upgrade()
            .ok_or_else(|| error::LoaderError::new(error::DroppedError::new(())))
    }

    /// Returns `false` if the module has at least one identifier (a name and version); otherwise, `true`.
    pub fn is_anonymous(&self) -> bool {
        self.module_identifiers.is_empty()
    }

    pub fn loader(&self) -> &Weak<State> {
        &self.loader
    }

    pub fn symbols(&self) -> &crate::symbol::Lookup {
        &self.symbols
    }

    pub fn module_identifiers(&self) -> &sailar::validation::ModuleIdentifierSet<'static> {
        &self.module_identifiers
    }

    pub fn entry_point<'a>(self: &'a Arc<Self>) -> Option<&'a Arc<function::Function>> {
        self.entry_point.as_ref()
    }

    pub fn identifiers(&self) -> &[Cow<'static, Id>] {
        &self.identifiers
    }

    pub fn type_signatures(&self) -> &[Arc<type_system::Signature>] {
        &self.type_signatures
    }

    pub fn function_signatures(&self) -> &[Arc<function::Signature>] {
        &self.function_signatures
    }

    pub fn code_blocks(&self) -> &[Arc<code_block::Code>] {
        &self.code_blocks
    }

    pub fn defined_function_templates(&self) -> &[Arc<function::DefinedTemplate>] {
        &self.defined_function_templates
    }

    pub fn functions(&self) -> &[Arc<function::Function>] {
        &self.functions
    }

    pub fn index_function_template(&self, index: sailar::index::FunctionTemplate) -> function::Template {
        // TODO: Handle imported function templates
        self.defined_function_templates[usize::from(index)].clone().into()
    }
}

impl Debug for Module {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Module")
            .field("module_identifier", &self.module_identifiers)
            .field("symbols", &self.symbols)
            .field("identifiers", &self.identifiers)
            .field("type_signatures", &self.type_signatures)
            .field("function_signatures", &self.function_signatures)
            .finish()
    }
}

impl std::cmp::PartialEq for Module {
    fn eq(&self, other: &Self) -> bool {
        !self.module_identifiers().is_disjoint(other.module_identifiers())
    }
}

impl std::cmp::Eq for Module {}

pub(crate) fn module_weak_eq(a: &Weak<Module>, b: &Weak<Module>) -> bool {
    a.ptr_eq(b) || a.upgrade().zip(b.upgrade()).map_or(false, |(a, b)| a == b)
}

/// Helper struct for displaying the name and version of a [`Module`] using [`Display`].
///
/// [`Display`]: std::fmt::Display
#[repr(transparent)]
pub struct Display<'a>(&'a Module);

impl<'a, T: Borrow<Module>> From<&'a T> for Display<'a> {
    fn from(reference: &'a T) -> Self {
        Self(reference.borrow())
    }
}

impl Debug for Display<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Debug::fmt(&self.0.module_identifiers(), f)
    }
}

impl std::fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        // TODO: Have a better way to display multiple module identifiers
        if let Some(identifier) = self.0.module_identifiers().iter().next() {
            Debug::fmt(identifier.name(), f)?;
            for (i, number) in identifier.version().iter().enumerate() {
                if i == 0 {
                    f.write_str(", ")?;
                } else {
                    std::fmt::Write::write_char(f, '.')?;
                }

                std::fmt::Display::fmt(number, f)?;
            }
            Ok(())
        } else {
            write!(f, "<anonymous>@{:p}", self.0)
        }
    }
}

//! Module for interacting with SAILAR binary modules.

use crate::code_block;
use crate::error;
use crate::function;
use crate::source::Source;
use crate::state::State;
use crate::symbol::{DuplicateSymbolError, Symbol};
use crate::type_system;
use sailar::identifier::Id;
use sailar::index;
use sailar::record;
use std::borrow::{Borrow, Cow};
use std::collections::hash_map;
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, Weak};

pub type Record = record::Record<'static>;

pub type ModuleIdentifier = record::ModuleIdentifier<'static>;

pub type Export = record::Export<'static>;

pub struct SymbolLookup {
    lookup: rustc_hash::FxHashMap<Symbol, ()>,
}

impl SymbolLookup {
    pub fn get<S: ?Sized>(&self, symbol: &S) -> Option<&Symbol>
    where
        Symbol: std::borrow::Borrow<S>,
        S: std::hash::Hash + std::cmp::Eq,
    {
        self.lookup.get_key_value(symbol).map(|(k, _)| k)
    }

    pub fn iter(&self) -> impl std::iter::ExactSizeIterator<Item = &Symbol> {
        self.lookup.keys()
    }

    fn try_insert<S: Into<Symbol>>(&mut self, symbol: Option<S>) -> Result<(), DuplicateSymbolError> {
        if let Some(s) = symbol {
            match self.lookup.entry(s.into()) {
                hash_map::Entry::Occupied(occupied) => Err(DuplicateSymbolError::new(occupied.key().clone())),
                hash_map::Entry::Vacant(vacant) => {
                    vacant.insert(());
                    Ok(())
                }
            }
        } else {
            Ok(())
        }
    }
}

impl Debug for SymbolLookup {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

type LazyEntryPoint =
    Option<lazy_init::LazyTransform<index::FunctionInstantiation, Result<Arc<function::Instantiation>, error::LoaderError>>>;

pub struct Module {
    loader: Weak<State>,
    module_identifier: Option<Arc<ModuleIdentifier>>,
    entry_point: LazyEntryPoint,
    symbols: SymbolLookup,
    identifiers: Vec<Cow<'static, Id>>,
    type_signatures: Vec<Arc<type_system::Signature>>,
    function_signatures: Vec<Arc<function::Signature>>,
    code_blocks: Vec<Arc<code_block::Code>>,
    function_definitions: Vec<Arc<function::Definition>>,
    //function_imports: Vec<Arc<function::Import>>,
    function_instantiations: Vec<Arc<function::Instantiation>>,
}

impl Module {
    fn initialize<S>(&mut self, this: &Weak<Self>, source: S) -> Result<(), S::Error>
    where
        S: Source,
        S::Error: std::error::Error,
    {
        for result in source.source()? {
            match result? {
                Record::MetadataField(field) => match field {
                    record::MetadataField::ModuleIdentifier(identifier) => self.module_identifier = Some(Arc::new(identifier)),
                    record::MetadataField::EntryPoint(index) => self.entry_point = Some(lazy_init::LazyTransform::new(index)),
                    bad => todo!("unknown metadata field {:?}", bad),
                },
                Record::Identifier(identifier) => self.identifiers.push(identifier),
                Record::TypeSignature(signature) => self.type_signatures.push(crate::type_system::Signature::new(
                    signature.into_owned(),
                    self.type_signatures().len().into(),
                    this.clone(),
                )),
                Record::FunctionSignature(signature) => self.function_signatures.push(function::Signature::new(
                    signature,
                    self.function_signatures.len().into(),
                    this.clone(),
                )),
                Record::CodeBlock(code) => self.code_blocks.push(code_block::Code::new(*code.into_boxed(), this.clone())),
                Record::FunctionDefinition(definition) => {
                    let function = function::Definition::new(*definition.into_boxed(), this.clone());
                    self.symbols
                        .try_insert(function.to_symbol())
                        .expect("TODO: handle duplicate symbol error");
                    self.function_definitions.push(function);
                }
                Record::FunctionInstantiation(instantiation) => self
                    .function_instantiations
                    .push(function::Instantiation::new(*instantiation.into_boxed(), this.clone())),
                bad => todo!("unsupported {:?}", bad),
            }
        }

        Ok(())
    }

    pub(crate) fn from_source<S>(source: S, loader: Weak<State>) -> Result<Arc<Self>, S::Error>
    where
        S: Source,
        S::Error: std::error::Error,
    {
        let mut error = None;
        let module = Arc::new_cyclic(|this| {
            let mut module = Self {
                loader,
                module_identifier: None,
                entry_point: None,
                symbols: SymbolLookup {
                    lookup: Default::default(),
                },
                identifiers: Vec::default(),
                type_signatures: Vec::default(),
                function_signatures: Vec::default(),
                code_blocks: Vec::default(),
                function_definitions: Vec::default(),
                function_instantiations: Vec::default(),
            };

            error = module.initialize(this, source).err();
            module
        });

        if let Some(e) = error {
            Err(e)
        } else {
            Ok(module)
        }
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

    /// Returns `false` if the module has an identifier (a name and version); otherwise, `true`.
    pub fn is_anonymous(&self) -> bool {
        self.module_identifier.is_none()
    }

    pub fn loader(&self) -> &Weak<State> {
        &self.loader
    }

    pub fn symbols(&self) -> &SymbolLookup {
        &self.symbols
    }

    /// Gets an optional weak reference to the module's identifier, indicating its name and version.
    pub fn module_identifier(&self) -> Option<&Arc<ModuleIdentifier>> {
        self.module_identifier.as_ref()
    }

    pub fn entry_point<'a>(self: &'a Arc<Self>) -> Result<Option<&'a Arc<function::Instantiation>>, error::LoaderError> {
        match &self.entry_point {
            None => Ok(None),
            Some(entry_point) => entry_point
                .get_or_create(|index| self.get_function_instantiation(index).map(Clone::clone))
                .as_ref()
                .map(Some)
                .map_err(Clone::clone),
        }
    }

    fn fail_index_check<I: error::IndexType>(self: &Arc<Self>, index: I, maximum: Option<usize>) -> error::LoaderError {
        error::InvalidModuleError::new(error::InvalidIndexError::new(index, maximum), self.clone()).into()
    }

    fn get_with_index_check<T, I: error::IndexType>(self: &Arc<Self>, index: I, slice: &[T]) -> Result<&T, error::LoaderError> {
        slice
            .get(index.into())
            .ok_or_else(|| self.fail_index_check(index, if slice.is_empty() { None } else { Some(slice.len() - 1) }))
    }

    pub fn identifiers(&self) -> &[Cow<'static, Id>] {
        &self.identifiers
    }

    pub fn type_signatures(&self) -> &[Arc<type_system::Signature>] {
        &self.type_signatures
    }

    pub fn get_type_signature<'a>(
        self: &'a Arc<Self>,
        index: index::TypeSignature,
    ) -> Result<&'a Arc<type_system::Signature>, error::LoaderError> {
        self.get_with_index_check(index, &self.type_signatures)
    }

    pub fn function_signatures(&self) -> &[Arc<function::Signature>] {
        &self.function_signatures
    }

    pub fn get_function_signature<'a>(
        self: &'a Arc<Self>,
        index: index::FunctionSignature,
    ) -> Result<&'a Arc<function::Signature>, error::LoaderError> {
        self.get_with_index_check(index, &self.function_signatures)
    }

    pub fn code_blocks(&self) -> &[Arc<code_block::Code>] {
        &self.code_blocks
    }

    pub fn get_code_block<'a>(
        self: &'a Arc<Self>,
        index: index::CodeBlock,
    ) -> Result<&'a Arc<code_block::Code>, error::LoaderError> {
        self.get_with_index_check(index, &self.code_blocks)
    }

    pub fn function_definitions(&self) -> &[Arc<function::Definition>] {
        &self.function_definitions
    }

    pub fn function_instantiations(&self) -> &[Arc<function::Instantiation>] {
        &self.function_instantiations
    }

    pub fn get_function_instantiation<'a>(
        self: &'a Arc<Self>,
        index: index::FunctionInstantiation,
    ) -> Result<&'a Arc<function::Instantiation>, error::LoaderError> {
        self.get_with_index_check(index, &self.function_instantiations)
    }

    pub fn get_function_template(
        self: &Arc<Self>,
        index: index::FunctionTemplate,
    ) -> Result<function::Template, error::LoaderError> {
        let i = usize::from(index);
        // TODO: Update when there are function imports.
        if let Some(definition) = self.function_definitions.get(i) {
            Ok(function::Template::Definition(definition.clone()))
        } else {
            Err(self.fail_index_check(
                index,
                if self.function_definitions.is_empty() {
                    None
                } else {
                    Some(self.function_definitions.len() - 1)
                },
            ))
        }
    }
}

impl Debug for Module {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Module")
            .field("module_identifier", &self.module_identifier)
            .field("symbols", &self.symbols)
            .field("identifiers", &self.identifiers)
            .field("type_signatures", &self.type_signatures)
            .field("function_signatures", &self.function_signatures)
            .field("function_definitions", &self.function_definitions)
            .finish()
    }
}

impl std::cmp::PartialEq for Module {
    fn eq(&self, other: &Self) -> bool {
        self.module_identifier == other.module_identifier
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
        Debug::fmt(&self.0.module_identifier(), f)
    }
}

impl std::fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        if let Some(identifier) = self.0.module_identifier() {
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

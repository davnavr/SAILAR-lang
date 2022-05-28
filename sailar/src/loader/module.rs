//! Module for interacting with SAILAR binary modules.

use crate::binary::record;
use crate::identifier::Id;
use crate::loader;
use crate::loader::function;
use std::borrow::{Borrow, Cow};
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, Mutex, Weak};

pub type Record = record::Record<'static>;

#[derive(Clone)]
pub struct ModuleIdentifier(Arc<Module>);

impl ModuleIdentifier {
    pub fn name(&self) -> Option<&Id> {
        self.as_ref().map(|identifier| identifier.name())
    }

    /// Gets the version number specified in the module's identifier, or `None` if the module is anonymous.
    pub fn version(&self) -> Option<&[usize]> {
        self.as_ref().map(|identifier| identifier.version())
    }

    pub fn as_ref(&self) -> Option<&record::ModuleIdentifier<'static>> {
        self.0.get_module_identifier()
    }

    #[inline]
    pub fn module(&self) -> &Arc<Module> {
        &self.0
    }
}

impl std::cmp::PartialEq for ModuleIdentifier {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl std::cmp::Eq for ModuleIdentifier {}

impl std::hash::Hash for ModuleIdentifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

impl Borrow<Option<record::ModuleIdentifier<'static>>> for ModuleIdentifier {
    fn borrow(&self) -> &Option<record::ModuleIdentifier<'static>> {
        &self.0.module_identifier
    }
}

impl Debug for ModuleIdentifier {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let id: &Option<_> = self.borrow();
        f.debug_tuple("ModuleIdentifier").field(id).finish()
    }
}

#[derive(Debug)]
pub struct Module {
    loader: Weak<loader::State>,
    identifiers: Vec<Cow<'static, Id>>,
    module_identifier: Option<record::ModuleIdentifier<'static>>,
    function_definitions: Vec<Arc<function::Definition>>,
    function_instantiations: Vec<Arc<function::Instantiation>>,
    // TODO: Have lookup for exported functions
}

impl Module {
    pub(crate) fn from_source<S: loader::Source>(source: S, loader: &Arc<loader::State>) -> Result<Arc<Self>, S::Error> {
        let mut error = None;

        let module = Arc::new_cyclic(|module_weak| {
            let mut module = Self {
                loader: Arc::downgrade(loader),
                identifiers: Vec::default(),
                module_identifier: None,
                function_definitions: Vec::default(),
                function_instantiations: Vec::default(),
            };

            // TODO: How to error on duplicate module identifier?
            let result = source.iter_records(|record| match record {
                Record::MetadataField(field) => match field {
                    record::MetadataField::ModuleIdentifier(identifier) => module.module_identifier = Some(identifier),
                },
                Record::Identifier(identifier) => module.identifiers.push(identifier),
                Record::FunctionDefinition(definition) => module
                    .function_definitions
                    .push(function::Definition::new(definition, module_weak.clone())),
                bad => todo!("unsupported {:?}", bad),
            });

            if let Err(e) = result {
                error = Some(e);
            }

            module
        });

        match error {
            None => Ok(module),
            Some(e) => Err(e),
        }
    }

    /// Indicates if the module has an identifier (a name and version).
    pub fn is_anonymous(&self) -> bool {
        self.module_identifier.is_none()
    }

    /// Gets a weak reference to the loader.
    ///
    /// Since this reference is most likely to exist, consider using [`loader`] instead.
    pub fn loader_weak(&self) -> &Weak<loader::State> {
        &self.loader
    }

    /// Gets a reference to the loader.
    ///
    /// # Panics
    ///
    /// Panics if the loader was dropped. Code that uses the SAILAR loader should ensure that the loader outlives all loaded
    /// modules.
    pub fn loader(&self) -> Arc<loader::State> {
        match self.loader.upgrade() {
            Some(loader) => loader,
            None => panic!("loader was dropped"),
        }
    }

    #[inline]
    pub fn identifiers(&self) -> &[Cow<'static, Id>] {
        &self.identifiers
    }

    /// Gets an optional reference to the module's identifier.
    ///
    /// For a shared reference to the module's identifier, use [`module_identifier`].
    #[inline]
    pub fn get_module_identifier(&self) -> Option<&record::ModuleIdentifier<'static>> {
        self.module_identifier.as_ref()
    }

    /// Gets a shared, optional reference to the module's identifier.
    pub fn module_identifier(self: &Arc<Self>) -> ModuleIdentifier {
        ModuleIdentifier(self.clone())
    }
}

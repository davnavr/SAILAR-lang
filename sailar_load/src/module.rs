//! Module for interacting with SAILAR binary modules.

use sailar::record;
use sailar::identifier::Id;
use std::borrow::Cow;
use std::fmt::{Debug, Formatter};

pub type Record = record::Record<'static>;

pub type ModuleIdentifier = record::ModuleIdentifier<'static>;

pub struct Module<'s> {
    loader: &'s crate::State<'s>,
    identifiers: Vec<Cow<'static, Id>>,
    pub(super) module_identifier: Option<&'s ModuleIdentifier>,
    //function_definitions: Vec<Arc<function::Definition>>,
    //function_instantiations: Vec<Arc<function::Instantiation>>,
    //function_exports: rustc_hash::HashSet<Arc<function::Symbol>> // TODO: Have lookup for exported functions
}

impl<'s> Module<'s> {
    pub(crate) fn from_source<S: crate::Source>(
        source: S,
        loader: &'s crate::State<'s>,
        module_identifier: &mut Option<ModuleIdentifier>,
    ) -> Result<Box<Self>, S::Error> {
        let mut module = Box::new(Self {
            loader,
            identifiers: Vec::default(),
            module_identifier: None,
            //function_definitions: Vec::default(),
            //function_instantiations: Vec::default(),
        });

        // TODO: How to error on more than one module identifier?
        source.iter_records(|record| match record {
            Record::MetadataField(field) => match field {
                record::MetadataField::ModuleIdentifier(identifier) => *module_identifier = Some(identifier),
                bad => todo!("unknown metadata field {:?}", bad),
            },
            Record::Identifier(identifier) => module.identifiers.push(identifier),
            // Record::FunctionDefinition(definition) => module
            //     .function_definitions
            //     .push(function::Definition::new(definition, module_weak.clone())),
            // Record::FunctionInstantiation(instantiation) => module
            //     .function_instantiations
            //     .push(function::Instantiation::new(instantiation, module_weak.clone())),
            bad => todo!("unsupported {:?}", bad),
        })?;

        Ok(module)
    }

    /// Indicates if the module has an identifier (a name and version).
    pub fn is_anonymous(&'s self) -> bool {
        self.module_identifier.is_none()
    }

    #[inline]
    pub fn loader(&'s self) -> &'s crate::State {
        self.loader
    }

    #[inline]
    pub fn identifiers(&'s self) -> &'s [Cow<'static, Id>] {
        &self.identifiers
    }

    /// Gets an optional reference to the module's identifier.
    #[inline]
    pub fn module_identifier(&'s self) -> Option<&'s ModuleIdentifier> {
        self.module_identifier
    }

    // #[inline]
    // pub fn function_definitions(&self) -> &[Arc<function::Definition>] {
    //     &self.function_definitions
    // }

    // #[inline]
    // pub fn function_instantiations(&self) -> &[Arc<function::Instantiation>] {
    //     &self.function_instantiations
    // }
}

impl Debug for Module<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Module")
            .field("identifiers", &self.identifiers)
            .field("module_identifier", &self.module_identifier)
            .finish()
    }
}

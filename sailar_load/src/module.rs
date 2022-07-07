//! Module for interacting with SAILAR binary modules.

use sailar::identifier::Id;
use sailar::record;
use std::borrow::Cow;
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, Weak};

pub type Record = record::Record<'static>;

pub type ModuleIdentifier = record::ModuleIdentifier<'static>;

pub struct Module {
    loader: Weak<crate::State>,
    identifiers: Vec<Cow<'static, Id>>,
    module_identifier: Option<Arc<ModuleIdentifier>>,
    //function_definitions: Vec<Arc<function::Definition>>,
    //function_instantiations: Vec<Arc<function::Instantiation>>,
    //function_exports: rustc_hash::HashSet<Arc<function::Symbol>> // TODO: Have lookup for exported functions
}

impl Module {
    pub(crate) fn from_source<S: crate::Source>(source: S, loader: Weak<crate::State>) -> Result<Arc<Self>, S::Error> {
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
                record::MetadataField::ModuleIdentifier(identifier) => module.module_identifier = Some(Arc::new(identifier)),
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

        Ok(Arc::from(module))
    }

    /// Indicates if the module has an identifier (a name and version).
    pub fn is_anonymous(&self) -> bool {
        self.module_identifier.is_none()
    }

    pub fn loader(&self) -> &Weak<crate::State> {
        &self.loader
    }

    pub fn identifiers(&self) -> &[Cow<'static, Id>] {
        &self.identifiers
    }

    /// Gets an optional weak reference to the module's identifier, indicating its name and version.
    pub fn module_identifier(&self) -> Option<&Arc<ModuleIdentifier>> {
        self.module_identifier.as_ref()
    }

    // pub fn function_definitions(&self) -> &[Arc<function::Definition>] {
    //     &self.function_definitions
    // }

    // pub fn function_instantiations(&self) -> &[Arc<function::Instantiation>] {
    //     &self.function_instantiations
    // }
}

impl Debug for Module {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Module")
            .field("identifiers", &self.identifiers)
            .field("module_identifier", &self.module_identifier)
            .finish()
    }
}

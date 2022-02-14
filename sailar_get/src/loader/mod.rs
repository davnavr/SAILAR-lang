use sailar::format;
use std::collections::hash_map;

mod cache;
mod code;
mod data;
mod error;
mod field;
mod function;
mod module;
mod register;
mod resolver;
mod structure;
mod symbol;
mod type_signature;

pub use code::{Block as CodeBlock, Code, JumpTarget};
pub use data::Data;
pub use error::*;
pub use field::Field;
pub use format::{Identifier, ModuleIdentifier};
pub use function::{Function, Signature as FunctionSignature};
pub use module::Module;
pub use register::{Input as InputRegister, Register};
pub use resolver::{ReferenceResolver, ResolverClosure};
pub use structure::Struct;
pub use symbol::{Function as FunctionSymbol, Module as ModuleSymbol, Symbol};
pub use type_signature::{calculate_size as calculate_raw_type_size, Type as TypeSignature};

pub type Result<T> = std::result::Result<T, Error>;

fn read_index<
    I: TryInto<usize> + Copy + Into<format::numeric::UInteger>,
    T,
    R: FnOnce(usize) -> Result<T>,
>(
    index: I,
    reader: R,
) -> Result<T> {
    index
        .try_into()
        .map_err(|_| Error::IndexOutOfBounds(index.into()))
        .and_then(reader)
}

fn read_index_from<
    'a,
    I: TryInto<usize> + Copy + Into<format::numeric::UInteger>,
    T,
    U,
    R: FnOnce(&'a T) -> Result<U>,
>(
    index: I,
    s: &'a [T],
    reader: R,
) -> Result<U> {
    read_index::<I, U, _>(index, |actual_index| {
        s.get(actual_index)
            .ok_or_else(|| Error::IndexOutOfBounds(index.into()))
            .and_then(reader)
    })
}

pub type PointerSize = std::num::NonZeroU8;

pub struct Loader<'a> {
    pointer_size: PointerSize,
    resolver: std::cell::RefCell<&'a mut dyn ReferenceResolver>,
    resolved_modules: typed_arena::Arena<Module<'a>>,
    module_lookup: std::cell::RefCell<hash_map::HashMap<ModuleIdentifier, &'a Module<'a>>>,
}

impl<'a> Loader<'a> {
    pub fn initialize(
        loader: &'a mut Option<Loader<'a>>,
        pointer_size: PointerSize,
        resolver: &'a mut dyn ReferenceResolver,
        application: format::Module,
    ) -> (&'a Self, &'a Module<'a>) {
        let loaded = loader.insert(Self {
            pointer_size,
            resolver: std::cell::RefCell::new(resolver),
            resolved_modules: typed_arena::Arena::new(),
            module_lookup: std::cell::RefCell::new(hash_map::HashMap::new()),
        });

        (loaded, loaded.force_load_module(application))
    }

    fn force_load_module(&'a self, module: format::Module) -> &'a Module<'a> {
        self.resolved_modules.alloc(Module::new(self, module))
    }

    /// Returns the presumed pointer size, in bytes, used by all loaded modules.
    pub fn pointer_size(&'a self) -> PointerSize {
        self.pointer_size
    }

    pub fn load_module(&'a self, name: &ModuleIdentifier) -> Result<&'a Module<'a>> {
        match self.module_lookup.borrow_mut().entry(name.clone()) {
            hash_map::Entry::Occupied(occupied) => Ok(*occupied.get()),
            hash_map::Entry::Vacant(vacant) => {
                let loaded = self
                    .resolver
                    .borrow_mut()
                    .resolve(name)?
                    .ok_or_else(|| Error::ModuleNotFound(name.clone()))?;
                Ok(*vacant.insert(self.force_load_module(*loaded)))
            }
        }
    }

    pub fn lookup_module(&'a self, name: &ModuleIdentifier) -> Option<&'a Module<'a>> {
        self.module_lookup.borrow().get(name).copied()
    }

    pub fn lookup_function(&'a self, name: Symbol<'a>) -> Vec<&'a Function<'a>> {
        self.module_lookup
            .borrow()
            .values()
            .filter_map(|module| module.lookup_function(name.clone()))
            .collect()
    }
}

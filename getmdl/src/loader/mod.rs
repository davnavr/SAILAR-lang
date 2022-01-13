use registir::format;
use std::{cell::RefCell, collections::hash_map};
use typed_arena::Arena as TypedArena;

mod cache;
mod error;
mod function;
mod module;
mod symbol;

pub use error::Error;
pub use format::{Identifier, ModuleIdentifier};
pub use function::{Function, Signature as FunctionSignature};
pub use module::Module;
pub use symbol::{Symbol, Function as FunctionSymbol};

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
            .ok_or(Error::IndexOutOfBounds(index.into()))
            .and_then(reader)
    })
}

pub struct Loader<'a> {
    module_arena: TypedArena<Module<'a>>,
    loaded_modules: RefCell<hash_map::HashMap<ModuleIdentifier, &'a Module<'a>>>,
}

impl<'a> Loader<'a> {
    fn new_empty() -> Self {
        Self {
            module_arena: TypedArena::new(),
            loaded_modules: RefCell::new(hash_map::HashMap::new()),
        }
    }

    fn load_module_raw(&'a self, source: format::Module) -> &'a Module<'a> {
        let identifier = source.header.0.identifier.clone();
        match self.loaded_modules.borrow_mut().entry(identifier) {
            hash_map::Entry::Vacant(vacant) => {
                let loaded = self.module_arena.alloc(Module::new(source));
                vacant.insert(loaded);
                loaded
            }
            hash_map::Entry::Occupied(occupied) => occupied.get(),
        }
    }

    pub fn initialize(
        loader: &'a mut Option<Loader<'a>>,
        application: format::Module,
    ) -> (&'a Self, &'a Module<'a>) {
        let loaded = loader.insert(Loader::new_empty());
        (loaded, loaded.load_module_raw(application))
    }

    // TODO: How to force loading of a module if it is an import of one of the already loaded modules?
    pub fn lookup_module(
        &'a self,
        name: &ModuleIdentifier,
    ) -> std::result::Result<&'a Module<'a>, ()> {
        self.loaded_modules.borrow().get(name).copied().ok_or(())
    }
}

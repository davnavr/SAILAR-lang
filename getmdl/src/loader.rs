use registir::format;
use std::cell::RefCell;
use std::collections::{hash_map, HashMap};
use typed_arena::Arena as TypedArena;

pub use format::{Identifier, ModuleIdentifier};

pub struct Module<'a> {
    source: format::Module,
    type_arena: TypedArena<Type<'a>>,
    loaded_types: RefCell<HashMap<usize, &'a Type<'a>>>,
    //loaded_fields: ,
    method_arena: TypedArena<Method<'a>>,
    loaded_methods: RefCell<HashMap<usize, &'a Method<'a>>>,
}

#[derive(Debug)]
#[non_exhaustive]
pub enum LoadError {
    IndexOutOfBounds(format::numeric::UInteger),
    Other(Box<dyn std::error::Error>),
}

impl std::fmt::Display for LoadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IndexOutOfBounds(index) => write!(f, "Attempted load with index {}, which is out of bounds", index),
            Self::Other(error) => std::fmt::Display::fmt(error, f),
        }
    }
}

impl std::error::Error for LoadError { }

pub type LoadResult<T> = Result<T, LoadError>;

impl<'a> Module<'a> {
    fn new(source: format::Module) -> Self {
        Self {
            source,
            type_arena: TypedArena::new(),
            loaded_types: RefCell::new(HashMap::new()),
            method_arena: TypedArena::new(),
            loaded_methods: RefCell::new(HashMap::new()),
        }
    }

    pub fn identifier(&'a self) -> &'a ModuleIdentifier {
        &self.source.header.0.identifier
    }

    fn load_raw<
        T: 'a,
        I: TryInto<usize> + Copy + Into<format::numeric::UInteger>,
        L,
        F: FnOnce(usize) -> Option<&'a T>,
        C: FnOnce(&'a T) -> LoadResult<L>,
    >(
        lookup: &'a RefCell<HashMap<usize, &'a L>>,
        arena: &'a TypedArena<L>,
        loader: F,
        constructor: C,
        index: I,
    ) -> LoadResult<&'a L> {
        let raw_index = index
            .try_into()
            .map_err(|_| LoadError::IndexOutOfBounds(index.into()))?;
        match lookup.borrow_mut().entry(raw_index) {
            hash_map::Entry::Occupied(occupied) => Ok(occupied.get()),
            hash_map::Entry::Vacant(vacant) => match loader(raw_index) {
                Some(source) => {
                    let loaded = arena.alloc(constructor(source)?);
                    vacant.insert(loaded);
                    Ok(loaded)
                }
                None => Err(LoadError::IndexOutOfBounds(index.into())),
            },
        }
    }

    pub fn load_type_raw(
        &'a self,
        index: format::indices::TypeDefinition,
    ) -> LoadResult<&'a Type<'a>> {
        Self::load_raw(
            &self.loaded_types,
            &self.type_arena,
            |index| self.source.definitions.0.defined_types.0.get(index),
            |source| Ok(Type::new(self, source)),
            index,
        )
    }

    pub fn load_method_raw(
        &'a self,
        index: format::indices::MethodDefinition,
    ) ->  LoadResult<&'a Method<'a>> {
        Self::load_raw(
            &self.loaded_methods,
            &self.method_arena,
            |index| self.source.definitions.0.defined_methods.0.get(index),
            |source| Ok(Method::new(self.load_type_raw(source.owner)?, source)),
            index,
        )
    }

    pub fn entry_point(&'a self) ->  LoadResult<Option<&'a Method<'a>>> {
        match self.source.entry_point.0 {
            Some(main_index) => self.load_method_raw(main_index).map(Some),
            None => Ok(None)
        }
    }
}

pub struct Method<'a> {
    source: &'a format::Method,
    owner: &'a Type<'a>,
}

impl<'a> Method<'a> {
    fn new(owner: &'a Type<'a>, source: &'a format::Method) -> Self {
        Self { source, owner }
    }

    pub fn declaring_module(&'a self) -> &'a Module<'a> {
        self.owner.declaring_module()
    }

    /// Gets the raw method blocks that make up the method's body, if it is defined.
    pub fn raw_code(&'a self) -> LoadResult<Option<&'a format::Code>> {
        use format::MethodBody;

        match self.source.body {
            MethodBody::Defined(index) => usize::try_from(index)
                .map_err(|_| ())
                .and_then(|index| {
                    self.declaring_module()
                        .source
                        .method_bodies
                        .0
                        .get(index)
                        .ok_or(())
                })
                .map_err(|_| LoadError::IndexOutOfBounds(index.into()))
                .map(Some),
            MethodBody::Abstract | MethodBody::External { .. } => Ok(None),
        }
    }
}

pub struct Type<'a> {
    source: &'a format::Type,
    module: &'a Module<'a>,
    loaded_methods: RefCell<HashMap<usize, &'a Method<'a>>>,
}

impl<'a> Type<'a> {
    fn new(module: &'a Module<'a>, source: &'a format::Type) -> Self {
        Self {
            source,
            module,
            loaded_methods: RefCell::new(HashMap::new()),
        }
    }

    pub fn declaring_module(&'a self) -> &'a Module<'a> {
        self.module
    }
}

pub struct Loader<'a> {
    module_arena: TypedArena<Module<'a>>,
    loaded_modules: RefCell<HashMap<ModuleIdentifier, &'a Module<'a>>>,
}

impl<'a> Loader<'a> {
    pub fn new() -> Self {
        Self {
            module_arena: TypedArena::new(),
            loaded_modules: RefCell::new(HashMap::new()),
        }
    }

    pub fn load_module_raw(&'a self, source: format::Module) -> &'a Module {
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
}

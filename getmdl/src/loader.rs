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
            Self::IndexOutOfBounds(index) => write!(
                f,
                "attempted load with index {}, which is out of bounds",
                index
            ),
            Self::Other(error) => std::fmt::Display::fmt(error, f),
        }
    }
}

impl std::error::Error for LoadError {}

pub type LoadResult<T> = Result<T, LoadError>;

fn read_index<
    I: TryInto<usize> + Copy + Into<format::numeric::UInteger>,
    T,
    R: FnOnce(usize) -> LoadResult<T>,
>(
    index: I,
    reader: R,
) -> LoadResult<T> {
    index
        .try_into()
        .map_err(|_| LoadError::IndexOutOfBounds(index.into()))
        .and_then(reader)
}

fn read_index_from<
    'a,
    I: TryInto<usize> + Copy + Into<format::numeric::UInteger>,
    T,
    U,
    R: FnOnce(&'a T) -> LoadResult<U>,
>(
    index: I,
    s: &'a [T],
    reader: R,
) -> LoadResult<U> {
    read_index::<I, U, _>(index, |actual_index| {
        s.get(actual_index)
            .ok_or(LoadError::IndexOutOfBounds(index.into()))
            .and_then(reader)
    })
}

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
        read_index::<_, &'a L, _>(index, |raw_index| {
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
        })
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
    ) -> LoadResult<&'a Method<'a>> {
        Self::load_raw(
            &self.loaded_methods,
            &self.method_arena,
            |index| self.source.definitions.0.defined_methods.0.get(index),
            |source| Ok(Method::new(self.load_type_raw(source.owner)?, source)),
            index,
        )
    }

    pub fn entry_point(&'a self) -> LoadResult<Option<&'a Method<'a>>> {
        match self.source.entry_point.0 {
            Some(main_index) => self.load_method_raw(main_index).map(Some),
            None => Ok(None),
        }
    }

    pub fn load_type_signature_raw(
        &'a self,
        index: format::indices::TypeSignature,
    ) -> LoadResult<&'a format::TypeSignature> {
        read_index_from(index, &self.source.type_signatures.0, Ok)
    }

    fn collect_type_signatures_raw(
        &'a self,
        indices: &'a [format::indices::TypeSignature],
    ) -> LoadResult<Vec<&'a format::TypeSignature>> {
        let mut types = Vec::with_capacity(indices.len());
        for index in indices {
            types.push(self.load_type_signature_raw(*index)?);
        }
        Ok(types)
    }

    pub fn load_code_raw(&'a self, index: format::indices::Code) -> LoadResult<&'a format::Code> {
        read_index_from(index, &self.source.method_bodies.0, Ok)
    }
}

pub struct Method<'a> {
    source: &'a format::Method,
    owner: &'a Type<'a>,
}

pub struct MethodSignatureTypes<'a> {
    pub return_types: Vec<&'a format::TypeSignature>,
    pub parameter_types: Vec<&'a format::TypeSignature>,
}

impl<'a> Method<'a> {
    fn new(owner: &'a Type<'a>, source: &'a format::Method) -> Self {
        Self { source, owner }
    }

    pub fn declaring_module(&'a self) -> &'a Module<'a> {
        self.owner.declaring_module()
    }

    pub fn raw_body(&'a self) -> &'a format::MethodBody {
        &self.source.body
    }

    /// Gets the raw method blocks that make up the method's body, if it is defined.
    pub fn raw_code(&'a self) -> LoadResult<Option<&'a format::Code>> {
        use format::MethodBody;

        match self.raw_body() {
            MethodBody::Defined(index) => self.declaring_module().load_code_raw(*index).map(Some),
            MethodBody::Abstract | MethodBody::External { .. } => Ok(None),
        }
    }

    pub fn raw_signature(&'a self) -> LoadResult<&'a format::MethodSignature> {
        read_index_from(
            self.source.signature,
            &self.declaring_module().source.method_signatures.0,
            Ok,
        )
    }

    pub fn raw_signature_types(&'a self) -> LoadResult<MethodSignatureTypes<'a>> {
        let signature = self.raw_signature()?;
        Ok(MethodSignatureTypes {
            return_types: self
                .declaring_module()
                .collect_type_signatures_raw(&signature.return_types)?,
            parameter_types: self
                .declaring_module()
                .collect_type_signatures_raw(&signature.parameter_types)?,
        })
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
    fn new_empty() -> Self {
        Self {
            module_arena: TypedArena::new(),
            loaded_modules: RefCell::new(HashMap::new()),
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
}

use super::*;

pub struct Module<'a> {
    source: format::Module,
    function_signature_cache:
        cache::IndexLookup<'a, format::indices::FunctionSignature, FunctionSignature<'a>>,
    //loaded_structs
    //loaded_globals
    loaded_functions: cache::IndexLookup<'a, format::indices::FunctionDefinition, Function<'a>>,
    function_lookup_cache: RefCell<hash_map::HashMap<Symbol<'a>, &'a Function<'a>>>,
}

fn load_raw_cached<'a, I, T, L, F, C>(
    lookup: &'a cache::IndexLookup<'a, I, L>,
    index: I,
    loader: F,
    constructor: C,
) -> Result<&'a L>
where
    T: 'a,
    I: TryInto<usize> + Copy + Eq + std::hash::Hash + Into<format::numeric::UInteger>,
    F: FnOnce(usize) -> Result<Option<&'a T>>,
    C: FnOnce(&'a T) -> Result<L>,
{
    lookup.insert_or_get(index, |index| {
        read_index(index, |raw_index| {
            constructor(loader(raw_index)?.ok_or(Error::IndexOutOfBounds(index.into()))?)
        })
    })
}

impl<'a> Module<'a> {
    pub(crate) fn new(source: format::Module) -> Self {
        Self {
            source,
            function_signature_cache: cache::IndexLookup::new(),
            loaded_functions: cache::IndexLookup::new(),
            // TODO: Could construct the function_lookup_cache IF the module is known to not have an entry point.
            function_lookup_cache: RefCell::new(hash_map::HashMap::new()),
        }
    }

    /// Retrieves the module's name and version.
    pub fn identifier(&'a self) -> &'a ModuleIdentifier {
        &self.source.header.0.identifier
    }

    pub fn full_symbol(&'a self) -> ModuleSymbol<'a> {
        ModuleSymbol::Borrowed(self.identifier())
    }

    pub fn load_identifier_raw(
        &'a self,
        index: format::indices::Identifier,
    ) -> Result<&'a Identifier> {
        read_index_from(index, &self.source.identifiers, Ok)
    }

    pub fn load_type_signature_raw(
        &'a self,
        index: format::indices::TypeSignature,
    ) -> Result<&'a format::TypeSignature> {
        read_index_from(index, &self.source.type_signatures, Ok)
    }

    fn collect_type_signatures_raw(
        &'a self,
        indices: &'a [format::indices::TypeSignature],
    ) -> Result<Vec<&'a format::TypeSignature>> {
        let mut types = Vec::with_capacity(indices.len());
        for index in indices {
            types.push(self.load_type_signature_raw(*index)?);
        }
        Ok(types)
    }

    pub fn load_function_signature_raw(
        &'a self,
        index: format::indices::FunctionSignature,
    ) -> Result<&'a format::FunctionSignature> {
        read_index_from(index, &self.source.function_signatures, Ok)
    }

    pub fn load_function_signature(
        &'a self,
        index: format::indices::FunctionSignature,
    ) -> Result<&'a FunctionSignature<'a>> {
        load_raw_cached(
            &self.function_signature_cache,
            index,
            |_| self.load_function_signature_raw(index).map(Some),
            |signature| {
                Ok(FunctionSignature::new(
                    self.collect_type_signatures_raw(&signature.return_types)?,
                    self.collect_type_signatures_raw(&signature.parameter_types)?,
                ))
            },
        )
    }

    pub fn load_code_raw(&'a self, index: format::indices::Code) -> Result<&'a format::Code> {
        read_index_from(index, &self.source.function_bodies, Ok)
    }

    pub fn load_function_definition_raw(
        &'a self,
        index: format::indices::FunctionDefinition,
    ) -> Result<&'a Function<'a>> {
        load_raw_cached(
            &self.loaded_functions,
            index,
            |raw_index| {
                let functions: &[_] = &self.source.definitions.defined_functions;
                Ok(functions.get(raw_index))
            },
            |source| Ok(Function::new(self, source)),
        )
    }

    pub fn load_function_raw(&'a self, index: format::indices::Function) -> Result<&'a Function<'a>> {
        match index {
            format::indices::Function::Defined(defined_index) => self.load_function_definition_raw(defined_index),
            format::indices::Function::Imported(_) => todo!("resolution of function imports is not yet supported"),
        }
    }

    /// Retrieves the entry point for the application, if it exists.
    pub fn entry_point(&'a self) -> Result<Option<&'a Function<'a>>> {
        match self.source.entry_point.0 {
            Some(entry_point) => self.load_function_definition_raw(entry_point).map(Some),
            None => Ok(None),
        }
    }

    /// Searches for a function defined in this module corresponding to the given symbol.
    pub fn lookup_function(&'a self, symbol: Symbol<'a>) -> Option<&'a Function<'a>> {
        match self.function_lookup_cache.borrow_mut().entry(symbol) {
            hash_map::Entry::Vacant(vacant) => {
                let mut index = 0u32;
                for definition in self.source.definitions.0.defined_functions.iter() {
                    if let Some(actual_symbol) = self
                        .load_identifier_raw(definition.symbol)
                        .ok()
                        .filter(|s| *s == vacant.key().as_ref())
                    {
                        let loaded = self
                            .loaded_functions
                            .insert_or_get::<(), _>(
                                format::indices::FunctionDefinition::from(index),
                                |_| Ok(Function::new(self, &definition)),
                            )
                            .unwrap();

                        return Some(*vacant.insert(loaded) as &'a _);
                    }

                    index += 1;
                }
                None
            }
            hash_map::Entry::Occupied(occupied) => Some(occupied.get()),
        }
    }
}

impl<'a> std::fmt::Debug for &'a Module<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Module")
            .field("identifier", self.identifier())
            .field("format_version", &self.source.format_version)
            .field("loaded_functions", &&self.loaded_functions)
            .finish()
    }
}

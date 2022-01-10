use super::*;

pub struct Module<'a> {
    source: format::Module,
    function_signature_cache:
        cache::IndexLookup<'a, format::indices::FunctionSignature, FunctionSignature<'a>>,
    //loaded_structs
    //loaded_globals
    loaded_functions: cache::IndexLookup<'a, format::indices::FunctionDefinition, Function<'a>>,
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
        }
    }

    /// Retrieves the module's name and version.
    pub fn identifier(&'a self) -> &'a ModuleIdentifier {
        &self.source.header.0.identifier
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

    /// Retrieves the entry point for the application, if it exists.
    pub fn entry_point(&'a self) -> Result<Option<&'a Function<'a>>> {
        match self.source.entry_point.0 {
            Some(entry_point) => self.load_function_definition_raw(entry_point).map(Some),
            None => Ok(None),
        }
    }
}

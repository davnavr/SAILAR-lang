use crate::loader::{self, cache, Error, Result};
use registir::format;
use std::collections::hash_map;

pub struct Module<'a> {
    source: format::Module,
    function_signature_cache:
        cache::IndexLookup<'a, format::indices::FunctionSignature, loader::FunctionSignature<'a>>,
    type_signature_cache:
        cache::IndexLookup<'a, format::indices::TypeSignature, loader::TypeSignature<'a>>,
    loaded_structs: cache::IndexLookup<'a, format::indices::StructDefinition, loader::Struct<'a>>,
    //loaded_globals
    loaded_fields: cache::IndexLookup<'a, format::indices::FieldDefinition, loader::Field<'a>>,
    loaded_functions:
        cache::IndexLookup<'a, format::indices::FunctionDefinition, loader::Function<'a>>,
    function_lookup_cache:
        std::cell::RefCell<hash_map::HashMap<loader::Symbol<'a>, &'a loader::Function<'a>>>,
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
        loader::read_index(index, |raw_index| {
            constructor(loader(raw_index)?.ok_or_else(|| Error::IndexOutOfBounds(index.into()))?)
        })
    })
}

impl<'a> Module<'a> {
    pub(crate) fn new(source: format::Module) -> Self {
        Self {
            source,
            function_signature_cache: cache::IndexLookup::new(),
            type_signature_cache: cache::IndexLookup::new(),
            loaded_structs: cache::IndexLookup::new(),
            loaded_fields: cache::IndexLookup::new(),
            loaded_functions: cache::IndexLookup::new(),
            // TODO: Could construct the function_lookup_cache IF the module is known to not have an entry point.
            function_lookup_cache: std::cell::RefCell::new(hash_map::HashMap::new()),
        }
    }

    /// Retrieves the module's name and version.
    pub fn identifier(&'a self) -> &'a loader::ModuleIdentifier {
        &self.source.header.0.identifier
    }

    pub fn full_symbol(&'a self) -> loader::ModuleSymbol<'a> {
        loader::ModuleSymbol::Borrowed(self.identifier())
    }

    pub fn load_identifier_raw(
        &'a self,
        index: format::indices::Identifier,
    ) -> Result<&'a loader::Identifier> {
        loader::read_index_from(index, &self.source.identifiers, Ok)
    }

    pub fn load_type_signature_raw(
        &'a self,
        index: format::indices::TypeSignature,
    ) -> Result<&'a format::TypeSignature> {
        loader::read_index_from(index, &self.source.type_signatures, Ok)
    }

    pub fn load_type_signature(
        &'a self,
        index: format::indices::TypeSignature,
    ) -> Result<&'a loader::TypeSignature<'a>> {
        load_raw_cached(
            &self.type_signature_cache,
            index,
            |_| self.load_type_signature_raw(index).map(Some),
            |source| Ok(loader::TypeSignature::new(self, source)),
        )
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
        loader::read_index_from(index, &self.source.function_signatures, Ok)
    }

    pub fn load_function_signature(
        &'a self,
        index: format::indices::FunctionSignature,
    ) -> Result<&'a loader::FunctionSignature<'a>> {
        load_raw_cached(
            &self.function_signature_cache,
            index,
            |_| self.load_function_signature_raw(index).map(Some),
            |signature| {
                Ok(loader::FunctionSignature::new(
                    self.collect_type_signatures_raw(&signature.return_types)?,
                    self.collect_type_signatures_raw(&signature.parameter_types)?,
                ))
            },
        )
    }

    pub fn load_code_raw(&'a self, index: format::indices::Code) -> Result<&'a format::Code> {
        loader::read_index_from(index, &self.source.function_bodies, Ok)
    }

    pub fn load_struct_definition_raw(
        &'a self,
        index: format::indices::StructDefinition,
    ) -> Result<&'a loader::Struct<'a>> {
        load_raw_cached(
            &self.loaded_structs,
            index,
            |raw_index| {
                let structs: &[_] = &self.source.definitions.defined_structs;
                Ok(structs.get(raw_index))
            },
            |source| Ok(loader::Struct::new(self, source)),
        )
    }

    pub fn load_function_definition_raw(
        &'a self,
        index: format::indices::FunctionDefinition,
    ) -> Result<&'a loader::Function<'a>> {
        load_raw_cached(
            &self.loaded_functions,
            index,
            |raw_index| {
                let functions: &[_] = &self.source.definitions.defined_functions;
                Ok(functions.get(raw_index))
            },
            |source| Ok(loader::Function::new(self, source)),
        )
    }

    pub fn load_function_raw(
        &'a self,
        index: format::indices::Function,
    ) -> Result<&'a loader::Function<'a>> {
        match index {
            format::indices::Function::Defined(defined_index) => {
                self.load_function_definition_raw(defined_index)
            }
            format::indices::Function::Imported(_) => {
                todo!("resolution of function imports is not yet supported")
            }
        }
    }

    pub fn load_field_definition_raw(
        &'a self,
        index: format::indices::FieldDefinition,
    ) -> Result<&'a loader::Field<'a>> {
        load_raw_cached(
            &self.loaded_fields,
            index,
            |raw_index| {
                let fields: &[_] = &self.source.definitions.defined_fields;
                Ok(fields.get(raw_index))
            },
            |source| {
                Ok(loader::Field::new(
                    self.load_struct_definition_raw(source.owner)?,
                    source,
                ))
            },
        )
    }

    /// Retrieves the entry point for the application, if it exists.
    pub fn entry_point(&'a self) -> Result<Option<&'a loader::Function<'a>>> {
        match self.source.entry_point.0 {
            Some(entry_point) => self.load_function_definition_raw(entry_point).map(Some),
            None => Ok(None),
        }
    }

    /// Searches for a function defined in this module corresponding to the given symbol.
    pub fn lookup_function(
        &'a self,
        symbol: loader::Symbol<'a>,
    ) -> Option<&'a loader::Function<'a>> {
        match self.function_lookup_cache.borrow_mut().entry(symbol) {
            hash_map::Entry::Vacant(vacant) => {
                let function_definitions: &[_] = &*self.source.definitions.0.defined_functions.0;
                for (definition, index) in function_definitions.iter().zip(0u32..) {
                    match self.load_identifier_raw(definition.symbol) {
                        Ok(actual_symbol) if actual_symbol == vacant.key().as_ref() => {
                            let loaded = self
                                .loaded_functions
                                .insert_or_get::<(), _>(
                                    format::indices::FunctionDefinition::from(index),
                                    |_| Ok(loader::Function::new(self, definition)),
                                )
                                .unwrap();

                            return Some(*vacant.insert(loaded) as &'a _);
                        }
                        _ => continue,
                    }
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
            .field("loaded_functions", &&self.loaded_structs)
            .field("loaded_functions", &&self.loaded_functions)
            .finish()
    }
}

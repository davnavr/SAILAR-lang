use crate::loader::{self, cache, Error, Result};
use sailar::{format, hashing::IntegerHashBuilder};
use std::cell::RefCell;
use std::collections::hash_map;

pub struct Module<'a> {
    loader: &'a loader::Loader<'a>,
    source: format::Module,
    function_signature_cache:
        cache::IndexLookup<'a, format::indices::FunctionSignature, loader::FunctionSignature<'a>>,
    type_signature_cache:
        cache::IndexLookup<'a, format::indices::TypeSignature, loader::TypeSignature<'a>>,
    code_cache: cache::IndexLookup<'a, format::indices::Code, loader::Code<'a>>,
    loaded_structs: cache::IndexLookup<'a, format::indices::StructDefinition, loader::Struct<'a>>,
    //loaded_globals
    loaded_functions:
        cache::IndexLookup<'a, format::indices::FunctionDefinition, loader::Function<'a>>,
    function_lookup_cache: RefCell<hash_map::HashMap<loader::Symbol<'a>, &'a loader::Function<'a>>>,
    module_import_cache:
        RefCell<hash_map::HashMap<format::indices::Module, &'a Module<'a>, IntegerHashBuilder>>,
    function_import_cache: RefCell<
        hash_map::HashMap<
            format::indices::FunctionImport,
            &'a loader::Function<'a>,
            IntegerHashBuilder,
        >,
    >,
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
    pub(super) fn new(loader: &'a loader::Loader<'a>, source: format::Module) -> Self {
        Self {
            source,
            loader,
            function_signature_cache: cache::IndexLookup::new(),
            type_signature_cache: cache::IndexLookup::new(),
            code_cache: cache::IndexLookup::new(),
            loaded_structs: cache::IndexLookup::new(),
            loaded_functions: cache::IndexLookup::new(),
            // TODO: Could construct the function_lookup_cache IF the module is known to not have an entry point.
            function_lookup_cache: RefCell::new(hash_map::HashMap::new()),
            module_import_cache: RefCell::new(hash_map::HashMap::with_hasher(
                IntegerHashBuilder::default(),
            )),
            function_import_cache: RefCell::new(hash_map::HashMap::with_hasher(
                IntegerHashBuilder::default(),
            )),
        }
    }

    pub fn loader(&'a self) -> &'a loader::Loader<'a> {
        self.loader
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
            |source| Ok(loader::TypeSignature::new(self, source, index)),
        )
    }

    fn collect_type_signatures(
        &'a self,
        indices: &'a [format::indices::TypeSignature],
    ) -> Result<Vec<&'a loader::TypeSignature<'a>>> {
        let mut types = Vec::with_capacity(indices.len());
        for index in indices {
            types.push(self.load_type_signature(*index)?);
        }
        Ok(types)
    }

    // TODO: Rename to load_function_signature_source
    pub fn load_function_signature_raw(
        &'a self,
        index: format::indices::FunctionSignature,
    ) -> Result<&'a format::FunctionSignature> {
        loader::read_index_from(index, &self.source.function_signatures, Ok)
    }

    // TODO: Rename to load_function_signature_raw
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
                    self.collect_type_signatures(&signature.return_types)?,
                    self.collect_type_signatures(&signature.parameter_types)?,
                ))
            },
        )
    }

    pub fn load_code_source(&'a self, index: format::indices::Code) -> Result<&'a format::Code> {
        loader::read_index_from(index, &self.source.function_bodies, Ok)
    }

    pub fn load_code_raw(&'a self, index: format::indices::Code) -> Result<&'a loader::Code<'a>> {
        load_raw_cached(
            &self.code_cache,
            index,
            |_| self.load_code_source(index).map(Some),
            |code| Ok(loader::Code::new(self, code)),
        )
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
            |source| Ok(loader::Struct::new(self, index, source)),
        )
    }

    pub fn load_struct_raw(
        &'a self,
        index: format::indices::Struct,
    ) -> Result<&'a loader::Struct<'a>> {
        match index {
            format::indices::Struct::Defined(defined_index) => {
                self.load_struct_definition_raw(defined_index)
            }
            format::indices::Struct::Imported(_) => {
                todo!("resolution of struct imports is not yet supported")
            }
        }
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
            |source| Ok(loader::Function::new(self, source, index)),
        )
    }

    pub fn load_function_import_raw(
        &'a self,
        index: format::indices::FunctionImport,
    ) -> Result<&'a loader::Function<'a>> {
        match self.function_import_cache.borrow_mut().entry(index) {
            hash_map::Entry::Occupied(occupied) => Ok(*occupied.get()),
            hash_map::Entry::Vacant(vacant) => loader::read_index_from(
                index,
                &self.source.imports.0.imported_functions.0,
                |import| {
                    let owning_module = self.load_module_raw(import.module)?;
                    let symbol = self.load_identifier_raw(import.symbol)?;
                    let function = owning_module
                        .lookup_function(loader::Symbol::Borrowed(symbol))
                        .ok_or_else(|| Error::ImportNotFound {
                            index: index.0,
                            symbol: symbol.clone(),
                        })?;

                    let expected_signature = self.load_function_signature(import.signature)?;

                    if expected_signature != function.signature()? {
                        return Err(Error::from(loader::FunctionImportSignatureMismatch {
                            symbol: symbol.clone(),
                            import: index,
                            import_signature: expected_signature.into_raw(),
                            importing_module: self.identifier().clone(),
                            definition: function.index(),
                            definition_signature: function.signature()?.into_raw(),
                            defining_module: function.declaring_module().identifier().clone(),
                        }));
                    }

                    Ok(*vacant.insert(function))
                },
            ),
        }
    }

    pub fn load_function_raw(
        &'a self,
        index: format::indices::Function,
    ) -> Result<&'a loader::Function<'a>> {
        match index {
            format::indices::Function::Defined(defined_index) => {
                self.load_function_definition_raw(defined_index)
            }
            format::indices::Function::Imported(imported_index) => {
                self.load_function_import_raw(imported_index)
            }
        }
    }

    pub(super) fn load_field_definition_source(
        &'a self,
        index: format::indices::FieldDefinition,
    ) -> Result<&'a format::Field> {
        loader::read_index_from(index, &self.source.definitions.defined_fields, Ok)
    }

    pub fn load_field_raw(
        &'a self,
        index: format::indices::Field,
    ) -> Result<&'a loader::Field<'a>> {
        match index {
            format::indices::Field::Defined(defined_index) => {
                let owning_struct = self.load_struct_definition_raw(
                    self.load_field_definition_source(defined_index)?.owner,
                )?;
                // TODO: Consider storing a lookup for field references.
                owning_struct.field(defined_index)
            }
            format::indices::Field::Imported(_) => {
                todo!("resolution of field imports is not yet supported")
            }
        }
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
                            let definition_index = format::indices::FunctionDefinition::from(index);

                            let loaded = self
                                .loaded_functions
                                .insert_or_get::<(), _>(definition_index, |_| {
                                    Ok(loader::Function::new(self, definition, definition_index))
                                })
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

    pub fn load_module_raw(
        &'a self,
        index: format::indices::Module,
    ) -> Result<&'a loader::Module<'a>> {
        if index.0 .0 == 0u32 {
            Ok(self)
        } else {
            match self.module_import_cache.borrow_mut().entry(index) {
                hash_map::Entry::Vacant(vacant) => loader::read_index_from(
                    index,
                    &self.source.imports.0.imported_modules.0,
                    |import_name| Ok(*vacant.insert(self.loader.load_module(import_name)?)),
                ),
                hash_map::Entry::Occupied(occupied) => Ok(*occupied.get()),
            }
        }
    }
}

impl<'a> std::fmt::Debug for &'a Module<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Module")
            .field("identifier", self.identifier())
            .field("format_version", &self.source.format_version)
            .field("loaded_structs", &&self.loaded_structs)
            .field("loaded_functions", &&self.loaded_functions)
            .finish()
    }
}

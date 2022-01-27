use crate::loader::{self, Result};
use sailar::format;
use std::collections::hash_map;

#[derive(Default)]
struct Layout<'a> {
    fields: hash_map::HashMap<format::indices::FieldDefinition, loader::Field<'a>>,
    total_size: usize,
}

pub struct Struct<'a> {
    source: &'a format::Struct,
    index: format::indices::StructDefinition,
    module: &'a loader::Module<'a>,
    layout: loader::cache::Once<Layout<'a>>,
}

impl<'a> Struct<'a> {
    pub(super) fn new(
        module: &'a loader::Module<'a>,
        index: format::indices::StructDefinition,
        source: &'a format::Struct,
    ) -> Self {
        Self {
            source,
            index,
            module,
            layout: loader::cache::Once::default(),
        }
    }

    fn layout(&'a self) -> Result<&'a Layout<'a>> {
        self.layout.get_or_insert_fallible(|| {
            let source_fields = &self.source.fields;
            let mut fields = hash_map::HashMap::with_capacity(source_fields.len());
            let mut total_size = 0;

            for field_index in source_fields.iter().copied() {
                let field_source = self.module.load_field_definition_source(field_index)?;

                if field_source.owner != self.index {
                    return Err(loader::Error::FieldOwnerMismatch {
                        field: field_index,
                        expected: self.index,
                        actual: field_source.owner,
                    });
                }

                let field_signature = self.module.load_type_signature(field_source.signature)?;

                match fields.entry(field_index) {
                    hash_map::Entry::Vacant(vacant) => {
                        vacant.insert(loader::Field::new(
                            self,
                            field_source,
                            field_signature,
                            total_size,
                        ));
                        total_size += field_signature.size()?;
                    }
                    hash_map::Entry::Occupied(_) => {
                        return Err(loader::Error::DuplicateField {
                            owner: self.index,
                            field: field_index,
                        })
                    }
                }
            }

            Ok(Layout { total_size, fields })
        })
    }

    pub fn total_size(&'a self) -> Result<usize> {
        Ok(self.layout()?.total_size)
    }

    pub fn iter_fields(
        &'a self,
    ) -> Result<impl std::iter::ExactSizeIterator<Item = &'a loader::Field<'a>> + 'a> {
        Ok(self.layout()?.fields.values())
    }

    pub fn field(
        &'a self,
        index: format::indices::FieldDefinition,
    ) -> Result<&'a loader::Field<'a>> {
        self.layout()?
            .fields
            .get(&index)
            .ok_or(loader::Error::IndexOutOfBounds(index.0))
    }

    pub fn is_export(&'a self) -> bool {
        self.source.is_export
    }

    pub fn symbol(&'a self) -> Result<&'a loader::Identifier> {
        self.module.load_identifier_raw(self.source.symbol)
    }

    pub fn declaring_module(&'a self) -> &'a loader::Module<'a> {
        self.module
    }
}

impl<'a> std::fmt::Debug for &'a Struct<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Struct")
            .field("is_export", &self.is_export())
            .field("symbol", &self.symbol().ok())
            .field("total_size", &self.total_size().ok())
            .finish()
    }
}

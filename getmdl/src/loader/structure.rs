use crate::loader::{self, Result};
use registir::format;
use std::collections::hash_map;

#[derive(Default)]
struct Layout<'a> {
    fields: hash_map::HashMap<format::indices::FieldDefinition, &'a loader::Field<'a>>,
    total_size: usize,
}

pub struct Struct<'a> {
    source: &'a format::Struct,
    module: &'a loader::Module<'a>,
    layout: std::cell::UnsafeCell<Option<Layout<'a>>>,
}

impl<'a> Struct<'a> {
    pub(super) fn new(module: &'a loader::Module<'a>, source: &'a format::Struct) -> Self {
        Self {
            source,
            module,
            layout: std::cell::UnsafeCell::new(None),
        }
    }

    fn layout(&'a self) -> Result<&'a Layout<'a>> {
        // Layout is never null.
        let layout: &'a mut Option<_> = unsafe { &mut *self.layout.get() };
        if let Some(struct_layout) = layout {
            Ok(struct_layout)
        } else {
            Ok(layout.insert({
                let source_fields = &self.source.fields;
                let mut fields = hash_map::HashMap::with_capacity(source_fields.len());
                let mut total_size = 0;

                for field_index in source_fields.iter().copied() {
                    let field = self.module.load_field_definition_raw(field_index)?;
                    total_size += field.size()?;
                    fields.insert(field_index, field);
                }

                Layout {
                    total_size,
                    fields,
                }
            }))
        }
    }

    pub fn total_size(&'a self) -> Result<usize> {
        Ok(self.layout()?.total_size)
    }

    pub fn fields(&'a self) -> Result<impl std::iter::Iterator<Item = &'a loader::Field<'a>> + 'a> {
        Ok(self.layout()?.fields.values().copied())
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
            .finish()
    }
}

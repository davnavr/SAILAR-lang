use crate::loader::{self, Result};
use registir::format;

pub struct Field<'a> {
    source: &'a format::Field,
    declaring_struct: &'a loader::Struct<'a>,
}

impl<'a> Field<'a> {
    pub(super) fn new(declaring_struct: &'a loader::Struct<'a>, source: &'a format::Field) -> Self {
        Self {
            source,
            declaring_struct,
        }
    }

    pub fn declaring_struct(&'a self) -> &'a loader::Struct<'a> {
        self.declaring_struct
    }

    pub fn declaring_module(&'a self) -> &'a loader::Module<'a> {
        self.declaring_struct.declaring_module()
    }

    pub fn is_export(&'a self) -> bool {
        self.source.is_export
    }

    pub fn symbol(&'a self) -> Result<&'a loader::Identifier> {
        self.declaring_module()
            .load_identifier_raw(self.source.symbol)
    }

    pub fn signature(&'a self) -> Result<&'a loader::TypeSignature<'a>> {
        self.declaring_module()
            .load_type_signature(self.source.signature)
    }

    pub fn size(&'a self) -> Result<usize> {
        self.signature()?.size()
    }
}

impl<'a> std::fmt::Debug for &'a Field<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Field")
            .field("is_export", &self.is_export())
            .field("symbol", &self.symbol().ok())
            .finish()
    }
}

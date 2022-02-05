use crate::loader;
use sailar::format;

pub struct Data<'a> {
    source: &'a format::DataArray,
    module: &'a loader::Module<'a>,
}

impl<'a> Data<'a> {
    pub(super) fn new(module: &'a loader::Module<'a>, source: &'a format::DataArray) -> Self {
        Self { source, module }
    }

    pub fn declaring_module(&'a self) -> &'a loader::Module<'a> {
        self.module
    }

    pub fn bytes(&'a self) -> &'a [u8] {
        &self.source.0 .0
    }
}

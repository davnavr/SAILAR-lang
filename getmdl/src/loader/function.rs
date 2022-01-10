use super::*;

pub struct Function<'a> {
    source: &'a format::Function,
    module: &'a Module<'a>,
}

impl<'a> Function<'a> {
    pub(crate) fn new(module: &'a Module<'a>, source: &'a format::Function) -> Self {
        Self { source, module }
    }

    pub fn symbol(&'a self) -> Result<Option<&'a Identifier>> {
        match self.source.symbol {
            Some(symbol) => self.module.load_identifier_raw(symbol).map(Some),
            None => Ok(None),
        }
    }

    pub fn declaring_module(&'a self) -> &'a Module<'a> {
        self.module
    }
}

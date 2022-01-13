use super::{format, Identifier, ModuleIdentifier};
use std::borrow::Cow;

pub type ModuleSymbol<'a> = Cow<'a, ModuleIdentifier>;

pub type Symbol<'a> = Cow<'a, Identifier>;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Function<'a> {
    module: ModuleSymbol<'a>,
    symbol: Symbol<'a>,
}

impl<'a> Function<'a> {
    pub fn new(module: ModuleSymbol<'a>, symbol: Symbol<'a>) -> Self {
        Self { module, symbol }
    }

    pub fn module(&self) -> &ModuleIdentifier {
        &self.module
    }

    pub fn symbol(&self) -> &Identifier {
        &self.symbol
    }
}

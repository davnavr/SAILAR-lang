use super::{Identifier, ModuleIdentifier};
use std::borrow::Cow;

pub type Module<'a> = Cow<'a, ModuleIdentifier>;

pub type Symbol<'a> = Cow<'a, Identifier>;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Function<'a> {
    module: Module<'a>,
    symbol: Symbol<'a>,
}

impl<'a> Function<'a> {
    pub fn new(module: Module<'a>, symbol: Symbol<'a>) -> Self {
        Self { module, symbol }
    }

    pub fn module(&self) -> &ModuleIdentifier {
        &self.module
    }

    pub fn symbol(&self) -> &Identifier {
        &self.symbol
    }

    pub fn to_owned<'b>(&self) -> Function<'b> {
        Function {
            module: Cow::Owned(self.module.clone().into_owned()),
            symbol: Cow::Owned(self.symbol.clone().into_owned()),
        }
    }
}

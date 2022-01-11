use super::{format, Identifier, ModuleIdentifier};

//pub enum AASymbol<'a> {
//    Owned(Identifier),
//    Borrowed(&'a Identifier)
//}

#[derive(Debug, Eq, PartialEq)]
pub enum SymbolKind<P, F> {
    Partial(P),
    Full(F),
}

pub trait Symbol: Sized {
    type Partial: Sized;

    fn print(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;

    //fn parse
}

// impl Symbol for ModuleIdentifier {
//     type Partial = Identifier;

// }

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Function {
    module: ModuleIdentifier,
    symbol: Identifier,
}

impl Function {
    pub fn new(module: ModuleIdentifier, symbol: Identifier) -> Self {
        Self { module, symbol }
    }

    pub fn module(&self) -> &ModuleIdentifier {
        &self.module
    }

    pub fn symbol(&self) -> &Identifier {
        &self.symbol
    }
}

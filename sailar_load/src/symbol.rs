//! Module for interacting with SAILAR module symbols.

use sailar::identifier::Id;
use sailar::record;
use std::borrow::Borrow;
use std::cmp::{Eq, PartialEq};
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};

#[macro_export]
#[doc(hidden)]
macro_rules! symbol_wrapper {
    ($vis:vis struct $name:ident($contained: ty)) => {
        #[derive(Clone, Debug)]
        #[repr(transparent)]
        $vis struct $name(std::sync::Arc<$contained>);

        impl $name {
            pub fn new(definition: std::sync::Arc<$contained>) -> Option<Self> {
                match definition.export() {
                    record::Export::Private(_) | record::Export::Export(_) => Some(Self(definition)),
                    record::Export::Hidden => None,
                }
            }

            pub fn definition(&self) -> &std::sync::Arc<$contained> {
                &self.0
            }

            pub fn is_private(&self) -> bool {
                match self.0.export() {
                    record::Export::Private(_) => true,
                    record::Export::Export(_) => false,
                    record::Export::Hidden => unreachable!(),
                }
            }

            pub fn module(&self) -> &std::sync::Weak<crate::module::Module> {
                self.0.module()
            }
        }
    };
}

#[derive(Clone)]
#[non_exhaustive]
pub enum Symbol {
    Function(crate::function::Symbol),
}

impl Symbol {
    pub fn export(&self) -> &record::Export<'static> {
        match self {
            Self::Function(f) => f.definition().export(),
        }
    }

    pub fn name(&self) -> &Id {
        self.export().symbol().unwrap()
    }

    pub fn module(&self) -> &std::sync::Weak<crate::module::Module> {
        match self {
            Self::Function(f) => f.module(),
        }
    }

    pub fn is_private(&self) -> bool {
        match self.export() {
            record::Export::Export(_) => false,
            record::Export::Private(_) => true,
            record::Export::Hidden => unreachable!("symbols should not refer to definitions that are hidden"),
        }
    }
}

macro_rules! symbol_from_impl {
    ($case_name: ident, $source: ty) => {
        impl From<$source> for Symbol {
            fn from(symbol: $source) -> Self {
                Self::$case_name(symbol)
            }
        }
    };
}

symbol_from_impl!(Function, crate::function::Symbol);

impl Debug for Symbol {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_tuple("Symbol").field(&self.name()).finish()
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name() && crate::module::module_weak_eq(self.module(), other.module())
    }
}

impl Eq for Symbol {}

impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name().hash(state)
    }
}

impl Borrow<Id> for Symbol {
    fn borrow(&self) -> &Id {
        self.name()
    }
}

#[derive(Debug)]
pub struct DuplicateSymbolError {
    symbol: Symbol,
}

impl DuplicateSymbolError {
    pub(crate) fn new(symbol: Symbol) -> Self {
        Self { symbol }
    }

    pub fn symbol(&self) -> &Symbol {
        &self.symbol
    }
}

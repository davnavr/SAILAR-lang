//! Module for interacting with SAILAR module symbols.

use crate::module::Export;
use sailar::identifier::Id;
use std::borrow::Borrow;
use std::cmp::{Eq, PartialEq};
use std::collections::hash_map;
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
                if definition.export().kind() == sailar::record::ExportKind::Hidden {
                    None
                } else {
                    Some(Self(definition))
                }
            }
        }

        impl std::ops::Deref for $name {
            type Target = std::sync::Arc<$contained>;

            fn deref(&self) -> &Self::Target {
                &self.0
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
    pub fn export(&self) -> &Export {
        match self {
            Self::Function(f) => f.export(),
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
        self.export().kind() == sailar::record::ExportKind::Private
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

pub struct Lookup {
    lookup: rustc_hash::FxHashMap<Symbol, ()>,
}

impl Lookup {
    pub(crate) fn new() -> Self {
        Self {
            lookup: Default::default(),
        }
    }

    pub fn get<S: ?Sized>(&self, symbol: &S) -> Option<&Symbol>
    where
        Symbol: std::borrow::Borrow<S>,
        S: std::hash::Hash + std::cmp::Eq,
    {
        self.lookup.get_key_value(symbol).map(|(k, _)| k)
    }

    pub fn iter(&self) -> impl std::iter::ExactSizeIterator<Item = &Symbol> {
        self.lookup.keys()
    }

    pub fn iter_functions(&self) -> impl std::iter::Iterator<Item = &crate::function::Symbol> {
        self.iter().map(|symbol| match symbol {
            Symbol::Function(f) => f,
        })
    }

    pub(crate) fn insert<S: Into<Symbol>>(&mut self, symbol: S) {
        match self.lookup.entry(symbol.into()) {
            hash_map::Entry::Occupied(occupied) => panic!("duplicate symbol {}", occupied.key().name()),
            hash_map::Entry::Vacant(vacant) => {
                vacant.insert(());
            }
        }
    }
}

impl Debug for Lookup {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

//! Manipulation of SAILAR function definitions and function imports.

use crate::{Id, Identifier};
use crate::reference::Orc;
use crate::type_system::Any;

/// Represents a SAILAR function signature.
#[derive(Clone, Default, Debug, Eq, Hash, PartialEq)]
pub struct Signature {
    result_types: Box<[Any]>,
    argument_types: Box<[Any]>,
}

impl Signature {
    pub fn new(result_types: Box<[Any]>, argument_types: Box<[Any]>) -> Self {
        Self {
            result_types,
            argument_types,
        }
    }

    #[inline]
    pub fn result_types(&self) -> &[Any] {
        &self.result_types
    }

    #[inline]
    pub fn argument_types(&self) -> &[Any] {
        &self.argument_types
    }
}

#[derive(Debug)]
pub struct Function {
    symbol: Identifier,
    signature: Orc<Signature>,
}

impl Function {
    pub(crate) fn new(symbol: Identifier, signature: Arc<Signature>) -> Self {
        Self {
            symbol,
            signature,
        }
    }

    #[inline]
    pub fn symbol(&self) -> &Id {
        self.symbol.as_id()
    }

    #[inline]
    pub fn signature(&self) -> &Signature {
        &self.signature
    }
}

#[derive(Debug)]
pub enum Kind {
    //Import(Orc<crate::module::Name>),
    /// Indicates that a function is a function definition, with the specified entry block.
    Defined(Orc<crate::block::Block>),
}

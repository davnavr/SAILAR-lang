//! Manipulation of SAILAR function definitions and function imports.

use crate::block;
use crate::module::Export;
use crate::type_system::Any;
use crate::{Id, Identifier};
use std::sync::Arc;

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
    signature: Arc<Signature>,
}

impl Function {
    pub(crate) fn new(symbol: Identifier, signature: Arc<Signature>) -> Self {
        Self { symbol, signature }
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

#[derive(Clone, Debug)]
pub struct ForeignBody {
    library_name: Arc<Identifier>,
    entry_point_name: Identifier,
}

impl ForeignBody {
    pub fn new(library_name: Arc<Identifier>, entry_point_name: Identifier) -> Self {
        Self {
            library_name,
            entry_point_name,
        }
    }

    /// The name of the library that the function is defined in.
    #[inline]
    pub fn library_name(&self) -> &Arc<Identifier> {
        &self.library_name
    }

    #[inline]
    pub fn entry_point_name(&self) -> &Id {
        self.entry_point_name.as_id()
    }
}

#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum Body {
    /// Indicates that a function's body is defined in its module, providing the entry block that is executed.
    Defined(Arc<block::Block>),
    /// Indicates that a function's body is defined elsewhere, used by the foreign function interface or to call functions
    /// defined in the runtime.
    Foreign(Box<ForeignBody>),
}

impl Body {
    pub(crate) fn flag(&self) -> u8 {
        match self {
            Self::Defined(_) => 0,
            Self::Foreign(_) => 0b10,
        }
    }
}

#[derive(Debug)]
pub struct Definition {
    body: Body,
    export: Export,
}

impl Definition {
    pub fn new(body: Body, export: Export) -> Self {
        Self { body, export }
    }

    #[inline]
    pub fn body(&self) -> &Body {
        &self.body
    }

    #[inline]
    pub fn is_export(&self) -> Export {
        self.export
    }
}

#[derive(Debug)]
pub enum Kind {
    //Import(Arc<crate::module::Name>),
    /// Indicates that a function is a function definition, with the specified entry block.
    Defined(Definition),
}

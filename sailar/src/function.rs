//! Manipulation of SAILAR function definitions and function imports.

use crate::type_system::Any;
use crate::reference::Ref;

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

// TODO: Only allow function instances to be made by calling a function in Module.
//pub fn add_function(&self, symbol, signature, some_argument: enum { ImportedModule, Body })
#[derive(Debug)]
pub struct Function {
    signature: Ref<Signature>,
}

impl Function {
    #[inline]
    pub fn signature(&self) -> &Signature {
        &self.signature
    }
}

pub enum Kind {
    //Import(Ref),
    Defined(Ref<crate::block::Block>),
}

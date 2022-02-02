use crate::builder;

mod function;

pub use function::{Body as FunctionBody, Builder as FunctionBuilder, Function};

pub struct Definitions {
    functions: function::Definitions,
}

impl Definitions {
    pub fn new() -> Self {
        Self {
            functions: function::Definitions::new(),
        }
    }
}

pub struct Functions<'a> {
    builder: &'a (),
    functions: &'a mut function::Definitions,
    type_signatures: &'a mut builder::type_signatures::Signatures,
}

pub struct Builder<'a> {
    builder: &'a (),
    definitions: &'a mut Definitions,
    type_signatures: &'a mut builder::type_signatures::Signatures,
}

impl<'a, 'b> Builder<'a>
where
    'a: 'b,
{
    pub(super) fn new(
        builder: &'a (),
        definitions: &'a mut Definitions,
        type_signatures: &'a mut builder::type_signatures::Signatures,
    ) -> Self {
        Self {
            builder,
            definitions,
            type_signatures,
        }
    }

    pub fn functions(&'b mut self) -> Functions<'b> {
        Functions {
            builder: self.builder,
            functions: &mut self.definitions.functions,
            type_signatures: self.type_signatures,
        }
    }
}

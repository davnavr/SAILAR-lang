use crate::loader;
use sailar::format;

pub struct Register<'a> {
    block: &'a loader::CodeBlock<'a>,
    index: format::indices::Register,
    value_type: &'a loader::TypeSignature<'a>,
}

impl<'a> Register<'a> {
    pub(crate) fn new(
        block: &'a loader::CodeBlock<'a>,
        index: format::indices::Register,
        value_type: &'a loader::TypeSignature<'a>,
    ) -> Self {
        Self {
            block,
            index,
            value_type,
        }
    }

    pub fn block(&'a self) -> &'a loader::CodeBlock<'a> {
        self.block
    }

    pub fn index(&'a self) -> format::indices::Register {
        self.index
    }

    pub fn value_type(&'a self) -> &'a loader::TypeSignature<'a> {
        self.value_type
    }

    pub fn is_temporary(&'a self) -> bool {
        match self.index {
            format::indices::Register::Temporary(_) => true,
            format::indices::Register::Input(_) => false,
        }
    }
}

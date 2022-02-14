use crate::loader::{self, cache::Once};
use sailar::format;

pub struct Register<'a> {
    block: &'a loader::CodeBlock<'a>,
    index: format::indices::Register,
    value_type: &'a loader::TypeSignature<'a>,
    input: Once<Input<'a>>,
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
            input: Once::new(),
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

    pub fn as_input(&'a self) -> Option<&'a Input<'a>> {
        // TODO: If self is the entry block, then append Callee as an input source
        match self.index {
            format::indices::Register::Temporary(_) => None,
            format::indices::Register::Input(_) => self
                .input
                .get_or_insert_fallible::<loader::Error, _>(|| {
                    let mut sources = Vec::new();

                    for block in self.block.declaring_code().all_blocks()? {
                        for target in block.jump_targets()? {
                            if std::ptr::eq(self.block, target.destination()) {
                                sources.push(InputSource::Other(target.inputs()));
                            }
                        }
                    }

                    Ok(Input { sources })
                })
                .ok(),
        }
    }
}

pub enum InputSource<'a> {
    Callee,
    Other(&'a [&'a loader::Register<'a>]),
}

pub struct Input<'a> {
    sources: Vec<InputSource<'a>>,
}

impl<'a> Input<'a> {
    pub fn sources(&'a self) -> &'a [InputSource<'a>] {
        &self.sources
    }
}

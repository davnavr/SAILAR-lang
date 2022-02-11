use crate::loader::{self, cache, Error, Result};
use sailar::format;

pub struct Block<'a> {
    source: &'a format::CodeBlock,
    index: format::indices::CodeBlock,
}

impl<'a> Block<'a> {
    fn new(source: &'a format::CodeBlock, index: format::indices::CodeBlock) -> Self {
        Self { source, index }
    }

    pub fn index(&'a self) -> format::indices::CodeBlock {
        self.index
    }

    pub fn as_raw(&'a self) -> &'a format::CodeBlock {
        self.source
    }
}

pub struct Code<'a> {
    source: &'a format::Code,
    module: &'a loader::Module<'a>,
    blocks: Vec<cache::Once<Block<'a>>>,
}

impl<'a> Code<'a> {
    pub(super) fn new(module: &'a loader::Module<'a>, source: &'a format::Code) -> Self {
        Self {
            module,
            source,
            blocks: {
                let mut blocks = Vec::new();
                blocks.resize_with(1 + source.blocks.len(), cache::Once::new);
                blocks
            },
        }
    }

    pub fn as_raw(&'a self) -> &'a format::Code {
        self.source
    }

    pub fn declaring_module(&'a self) -> &'a loader::Module<'a> {
        self.module
    }

    pub fn load_block(&'a self, index: format::indices::CodeBlock) -> Result<&'a Block<'a>> {
        loader::read_index(index, |raw_index| match self.blocks.get(raw_index) {
            Some(entry) => entry.get_or_insert_fallible(|| {
                let source = if raw_index == 0 {
                    &self.source.entry_block
                } else {
                    &self.source.blocks[raw_index - 1]
                };

                Ok(Block::new(source, index))
            }),
            None => Err(Error::IndexOutOfBounds(index.into())),
        })
    }

    pub fn entry_block(&'a self) -> &'a Block<'a> {
        self.load_block(format::indices::CodeBlock::from(0u32)).unwrap()
    }
}

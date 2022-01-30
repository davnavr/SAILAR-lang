use crate::builder::{self, Block, BuilderIdentifier};
use crate::format;
use std::fmt::{Debug, Formatter};

pub struct Definitions<'b> {
    builder: BuilderIdentifier<'b>,
    definitions: typed_arena::Arena<Code<'b>>,
    code_index: builder::counter::Cell<format::indices::Code>,
}

impl<'b> Definitions<'b> {
    pub(super) fn new(builder: BuilderIdentifier<'b>) -> Self {
        Self {
            builder,
            definitions: typed_arena::Arena::new(),
            code_index: builder::counter::Cell::new(),
        }
    }

    pub fn reserve(&'b mut self, additional: usize) {
        self.definitions.reserve_extend(additional)
    }

    pub fn define(&'b mut self, input_count: u32, result_count: u32) -> &mut Code {
        self.definitions.alloc(Code::new(
            self.builder,
            self.code_index.next(),
            input_count,
            result_count,
        ))
    }

    pub(super) fn build(&'b mut self) -> Vec<format::Code> {
        self.definitions.iter_mut().map(Code::build).collect()
    }
}

// May also need interior mutability
pub struct Code<'b> {
    builder: BuilderIdentifier<'b>,
    index: format::indices::Code,
    input_count: u32,
    result_count: u32,
    entry_block: Block<'b>,
    blocks: typed_arena::Arena<Block<'b>>,
    block_index: builder::counter::Cell<format::indices::CodeBlock>,
}

impl<'b> Code<'b> {
    fn new(
        builder: BuilderIdentifier<'b>,
        index: format::indices::Code,
        input_count: u32,
        result_count: u32,
    ) -> Self {
        Self {
            builder,
            index,
            input_count,
            result_count,
            entry_block: Block::new(
                builder,
                index,
                format::indices::CodeBlock::from(0),
                input_count,
                result_count,
            ),
            blocks: typed_arena::Arena::new(),
            block_index: builder::counter::Cell::with_start_value(1),
        }
    }

    pub fn index(&'b self) -> format::indices::Code {
        self.index
    }

    pub fn input_count(&'b self) -> u32 {
        self.input_count
    }

    pub fn result_count(&'b self) -> u32 {
        self.result_count
    }

    pub fn entry_block(&'b self) -> &'b Block<'b> {
        &self.entry_block
    }

    pub fn define_block(&'b mut self, input_count: u32) -> &'b Block {
        self.blocks.alloc(Block::new(
            self.builder,
            self.index,
            self.block_index.next(),
            input_count,
            self.result_count,
        ))
    }

    pub(super) fn build(&'b mut self) -> format::Code {
        format::Code {
            entry_block: self.entry_block.build(),
            blocks: format::LenVec(self.blocks.iter_mut().map(Block::build).collect()),
        }
    }
}

impl<'b> Debug for &'b Code<'b> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Code")
            .field("index", &self.index)
            .field("input_count", &self.input_count)
            .field("result_count", &self.result_count)
            .finish()
    }
}

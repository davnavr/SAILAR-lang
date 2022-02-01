use crate::builder::{self, block::Block};
use crate::format;

pub struct Definitions {
    definitions: typed_arena::Arena<Code>,
    code_index: builder::counter::Cell<format::indices::Code>,
}

impl Definitions {
    pub fn new() -> Self {
        Self {
            definitions: typed_arena::Arena::new(),
            code_index: builder::counter::Cell::new(),
        }
    }

    pub fn build(&mut self) -> Vec<format::Code> {
        self.definitions.iter_mut().map(Code::build).collect()
    }
}

impl<'a, 'b> builder::CodeDefinitions<'a> where 'a: 'b {
    pub fn define(&'b mut self, input_count: u32, result_count: u32) -> builder::Code<'b> {
        let code = self.definitions.definitions.alloc(Code::new(
            self.definitions.code_index.next(),
            input_count,
            result_count,
        ));

        builder::Code {
            builder: self.builder,
            code,
            type_signatures: self.type_signatures,
        }
    }

    pub fn reserve(&'b mut self, count: usize) {
        self.definitions.definitions.reserve_extend(count);
    }
}

// May also need interior mutability
pub struct Code {
    index: format::indices::Code,
    input_count: u32,
    result_count: u32,
    entry_block: Block,
    blocks: typed_arena::Arena<Block>,
    block_index: builder::counter::Cell<format::indices::CodeBlock>,
}

impl Code {
    fn new(index: format::indices::Code, input_count: u32, result_count: u32) -> Self {
        let mut block_index = builder::counter::Cell::new();
        Self {
            index,
            input_count,
            result_count,
            entry_block: Block::new(block_index.next(), input_count, result_count),
            blocks: typed_arena::Arena::new(),
            block_index,
        }
    }

    pub fn build(&mut self) -> format::Code {
        format::Code {
            entry_block: self.entry_block.build(),
            blocks: format::LenVec(self.blocks.iter_mut().map(Block::build).collect()),
        }
    }
}

impl<'a, 'b> builder::Code<'a> where 'a: 'b {
    pub fn index(&'b self) -> format::indices::Code {
        self.code.index
    }

    pub fn input_count(&'b self) -> u32 {
        self.code.input_count
    }

    pub fn result_count(&'b self) -> u32 {
        self.code.result_count
    }

    pub fn entry_block(&'b mut self) -> builder::Block<'b> {
        builder::Block::new(
            &mut self.code.entry_block,
            self.code.index,
            self.builder,
            self.type_signatures,
        )
    }

    pub fn define_block(&'b mut self, input_count: u32) -> builder::Block<'b> {
        let block = self.code.blocks.alloc(Block::new(
            self.code.block_index.next(),
            input_count,
            self.code.result_count,
        ));

        builder::Block::new(block, self.code.index, self.builder, self.type_signatures)
    }

    pub fn finish(self) -> &'a Code {
        self.code
    }
}

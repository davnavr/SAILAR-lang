use crate::{
    builder::{self, Block},
    format,
};

pub struct Definitions {
    definitions: typed_arena::Arena<Code>,
    next_code_index: u32,
}

impl Definitions {
    pub(super) fn new() -> Self {
        Self {
            definitions: typed_arena::Arena::new(),
            next_code_index: 0,
        }
    }

    pub fn reserve(&mut self, additional: usize) {
        self.definitions.reserve_extend(additional)
    }

    pub fn define(&mut self, input_count: u32, result_count: u32) -> &mut Code {
        let index = format::indices::Code::from(self.next_code_index);
        self.next_code_index += 1;
        self.definitions
            .alloc(Code::new(index, input_count, result_count))
    }
}

pub struct Code {
    index: format::indices::Code,
    input_count: u32,
    result_count: u32,
    entry_block: Block,
    blocks: typed_arena::Arena<Block>,
    block_index: builder::counter::Counter<format::indices::CodeBlock>,
}

impl Code {
    fn new(index: format::indices::Code, input_count: u32, result_count: u32) -> Self {
        Self {
            index,
            input_count,
            result_count,
            entry_block: Block::new(index, format::indices::CodeBlock::from(0), result_count),
            blocks: typed_arena::Arena::new(),
            block_index: builder::counter::Counter::with_start_value(1),
        }
    }

    pub fn input_count(&self) -> u32 {
        self.input_count
    }

    pub fn result_count(&self) -> u32 {
        self.result_count
    }

    pub fn entry_block(&mut self) -> &mut Block {
        &mut self.entry_block
    }

    pub fn define_block(&mut self) -> &mut Block {
        self.blocks.alloc(Block::new(
            self.index,
            self.block_index.next(),
            self.result_count,
        ))
    }
}

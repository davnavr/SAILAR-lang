use crate::{builder::Block, format};

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

    pub fn define(&mut self) -> &mut Code {
        let index = format::indices::Code::from(self.next_code_index);
        self.next_code_index += 1;
        self.definitions.alloc(Code::new(index))
    }
}

// Interior mutability needed, since blocks contain pointers to the code that owns them.
pub struct Code {
    index: format::indices::Code,
    entry_block: Block,
    blocks: typed_arena::Arena<Block>,
    next_block_index: u32,
}

impl Code {
    fn new(index: format::indices::Code) -> Self {
        Self {
            index,
            entry_block: Block::new(index, format::indices::CodeBlock::from(0)),
            blocks: typed_arena::Arena::new(),
            next_block_index: 1,
        }
    }

    pub fn entry_block(&mut self) -> &mut Block {
        &mut self.entry_block
    }

    pub fn define_block(&mut self) -> &mut Block {
        let block_index = format::indices::CodeBlock::from(self.next_block_index);
        self.next_block_index += 1;
        self.blocks.alloc(Block::new(self.index, block_index))
    }
}

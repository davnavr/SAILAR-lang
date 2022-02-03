use crate::builder::{self, block::Block};
use crate::format;

pub struct Definitions {
    definitions: typed_arena::Arena<Code>,
    code_index: builder::counter::Cell<format::indices::Code>,
    type_signatures: std::rc::Rc<builder::TypeSignatures>,
}

impl Definitions {
    pub fn new(type_signatures: std::rc::Rc<builder::TypeSignatures>) -> Self {
        Self {
            definitions: typed_arena::Arena::new(),
            code_index: builder::counter::Cell::new(),
            type_signatures,
        }
    }

    pub fn define(&self, input_count: u32, result_count: u32) -> &Code {
        self.definitions.alloc(Code::new(
            self.code_index.next(),
            self.type_signatures.clone(),
            input_count,
            result_count,
        ))
    }

    pub fn reserve(&self, count: usize) {
        self.definitions.reserve_extend(count);
    }

    pub(super) fn build(&mut self) -> Vec<format::Code> {
        self.definitions.iter_mut().map(Code::build).collect()
    }
}

pub struct Code {
    index: format::indices::Code,
    type_signatures: std::rc::Rc<builder::TypeSignatures>,
    input_count: u32,
    result_count: u32,
    entry_block: Block,
    blocks: typed_arena::Arena<Block>,
    block_index: builder::counter::Cell<format::indices::CodeBlock>,
}

impl Code {
    fn new(
        index: format::indices::Code,
        type_signatures: std::rc::Rc<builder::TypeSignatures>,
        input_count: u32,
        result_count: u32,
    ) -> Self {
        let block_index = builder::counter::Cell::new();
        let entry_block = Block::new(
            block_index.next(),
            type_signatures.clone(),
            input_count,
            result_count,
        );

        Self {
            index,
            type_signatures,
            input_count,
            result_count,
            entry_block,
            blocks: typed_arena::Arena::new(),
            block_index,
        }
    }

    pub fn index(&self) -> format::indices::Code {
        self.index
    }

    pub fn input_count(&self) -> u32 {
        self.input_count
    }

    pub fn result_count(&self) -> u32 {
        self.result_count
    }

    pub fn entry_block(&self) -> &Block {
        &self.entry_block
    }

    pub fn define_block(&self, input_count: u32) -> &Block {
        self.blocks.alloc(Block::new(
            self.block_index.next(),
            self.type_signatures.clone(),
            input_count,
            self.result_count,
        ))
    }

    pub(super) fn build(&mut self) -> format::Code {
        format::Code {
            entry_block: self.entry_block.build(),
            blocks: format::LenVec(self.blocks.iter_mut().map(Block::build).collect()),
        }
    }
}

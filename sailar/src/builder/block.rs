use crate::format;
use crate::format::instruction_set::{self, Instruction};

pub struct Block {
    owner_index: format::indices::Code,
    index: format::indices::CodeBlock,
    instructions: Vec<Instruction>,
}

impl Block {
    pub(super) fn new(
        owner_index: format::indices::Code,
        index: format::indices::CodeBlock,
    ) -> Self {
        Self {
            owner_index,
            index,
            instructions: Vec::new(),
        }
    }

    pub fn emit_raw(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub fn nop(&mut self) {
        self.emit_raw(Instruction::Nop);
    }
}

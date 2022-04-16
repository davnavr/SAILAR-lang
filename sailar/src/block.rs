//! Manipulation of SAILAR code blocks.

use crate::instruction_set::Instruction;

/// Allows the building of SAILAR code blocks.
#[derive(Debug)]
pub struct Builder<'b> {
    instructions: &'b mut Vec<Instruction>,
}

impl<'b> Builder<'b> {
    pub fn emit_nop(&mut self) {
        self.instructions.push(Instruction::Nop);
    }

    pub fn emit_break(&mut self) {
        self.instructions.push(Instruction::Break);
    }

    //pub fn emit_ret(self) -> Block
}

#[derive(Debug)]
pub struct BuilderCache {
    buffer: Vec<Instruction>,
}

impl BuilderCache {
    pub fn new() -> Self {
        // Builders are expected to be used to build many blocks for an application or library, so a larger initial capacity is used
        const DEFAULT_BUILDER_CAPACITY: usize = 32;

        Self {
            buffer: Vec::with_capacity(DEFAULT_BUILDER_CAPACITY),
        }
    }

    pub fn builder(&mut self) -> Builder<'_> {
        Builder {
            instructions: &mut self.buffer,
        }
    }
}

#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ValidationError {
    #[error("code blocks must not be empty")]
    EmptyBlock,
}

#[derive(Clone, Debug)]
pub struct Block {}

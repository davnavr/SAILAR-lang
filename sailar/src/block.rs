//! Manipulation of SAILAR code blocks.

use crate::instruction_set::Instruction;

#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ValidationError {
    #[error("code blocks must not be empty")]
    EmptyBlock,
}

pub type ValidationResult<T> = Result<T, ValidationError>;

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

    fn validate(self) -> ValidationResult<Block> {
        if self.instructions.is_empty() {
            return Err(ValidationError::EmptyBlock);
        }

        Ok(Block {
            instructions: self.instructions.clone().into_boxed_slice(),
        })
    }

    pub fn emit_ret<V: Into<Box<[()]>>>(self, values: V) -> ValidationResult<Block> {
        self.instructions.push(Instruction::Ret(values.into()));
        self.validate()
    }
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

impl std::default::Default for BuilderCache {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[repr(transparent)]
pub struct Block {
    instructions: Box<[Instruction]>,
}

impl Block {
    #[inline]
    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }
}

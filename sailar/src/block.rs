//! Manipulation of SAILAR code blocks.

use crate::instruction_set::{Instruction, TypedValue as _, Value};
use crate::type_system;

#[derive(Clone, Debug, thiserror::Error)]
#[error("expected result at index {index} to be of type {expected:?} but got {actual:?}")]
pub struct InvalidResultTypeError {
    index: usize,
    expected: type_system::Any,
    actual: type_system::Any,
}

#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ValidationError {
    #[error("code blocks must not be empty")]
    EmptyBlock,
    #[error(transparent)]
    InvalidResultType(#[from] InvalidResultTypeError),
    #[error("expected {expected} results but got {actual}")]
    ResultCountMismatch { expected: usize, actual: usize },
}

pub type ValidationResult<T> = Result<T, ValidationError>;

/// Allows the building of SAILAR code blocks.
#[derive(Debug)]
pub struct Builder<'b> {
    result_types: Box<[type_system::Any]>,
    input_types: Box<[type_system::Any]>,
    instructions: &'b mut Vec<Instruction>,
}

impl<'b> Builder<'b> {
    pub fn emit_nop(&mut self) {
        self.instructions.push(Instruction::Nop);
    }

    pub fn emit_break(&mut self) {
        self.instructions.push(Instruction::Break);
    }

    fn finish(self) -> ValidationResult<Block> {
        if self.instructions.is_empty() {
            return Err(ValidationError::EmptyBlock);
        }

        Ok(Block {
            result_types: self.result_types,
            input_types: self.input_types,
            instructions: self.instructions.clone().into_boxed_slice(),
        })
    }

    pub fn emit_ret<V: Into<Box<[Value]>>>(self, values: V) -> ValidationResult<Block> {
        let return_values = values.into();

        for (index, (value, expected)) in return_values.iter().zip(self.result_types.iter()).enumerate() {
            let actual = value.value_type();
            if &actual != expected {
                return Err(InvalidResultTypeError {
                    index,
                    expected: expected.clone(),
                    actual,
                }
                .into());
            }
        }

        if return_values.len() != self.result_types.len() {
            return Err(ValidationError::ResultCountMismatch {
                expected: self.result_types.len(),
                actual: return_values.len(),
            });
        }

        self.instructions.push(Instruction::Ret(return_values));
        self.finish()
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

    pub fn builder(&mut self, result_types: Box<[type_system::Any]>, input_types: Box<[type_system::Any]>) -> Builder<'_> {
        Builder {
            result_types,
            input_types,
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
pub struct Block {
    result_types: Box<[type_system::Any]>,
    input_types: Box<[type_system::Any]>,
    instructions: Box<[Instruction]>,
}

impl Block {
    #[inline]
    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    #[inline]
    pub fn result_types(&self) -> &[type_system::Any] {
        &self.result_types
    }

    #[inline]
    pub fn input_types(&self) -> &[type_system::Any] {
        &self.input_types
    }
}

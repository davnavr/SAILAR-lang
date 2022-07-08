//! Module for interacting with SAILAR code blocks.

use crate::error;
use crate::module;
use crate::type_system;
use sailar::instruction;
use sailar::record;
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, Weak};

type Record = record::CodeBlock<'static>;

/// A SAILAR instruction that has not yet been type checked.
pub type UnvalidatedInstruction = instruction::Instruction;

#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum InvalidInstructionKind {
    #[error("expected terminator instruction at end of block")]
    ExpectedTerminator,
}

#[derive(Clone)]
pub struct InvalidInstructionErrorInner {
    kind: InvalidInstructionKind,
    index: usize,
    instruction: UnvalidatedInstruction,
    code_block: Arc<Code>,
}

#[derive(Clone, thiserror::Error)]
pub struct InvalidInstructionError(Box<InvalidInstructionErrorInner>);

impl InvalidInstructionError {
    fn new<E: Into<InvalidInstructionKind>>(
        kind: E,
        index: usize,
        instruction: UnvalidatedInstruction,
        code_block: Arc<Code>,
    ) -> Self {
        Self(Box::new(InvalidInstructionErrorInner {
            kind: kind.into(),
            index,
            instruction,
            code_block,
        }))
    }

    pub fn kind(&self) -> &InvalidInstructionKind {
        &self.0.kind
    }

    pub fn index(&self) -> usize {
        self.0.index
    }

    pub fn instruction(&self) -> &UnvalidatedInstruction {
        &self.0.instruction
    }

    pub fn code_block(&self) -> &Arc<Code> {
        &self.0.code_block
    }

    pub fn into_loader_error(self) -> error::LoaderError {
        match self.code_block().module().upgrade() {
            Some(module) => error::InvalidModuleError::new(self, module).into(),
            None => error::DroppedError::new(()).into(),
        }
    }
}

impl From<InvalidInstructionError> for error::LoaderError {
    fn from(error: InvalidInstructionError) -> Self {
        error.into_loader_error()
    }
}

impl Debug for InvalidInstructionError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("InvalidInstructionError")
            .field("kind", &self.0.kind)
            .field("index", &self.0.index)
            .field("instruction", &self.0.instruction)
            .finish()
    }
}

impl std::fmt::Display for InvalidInstructionError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "instruction {:?} at index {} is invalid, {}",
            &self.0.instruction, self.0.index, &self.0.kind
        )
    }
}

/// Represents a type checked SAILAR instruction.
#[derive(Clone, Debug)]
pub enum Instruction {
    Nop,
}

impl Instruction {
    pub fn is_terminator(&self) -> bool {
        match self {
            // => true,
            _ => false,
        }
    }
}

pub struct Code {
    record: Box<Record>,
    register_types: type_system::LazySignatureList,
    instructions: lazy_init::Lazy<Result<Vec<Instruction>, error::LoaderError>>,
    module: Weak<module::Module>,
}

impl Code {
    pub(crate) fn new(record: Box<Record>, module: Weak<module::Module>) -> Arc<Self> {
        Arc::new(Self {
            record,
            register_types: Default::default(),
            instructions: Default::default(),
            module,
        })
    }

    pub fn record(&self) -> &Record {
        &self.record
    }

    pub fn module(&self) -> &Weak<module::Module> {
        &self.module
    }

    /// The types of all input registers, results, and temporary registers in that order.
    pub fn register_types(&self) -> Result<&[Arc<type_system::Signature>], error::LoaderError> {
        self.register_types
            .get_or_initialize(&self.module, self.record.register_types().iter().copied())
    }

    pub fn input_types(&self) -> Result<&[Arc<type_system::Signature>], error::LoaderError> {
        self.register_types().map(|types| &types[0..self.record.input_count()])
    }

    pub fn result_types(&self) -> Result<&[Arc<type_system::Signature>], error::LoaderError> {
        self.register_types()
            .map(|types| &types[self.record.input_count()..self.record.input_count() + self.record.result_count()])
    }

    pub fn temporary_types(&self) -> Result<&[Arc<type_system::Signature>], error::LoaderError> {
        self.register_types()
            .map(|types| &types[self.record.input_count() + self.record.result_count()..])
    }

    fn validate_body(self: &Arc<Self>) -> Result<Vec<Instruction>, error::LoaderError> {
        let module = module::Module::upgrade_weak(&self.module)?;
        let instruction_count = self.record.instructions().len();
        let mut instructions = Vec::with_capacity(instruction_count);

        for (index, op) in self.record.instructions().iter().enumerate() {
            instructions.push(match op {
                UnvalidatedInstruction::Nop => Instruction::Nop,
                bad => todo!("support {:?}", bad),
            });
        }

        match instructions.last() {
            Some(terminator) if terminator.is_terminator() => (),
            Some(_) => {
                return Err(InvalidInstructionError::new(
                    InvalidInstructionKind::ExpectedTerminator,
                    instruction_count - 1,
                    self.record.instructions().last().unwrap().clone(),
                    self.clone(),
                )
                .into())
            }
            None => todo!("error for empty block")
        }

        Ok(instructions)
    }

    pub fn instructions<'a>(self: &'a Arc<Self>) -> Result<&'a [Instruction], error::LoaderError> {
        self.instructions
            .get_or_create(|| self.validate_body())
            .as_ref()
            .map(|instructions| instructions.as_slice())
            .map_err(Clone::clone)
    }
}

impl Debug for Code {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Code")
            .field("record", &self.record)
            .field("register_types", &self.register_types)
            .finish()
    }
}

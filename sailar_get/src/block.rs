//! Types for reading the contents of code blocks.

use sailar::instruction_set;
use std::sync::{Arc, Weak};

#[derive(Debug)]
pub struct Block {
    block: Arc<sailar::block::Block>,
    module: Weak<crate::ResolvedModule>,
}

impl Block {
    #[inline]
    pub fn block(&self) -> &Arc<sailar::block::Block> {
        &self.block
    }

    pub fn instructions(&self) -> Instructions<'_> {
        Instructions {
            instructions: self.block.instructions().iter(),
            module: self
                .module
                .upgrade()
                .expect("module dropped while attempting to resolve instructions"),
        }
    }
}

pub struct Instructions<'b> {
    instructions: std::slice::Iter<'b, instruction_set::Instruction>,
    module: Arc<crate::ResolvedModule>,
}

impl<'b> std::iter::Iterator for Instructions<'b> {
    type Item = ResolvedInstruction<'b>;

    fn next(&mut self) -> Option<ResolvedInstruction<'b>> {
        use instruction_set::Instruction as Instr;

        Some(match self.instructions.next()? {
            Instr::Nop => ResolvedInstruction::Nop,
            Instr::Break => ResolvedInstruction::Break,
            Instr::Ret(values) => ResolvedInstruction::Ret(values),
            Instr::Call(call) => ResolvedInstruction::Call {
                function: self.module.get_function_instantiation(call.function()),
                arguments: call.arguments(),
            },
            Instr::AddI(op) => ResolvedInstruction::AddI(op),
            Instr::SubI(op) => ResolvedInstruction::SubI(op),
            Instr::MulI(op) => ResolvedInstruction::MulI(op),
            bad => unreachable!("unsupported instruction {:?}", bad),
        })
    }
}

// TODO: Could have special Value struct that also stores the (resolved) type of the value?

pub enum ResolvedInstruction<'b> {
    Nop,
    Break,
    Ret(&'b Box<[instruction_set::Value]>),
    Call {
        function: Arc<crate::ResolvedFunction>,
        arguments: &'b [instruction_set::Value],
    },
    AddI(&'b instruction_set::IntegerArithmetic),
    SubI(&'b instruction_set::IntegerArithmetic),
    MulI(&'b instruction_set::IntegerArithmetic),
}

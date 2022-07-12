//! Module for translating from SAILAR bytecode to LLVM IR.

use crate::compilation::Result;
use crate::helper::ptr::ArcEq;
use inkwell::basic_block::BasicBlock as LlvmBlock;
use inkwell::builder::Builder as LlvmBuilder;
use inkwell::values::FunctionValue as LlvmFunction;
use sailar_load::code_block::{Code, Instruction};
use std::collections::hash_map;
use std::sync::Arc;

/// Generates LLVM basic blocks containing translated SAILAR byte code.
pub struct Transpiler<'context> {
    context: &'context inkwell::context::Context,
    builder: LlvmBuilder<'context>,
    block_lookup: rustc_hash::FxHashMap<ArcEq<Code>, LlvmBlock<'context>>,
    undefined_blocks: Vec<(Arc<Code>, LlvmBlock<'context>)>,
}

impl<'context> Transpiler<'context> {
    pub fn new(context: &'context inkwell::context::Context) -> Self {
        Self {
            context,
            builder: context.create_builder(),
            block_lookup: Default::default(),
            undefined_blocks: Vec::new(),
        }
    }

    fn get_or_add_block(&mut self, function: LlvmFunction<'context>, code: Arc<Code>) -> LlvmBlock<'context> {
        match self.block_lookup.entry(ArcEq::from(code)) {
            hash_map::Entry::Occupied(occupied) => *occupied.get(),
            hash_map::Entry::Vacant(vacant) => {
                let block = self.context.append_basic_block(function, "");
                self.undefined_blocks.push((ArcEq::to_arc(vacant.key()), block));
                *vacant.insert(block)
            }
        }
    }

    /// Translates the contents of the specified SAILAR function, writing the LLVM IR to the specified LLVM function.
    pub fn translate(
        &mut self,
        function: Arc<sailar_load::function::Instantiation>,
        destination: LlvmFunction<'context>,
    ) -> Result<()> {
        self.block_lookup.clear();
        self.undefined_blocks.clear();

        let entry_block = match function.template()?.as_definition()?.body()? {
            sailar_load::function::Body::Defined(code) => code,
            //_ => todo!("cannot translate SAILAR function with foreign body"),
        };

        self.get_or_add_block(destination, entry_block.clone());

        while let Some((sailar_block, llvm_block)) = self.undefined_blocks.pop() {
            self.builder.position_at_end(llvm_block);
            for opcode in sailar_block.instructions()?.iter() {
                match opcode {
                    Instruction::Nop | Instruction::Break => (),
                    bad => todo!("add support for {:?}", bad),
                }
            }
        }

        Ok(())
    }
}

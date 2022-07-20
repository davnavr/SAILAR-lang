//! Module for translating from SAILAR bytecode to LLVM IR.

use crate::compilation::Result;
use crate::helper::ptr::ArcEq;
use inkwell::basic_block::BasicBlock as LlvmBlock;
use inkwell::builder::Builder as LlvmBuilder;
use inkwell::values::BasicValueEnum as LlvmBasicValue;
use inkwell::values::FunctionValue as LlvmFunction;
use sailar_load::code_block::{Code, TypedInstruction};
use sailar_load::type_system::Type;
use std::collections::hash_map;
use std::sync::Arc;

/// Generates LLVM basic blocks containing translated SAILAR byte code.
pub struct Transpiler<'cache, 'module, 'context> {
    builder: LlvmBuilder<'context>,
    type_cache: &'cache crate::signature::Cache<'module, 'context>,
    block_lookup: rustc_hash::FxHashMap<ArcEq<Code>, LlvmBlock<'context>>,
    undefined_blocks: Vec<(Arc<Code>, LlvmBlock<'context>)>,
}

impl<'cache, 'module, 'context> Transpiler<'cache, 'module, 'context> {
    pub fn new(type_cache: &'cache crate::signature::Cache<'module, 'context>) -> Self {
        Self {
            builder: type_cache.context().create_builder(),
            type_cache,
            block_lookup: Default::default(),
            undefined_blocks: Vec::new(),
        }
    }

    fn get_or_add_block(&mut self, function: LlvmFunction<'context>, code: Arc<Code>) -> LlvmBlock<'context> {
        match self.block_lookup.entry(ArcEq::from(code)) {
            hash_map::Entry::Occupied(occupied) => *occupied.get(),
            hash_map::Entry::Vacant(vacant) => {
                let block = self.type_cache.context().append_basic_block(function, "");
                self.undefined_blocks.push((ArcEq::to_arc(vacant.key()), block));
                *vacant.insert(block)
            }
        }
    }

    fn translate_value(&self, value: &sailar_load::code_block::TypedValue) -> Result<LlvmBasicValue<'context>> {
        match value.raw_value() {
            sailar::instruction::Value::Constant(sailar::instruction::Constant::Integer(constant_integer)) => {
                match value.value_type() {
                    Type::FixedInteger(integer_type) => {
                        let actual_type = self.type_cache.get_basic_type((*integer_type).into())?.into_int_type();
                        // Gets the value, which is in little endian byte order.
                        let raw_bytes: &[u8] = constant_integer;

                        if actual_type.get_bit_width() <= 64 {
                            let mut buffer = [0u8; 8];

                            // Truncation of raw_bytes may occur, but is it correct?
                            let raw_bytes = if raw_bytes.len() > 8 { &raw_bytes[0..8] } else { raw_bytes };

                            buffer[0..raw_bytes.len()].copy_from_slice(raw_bytes);

                            // TODO: Figure out when sign_extend needs to be set.
                            Ok(actual_type.const_int(u64::from_le_bytes(buffer), false).into())
                        } else {
                            todo!(
                                "how to encode integer constants of w/ a bit width of {}",
                                actual_type.get_bit_width()
                            )
                        }
                    }
                    bad => todo!("add translation for {:?}", bad),
                }
            }
            sailar::instruction::Value::IndexedRegister(_) => todo!("registers not yet supported"),
        }
    }

    /// Translates the contents of the specified SAILAR function, writing the LLVM IR to the specified LLVM function.
    pub fn translate(
        &mut self,
        function: Arc<sailar_load::function::Function>,
        destination: LlvmFunction<'context>,
    ) -> Result<()> {
        self.block_lookup.clear();
        self.undefined_blocks.clear();

        let entry_block = function.template()?.as_definition()?.entry_block()?;

        self.get_or_add_block(destination, entry_block.clone());

        while let Some((sailar_block, llvm_block)) = self.undefined_blocks.pop() {
            self.builder.position_at_end(llvm_block);
            for instruction in sailar_block.typed_instructions()?.iter() {
                match instruction {
                    TypedInstruction::Nop | TypedInstruction::Break => (),
                    TypedInstruction::Return(values) => {
                        let actual_return_value;
                        self.builder.build_return(match std::ops::Deref::deref(values) {
                            [] => None,
                            [value] => {
                                actual_return_value = self.translate_value(value)?;
                                Some(&actual_return_value)
                            }
                            _ => todo!("multiple return values not yet supported"),
                        });
                    } //bad => todo!("add support for {:?}", bad),
                }
            }
        }

        Ok(())
    }
}

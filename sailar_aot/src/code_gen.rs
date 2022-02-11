//! Translates SAILAR bytecode into LLVM bitcode.

use crate::error::Result;
use crate::ComparableRef;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use sailar::format::instruction_set as sail;
use std::cell::RefCell;
use std::collections::hash_map;

pub struct Cache<'b, 'c, 'l> {
    builder: &'b Builder<'c>,
    block_lookup: RefCell<
        hash_map::HashMap<ComparableRef<'l, sailar_get::loader::CodeBlock<'l>>, BasicBlock<'c>>,
    >,
    block_buffer: RefCell<Vec<(&'l sailar_get::loader::CodeBlock<'l>, BasicBlock<'c>)>>,
}

impl<'b, 'c, 'l> Cache<'b, 'c, 'l> {
    pub fn new(builder: &'b Builder<'c>) -> Self {
        Self {
            builder,
            block_lookup: RefCell::default(),
            block_buffer: RefCell::default(),
        }
    }
}

pub fn generate<'b, 'c, 'l>(
    context: &'c inkwell::context::Context,
    function: &'l sailar_get::loader::Function<'l>,
    value: inkwell::values::FunctionValue<'c>,
    cache: &Cache<'b, 'c, 'l>,
) -> Result<()> {
    cache.block_lookup.borrow_mut().clear();
    cache.block_buffer.borrow_mut().clear();

    let cache_block = |block: &'l sailar_get::loader::CodeBlock<'l>| -> Result<()> {
        match cache.block_lookup.borrow_mut().entry(ComparableRef(block)) {
            hash_map::Entry::Vacant(vacant) => {
                let block_name: String;
                let basic_block = context.append_basic_block(
                    value,
                    match block.index().0 .0 {
                        0 => "entry",
                        index => {
                            block_name = format!("block_{}", index - 1);
                            &block_name
                        }
                    },
                );

                vacant.insert(basic_block);
                cache.block_buffer.borrow_mut().push((block, basic_block));
                Ok(())
            }
            hash_map::Entry::Occupied(_) => unreachable!("duplicate block"),
        }
    };

    if let Some(code) = function.code()? {
        cache_block(code.entry_block())?;
        for index in 0..code.as_raw().blocks.0.len() {
            let index = sailar::format::indices::CodeBlock::try_from(index).unwrap();
            cache_block(code.load_block(index)?)?;
        }
    } else {
        todo!("cannot compile, external functions not yet supported")
    }

    for (block, code) in cache.block_buffer.borrow_mut().iter().copied() {
        cache.builder.position_at_end(code);

        for instruction in block.as_raw().instructions.0.iter() {
            match instruction {
                sail::Instruction::Nop => (),
                // sail::Instruction::ConstI(value) => {
                //     let constant = match *value {
                //         sail::IntegerConstant::S8(value) => {
                //             context.i8_type().const_int(value as u64, value < 0)
                //         }
                //         sail::IntegerConstant::U8(value) => {
                //             context.i8_type().const_int(value as u64, false)
                //         }
                //         sail::IntegerConstant::S16(value) => {
                //             context.i16_type().const_int(value as u64, value < 0)
                //         }
                //         sail::IntegerConstant::U16(value) => {
                //             context.i16_type().const_int(value as u64, false)
                //         }
                //         sail::IntegerConstant::S32(value) => {
                //             context.i32_type().const_int(value as u64, value < 0)
                //         }
                //         sail::IntegerConstant::U32(value) => {
                //             context.i32_type().const_int(value as u64, false)
                //         }
                //         sail::IntegerConstant::S64(value) => {
                //             context.i64_type().const_int(value as u64, value < 0)
                //         }
                //         sail::IntegerConstant::U64(value) => {
                //             context.i64_type().const_int(value as u64, false)
                //         }
                //     };

                //     todo!("a {:?}", constant.as_instruction());
                // }
                
                bad => todo!("add support for compiling instruction {:?}", bad),
            }
        }
    }

    Ok(())
}

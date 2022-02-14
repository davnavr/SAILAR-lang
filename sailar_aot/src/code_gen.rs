//! Translates SAILAR bytecode into LLVM bitcode.

use crate::error::{Error, Result};
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
    register_map: RefCell<
        hash_map::HashMap<
            ComparableRef<'l, sailar_get::loader::Register<'l>>,
            inkwell::values::BasicValueEnum<'c>,
        >,
    >,
}

impl<'b, 'c, 'l> Cache<'b, 'c, 'l> {
    pub fn new(builder: &'b Builder<'c>) -> Self {
        Self {
            builder,
            block_lookup: RefCell::default(),
            block_buffer: RefCell::default(),
            register_map: RefCell::default(),
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
        let input_registers = block.input_registers()?;
        let temporary_registers = block.temporary_registers()?;

        {
            let mut register_lookup = cache.register_map.borrow_mut();
            register_lookup.clear();
            register_lookup.reserve(input_registers.len() + temporary_registers.len());

            // TODO: This should only be used for the entry block.
            for (input, index) in input_registers.iter().zip(0u32..) {
                match register_lookup.entry(ComparableRef(input)) {
                    hash_map::Entry::Vacant(vacant) => {
                        vacant.insert(
                            value
                                .get_nth_param(index)
                                .expect("parameter should be defined"),
                        );
                    }
                    hash_map::Entry::Occupied(_) => {
                        unreachable!("input registers should not be duplicated")
                    }
                }
            }
        }

        let mut define_temporary = {
            let mut index = 0usize;
            move |value: inkwell::values::BasicValueEnum<'c>| -> Result<()> {
                match temporary_registers.get(index) {
                    None => Err(Error::TooManyTemporariesDefined {
                        expected: temporary_registers.len(),
                    }),
                    Some(temporary) => {
                        match cache
                            .register_map
                            .borrow_mut()
                            .entry(ComparableRef(temporary))
                        {
                            hash_map::Entry::Vacant(vacant) => {
                                // TODO: Check that basic value matches type of temporary register.
                                vacant.insert(value);
                                index += 1;
                                Ok(())
                            }
                            hash_map::Entry::Occupied(_) => unreachable!(
                                "temporary registers should not have duplicate definitions"
                            ),
                        }
                    }
                }
            }
        };

        let lookup_register = |index: sailar::format::indices::Register| -> Result<inkwell::values::BasicValueEnum<'c>> {
            let register = match index {
                sailar::format::indices::Register::Input(input) => block.input_registers()?.get(usize::try_from(input).unwrap()),
                sailar::format::indices::Register::Temporary(temporary) => block.temporary_registers()?.get(usize::try_from(temporary).unwrap()),
            }.ok_or(Error::UndefinedRegister(index))?;

            cache.register_map.borrow().get(&ComparableRef(register)).ok_or(Error::UndefinedRegister(index)).map(|value| *value)
        };

        let builder = cache.builder;
        builder.position_at_end(code);

        for instruction in block.as_raw().instructions.0.iter() {
            match instruction {
                sail::Instruction::Nop => (),
                sail::Instruction::Ret(values) => {
                    let return_value;
                    builder.build_return(match values.0.first() {
                        None => None,
                        Some(return_register) if values.len() == 1 => {
                            return_value = lookup_register(*return_register)?;
                            Some(&return_value)
                        }
                        Some(_) => todo!("multiple return values are not yet supported"),
                    });
                }
                sail::Instruction::ConstI(value) => {
                    define_temporary(inkwell::values::BasicValueEnum::IntValue(match *value {
                        sail::IntegerConstant::S8(value) => {
                            context.i8_type().const_int(value as u64, value < 0)
                        }
                        sail::IntegerConstant::U8(value) => {
                            context.i8_type().const_int(value as u64, false)
                        }
                        sail::IntegerConstant::S16(value) => {
                            context.i16_type().const_int(value as u64, value < 0)
                        }
                        sail::IntegerConstant::U16(value) => {
                            context.i16_type().const_int(value as u64, false)
                        }
                        sail::IntegerConstant::S32(value) => {
                            context.i32_type().const_int(value as u64, value < 0)
                        }
                        sail::IntegerConstant::U32(value) => {
                            context.i32_type().const_int(value as u64, false)
                        }
                        sail::IntegerConstant::S64(value) => {
                            context.i64_type().const_int(value as u64, value < 0)
                        }
                        sail::IntegerConstant::U64(value) => {
                            context.i64_type().const_int(value as u64, false)
                        }
                    }))?;
                }
                sail::Instruction::Add(operation) => {
                    let x = lookup_register(operation.x)?;
                    let y = lookup_register(operation.y)?;
                    match (x, y) {
                        (
                            inkwell::values::BasicValueEnum::IntValue(x_int),
                            inkwell::values::BasicValueEnum::IntValue(y_int),
                        ) => {
                            // TODO: Handle other overflow behaviors.
                            if operation.overflow != sail::OverflowBehavior::Ignore {
                                todo!("unsupported overflow behavior {:?}", operation.overflow)
                            }

                            // TODO: Check that integer types are the same.
                            define_temporary(inkwell::values::BasicValueEnum::IntValue(
                                builder.build_int_add(x_int, y_int, ""),
                            ))?
                        }
                        (_, inkwell::values::BasicValueEnum::IntValue(_)) => {
                            return Err(Error::RegisterTypeMismatch {
                                register: operation.x,
                            })
                        }
                        _ => {
                            return Err(Error::RegisterTypeMismatch {
                                register: operation.y,
                            })
                        }
                    }
                }
                bad => todo!("add support for compiling instruction {:?}", bad),
            }
        }
    }

    Ok(())
}

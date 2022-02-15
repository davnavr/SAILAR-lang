//! Translates SAILAR bytecode into LLVM bitcode.

use crate::error::{Error, Result};
use crate::{ComparableRef, TypeLookup};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use sailar::format::instruction_set as sail;
use std::cell::RefCell;
use std::collections::hash_map;

// TODO: Could merge BlockLookup and InputLookup
type BlockLookup<'c, 'l> =
    hash_map::HashMap<ComparableRef<'l, sailar_get::loader::CodeBlock<'l>>, BasicBlock<'c>>;

type RegisterMap<'c, 'l> = hash_map::HashMap<
    ComparableRef<'l, sailar_get::loader::Register<'l>>,
    inkwell::values::BasicValueEnum<'c>,
>;

type InputLookup<'c, 'l> = hash_map::HashMap<
    ComparableRef<'l, sailar_get::loader::CodeBlock<'l>>,
    Vec<inkwell::values::PhiValue<'c>>,
>;

type InputFixups<'c, 'l> = Vec<(
    &'l sailar_get::loader::CodeBlock<'l>,
    Vec<inkwell::values::BasicValueEnum<'c>>,
)>;

pub struct Cache<'b, 'c, 'l> {
    builder: &'b Builder<'c>,
    type_lookup: &'b TypeLookup<'c, 'l>,
    block_lookup: RefCell<BlockLookup<'c, 'l>>,
    block_buffer: RefCell<Vec<(&'l sailar_get::loader::CodeBlock<'l>, BasicBlock<'c>)>>,
    register_map: RefCell<RegisterMap<'c, 'l>>,
    input_lookup: RefCell<InputLookup<'c, 'l>>,
    input_fixups: RefCell<InputFixups<'c, 'l>>,
}

impl<'b, 'c, 'l> Cache<'b, 'c, 'l> {
    pub(crate) fn new(builder: &'b Builder<'c>, type_lookup: &'b TypeLookup<'c, 'l>) -> Self {
        Self {
            builder,
            type_lookup,
            block_lookup: RefCell::default(),
            block_buffer: RefCell::default(),
            register_map: RefCell::default(),
            input_lookup: RefCell::default(),
            input_fixups: RefCell::default(),
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
    cache.input_lookup.borrow_mut().clear();
    cache.input_fixups.borrow_mut().clear();

    // TODO: Append an entry block.
    // Block placed before the "entry" block, used since LLVM does not allow usage of phi instructions in the entry block.
    // This conflicts with SAILAR, which allows branching back and passing inputs to the entry block.
    let actual_entry_block = context.append_basic_block(value, "entry");

    if let Some(code) = function.code()? {
        let cache_block = |block: &'l sailar_get::loader::CodeBlock<'l>| -> Result<BasicBlock> {
            match cache.block_lookup.borrow_mut().entry(ComparableRef(block)) {
                hash_map::Entry::Vacant(vacant) => {
                    let basic_block = context.append_basic_block(value, "");
                    vacant.insert(basic_block);
                    cache.block_buffer.borrow_mut().push((block, basic_block));
                    Ok(basic_block)
                }
                hash_map::Entry::Occupied(_) => unreachable!("duplicate block"),
            }
        };

        let entry_block = cache_block(code.entry_block())?;

        // Branch to the entry block of the function.
        cache.builder.position_at_end(actual_entry_block);
        cache.builder.build_unconditional_branch(entry_block);

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
        let builder = cache.builder;
        builder.position_at_end(code);

        {
            let mut register_lookup = cache.register_map.borrow_mut();
            register_lookup.clear();
            register_lookup.reserve(input_registers.len() + temporary_registers.len());

            let is_entry_block = block.is_entry_block();
            let mut input_register_values = Vec::new();
            let mut input_lookup = cache.input_lookup.borrow_mut();

            for (input, input_index) in input_registers.iter().zip(0u32..) {
                if let hash_map::Entry::Vacant(vacant) = register_lookup.entry(ComparableRef(input))
                {
                    let input_value =
                        builder.build_phi(cache.type_lookup.get_type(input.value_type()), "");

                    if is_entry_block {
                        input_value.add_incoming(&[(
                            &value
                                .get_nth_param(input_index)
                                .expect("parameter should be defined"),
                            actual_entry_block,
                        )]);
                    }

                    input_register_values.push(input_value);
                    vacant.insert(input_value.as_basic_value());
                } else {
                    unreachable!("input registers should not be duplicated")
                }
            }

            input_lookup.insert(ComparableRef(block), input_register_values);
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

        // TODO: Have helper to lookup faster if a Register<'l> already exists instead of having bounds checking with indices.

        let lookup_register = |index: sailar::format::indices::Register| -> Result<inkwell::values::BasicValueEnum<'c>> {
            let register = match index {
                sailar::format::indices::Register::Input(input) => block.input_registers()?.get(usize::try_from(input).unwrap()),
                sailar::format::indices::Register::Temporary(temporary) => block.temporary_registers()?.get(usize::try_from(temporary).unwrap()),
            }.ok_or(Error::UndefinedRegister(index))?;

            cache.register_map.borrow().get(&ComparableRef(register)).ok_or(Error::UndefinedRegister(index)).map(|value| *value)
        };

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

        {
            let mut input_lookup = cache.input_lookup.borrow_mut();
            let block_lookup = cache.block_lookup.borrow();

            for target in block.jump_targets()?.iter() {
                let target_input_registers = target.inputs();
                let target_input_values = input_lookup
                    .get_mut(&ComparableRef(target.destination()))
                    .expect("all blocks should have entry in input lookup");

                if let Some(input_register) = target_input_registers.first() {
                    if target_input_registers.len() > 1 {
                        todo!("multiple input values are not yet supported")
                    }

                    let input_value = lookup_register(input_register.index())?;
                    target_input_values[0].add_incoming(&[(
                        &input_value,
                        *block_lookup
                            .get(&ComparableRef(target.destination()))
                            .expect("all blocks should exist in lookup"),
                    )]);
                }
            }
        }
    }

    Ok(())
}

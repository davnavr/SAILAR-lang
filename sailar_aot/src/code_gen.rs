//! Translates SAILAR bytecode into LLVM bitcode.

use crate::error::{Error, Result};
use crate::{ComparableRef, DataLookup, TypeLookup};
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
    data_lookup: &'b DataLookup<'b, 'c, 'l>,
    block_lookup: RefCell<BlockLookup<'c, 'l>>,
    block_buffer: RefCell<Vec<(&'l sailar_get::loader::CodeBlock<'l>, BasicBlock<'c>)>>,
    register_map: RefCell<RegisterMap<'c, 'l>>,
    input_lookup: RefCell<InputLookup<'c, 'l>>,
    input_fixups: RefCell<InputFixups<'c, 'l>>,
}

impl<'b, 'c, 'l> Cache<'b, 'c, 'l> {
    pub(crate) fn new(
        builder: &'b Builder<'c>,
        type_lookup: &'b TypeLookup<'c, 'l>,
        data_lookup: &'b DataLookup<'b, 'c, 'l>,
    ) -> Self {
        Self {
            builder,
            type_lookup,
            data_lookup,
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

        let lookup_register = |register: &'l sailar_get::loader::Register<'l>| -> Result<inkwell::values::BasicValueEnum<'c>> {
            cache.register_map
                .borrow()
                .get(&ComparableRef(register))
                .ok_or_else(|| Error::UndefinedRegister(register.index()))
                .map(|value| *value)
        };

        let lookup_indexed_register = |index: sailar::format::indices::Register| -> Result<inkwell::values::BasicValueEnum<'c>> {
            lookup_register(block.register_at(index)?)
        };

        for instruction in block.as_raw().instructions.0.iter() {
            match instruction {
                sail::Instruction::Nop => (),
                sail::Instruction::Ret(values) => {
                    let return_value;
                    builder.build_return(match values.0.first() {
                        None => None,
                        Some(return_register) if values.len() == 1 => {
                            return_value = lookup_indexed_register(*return_register)?;
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
                sail::Instruction::ConvI {
                    target_type,
                    overflow,
                    operand,
                } => {
                    // TODO: Handle other overflow behaviors.
                    if *overflow != sail::OverflowBehavior::Ignore {
                        todo!("unsupported overflow behavior {:?} for `conv.i`", overflow)
                    }

                    let target_integer_type = block
                        .declaring_module()
                        .loader()
                        .native_integer_type()?
                        .convert_integer_type(*target_type);

                    let actual_target_type =
                        cache.type_lookup.get_integer_type(target_integer_type);

                    let operand_register = block.register_at(*operand)?;
                    let operand_integer_type = operand_register
                        .value_type()
                        .as_fixed_int_type()
                        .expect("todo: error for invalid operand type in conv.i");

                    let operand_value = lookup_register(operand_register)?;

                    define_temporary(if operand_integer_type == target_integer_type {
                        operand_value
                    } else if operand_integer_type.byte_size() < target_integer_type.byte_size() {
                        // The operand's type is smaller, so a sign extension or a zero extension is possible.
                        let target_is_signed = target_integer_type.is_signed();
                        if operand_integer_type.is_signed() == target_is_signed {
                            // A sign or zero extension can occur without worrying about differing signs.
                            if target_is_signed {
                                builder
                                    .build_int_s_extend(
                                        operand_value.into_int_value(),
                                        actual_target_type,
                                        "",
                                    )
                                    .into()
                            } else {
                                builder
                                    .build_int_z_extend(
                                        operand_value.into_int_value(),
                                        actual_target_type,
                                        "",
                                    )
                                    .into()
                            }
                        } else {
                            todo!("integer extensions with differing signs are not yet supported")
                        }
                    } else {
                        //trunc .. to
                        todo!("integer truncations using conv.i are not yet supported")
                    })?;
                }
                sail::Instruction::Add(operation) => {
                    let x = lookup_indexed_register(operation.x)?;
                    let y = lookup_indexed_register(operation.y)?;
                    match (x, y) {
                        (
                            inkwell::values::BasicValueEnum::IntValue(x_int),
                            inkwell::values::BasicValueEnum::IntValue(y_int),
                        ) => {
                            // TODO: Handle other overflow behaviors.
                            if operation.overflow != sail::OverflowBehavior::Ignore {
                                todo!(
                                    "unsupported overflow behavior {:?} for `add`",
                                    operation.overflow
                                )
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
                sail::Instruction::MemInit {
                    destination,
                    source,
                } => {
                    // TODO: Error handling if destination is not an address.
                    let destination_address =
                        lookup_indexed_register(*destination)?.into_pointer_value();
                    match source {
                        sail::MemoryInitializationSource::FromData(data_index) => {
                            let data = function.declaring_module().load_data_raw(*data_index)?;
                            let data_value = cache.data_lookup.get(data);
                            // TODO: Get length from data_value type instead, and use .len() on array type.
                            let data_length = context
                                .i32_type()
                                .const_int(u64::try_from(data.bytes().len()).unwrap(), false);

                            builder
                                .build_memcpy(destination_address, 1, data_value, 1, data_length)
                                .unwrap();
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

                    let input_value = lookup_register(input_register)?;
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

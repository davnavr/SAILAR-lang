//! Translates SAILAR bytecode into LLVM bitcode.

use crate::error::{Error, Result};
use crate::{ComparableRef, DataLookup, FunctionLookup, TypeLookup};
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
    function_lookup: &'b FunctionLookup<'b, 'c, 'l>,
    type_lookup: &'b TypeLookup<'c, 'l>,
    data_lookup: &'b DataLookup<'b, 'c, 'l>,
    block_lookup: RefCell<BlockLookup<'c, 'l>>,
    block_buffer: RefCell<Vec<(&'l sailar_get::loader::CodeBlock<'l>, BasicBlock<'c>)>>,
    register_map: RefCell<RegisterMap<'c, 'l>>,
    input_lookup: RefCell<InputLookup<'c, 'l>>,
    input_fixups: RefCell<InputFixups<'c, 'l>>,
    value_buffer: RefCell<Vec<inkwell::values::BasicMetadataValueEnum<'c>>>,
}

impl<'b, 'c, 'l> Cache<'b, 'c, 'l> {
    pub(crate) fn new(
        builder: &'b Builder<'c>,
        function_lookup: &'b FunctionLookup<'b, 'c, 'l>,
        type_lookup: &'b TypeLookup<'c, 'l>,
        data_lookup: &'b DataLookup<'b, 'c, 'l>,
    ) -> Self {
        Self {
            builder,
            function_lookup,
            type_lookup,
            data_lookup,
            block_lookup: RefCell::default(),
            block_buffer: RefCell::default(),
            register_map: RefCell::default(),
            input_lookup: RefCell::default(),
            input_fixups: RefCell::default(),
            value_buffer: RefCell::default(),
        }
    }
}

pub fn generate<'b, 'c, 'l>(
    context: &'c inkwell::context::Context,
    function: &'l sailar_get::loader::Function<'l>,
    value: inkwell::values::FunctionValue<'c>,
    cache: &Cache<'b, 'c, 'l>,
) -> Result<()> {
    // Block placed before the "entry" block, used since LLVM does not allow usage of phi instructions in the entry block.
    // This conflicts with SAILAR, which allows branching back and passing inputs to the entry block.
    let actual_entry_block;

    if let Some(code) = function.code()? {
        cache.block_lookup.borrow_mut().clear();
        cache.block_buffer.borrow_mut().clear();
        cache.input_lookup.borrow_mut().clear();
        cache.input_fixups.borrow_mut().clear();

        actual_entry_block = context.append_basic_block(value, "entry");

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
            let index = sailar::format::indices::CodeBlock::try_from(index + 1).unwrap();
            cache_block(code.load_block(index)?)?;
        }
    } else {
        return Ok(());
    }

    {
        let mut input_lookup = cache.input_lookup.borrow_mut();

        for (block, code) in cache.block_buffer.borrow_mut().iter().copied() {
            let mut input_register_values = Vec::new();

            let builder = cache.builder;
            builder.position_at_end(code);

            for (input, input_index) in block.input_registers()?.iter().zip(0u32..) {
                let input_value =
                    builder.build_phi(cache.type_lookup.get_type(input.value_type()), "");

                if block.is_entry_block() {
                    let argument = &value
                        .get_nth_param(input_index)
                        .expect("parameter should be defined");

                    input_value.add_incoming(&[(argument, actual_entry_block)]);
                }

                input_register_values.push(input_value);
            }

            if let hash_map::Entry::Vacant(vacant) = input_lookup.entry(ComparableRef(block)) {
                vacant.insert(input_register_values);
            } else {
                unreachable!("blocks in input lookup should not be duplicated")
            }
        }
    }

    for (block, code) in cache.block_buffer.borrow_mut().iter().copied() {
        let input_registers = block.input_registers()?;
        let temporary_registers = block.temporary_registers()?;

        {
            let mut register_lookup = cache.register_map.borrow_mut();
            register_lookup.clear();
            register_lookup.reserve(input_registers.len() + temporary_registers.len());

            let input_register_lookup = cache.input_lookup.borrow();
            let input_register_values = input_register_lookup
                .get(&ComparableRef(block))
                .expect("all blocks should have entry in input lookup");

            for (input, input_value) in input_registers.iter().zip(input_register_values) {
                if let hash_map::Entry::Vacant(vacant) = register_lookup.entry(ComparableRef(input))
                {
                    vacant.insert(input_value.as_basic_value());
                } else {
                    unreachable!("input registers should not be duplicated")
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

        let lookup_indexed_arguments = |indices: &'l [sailar::format::indices::Register]| -> Result<
            std::cell::RefMut<[inkwell::values::BasicMetadataValueEnum<'c>]>,
        > {
            let mut buffer = cache.value_buffer.borrow_mut();
            buffer.clear();
            for index in indices.iter().copied() {
                buffer.push(lookup_indexed_register(index)?.into());
            }
            Ok(std::cell::RefMut::map(buffer, |buffer| {
                buffer.as_mut_slice()
            }))
        };

        let lookup_branch_target = |target: sailar::format::indices::CodeBlock| -> Result<inkwell::basic_block::BasicBlock<'c>> {
            let target_block = block.declaring_code().load_block(target)?;
            Ok(*cache.block_lookup.borrow_mut().get(&ComparableRef(target_block)).expect("all target blocks should exist in lookup"))
        };

        let builder = cache.builder;
        builder.position_at_end(code);

        let pointer_as_unsigned_native =
            |pointer_value: inkwell::values::PointerValue<'c>| -> inkwell::values::IntValue<'c> {
                let native_type = cache
                    .type_lookup
                    .get_integer_type(sailar::format::type_system::Int::UNative);

                builder.build_ptr_to_int(pointer_value, native_type, "")
            };

        let basic_arithmetic_operation =
            |operation: &'l sail::BasicArithmeticOperation,
             allow_pointer_arithmetic: bool|
             -> Result<(inkwell::values::IntValue<'c>, inkwell::values::IntValue<'c>)> {
                let x = lookup_indexed_register(operation.x)?;
                let y = lookup_indexed_register(operation.y)?;
                let x_value;
                let y_value;

                match (x, y) {
                    (
                        inkwell::values::BasicValueEnum::IntValue(x_int),
                        inkwell::values::BasicValueEnum::IntValue(y_int),
                    ) => {
                        x_value = x_int;
                        y_value = y_int;
                    }
                    (
                        inkwell::values::BasicValueEnum::PointerValue(x_pointer),
                        inkwell::values::BasicValueEnum::IntValue(y_int),
                    ) if allow_pointer_arithmetic => {
                        x_value = pointer_as_unsigned_native(x_pointer);
                        y_value = y_int;
                    }
                    (
                        inkwell::values::BasicValueEnum::IntValue(x_int),
                        inkwell::values::BasicValueEnum::PointerValue(y_pointer),
                    ) if allow_pointer_arithmetic => {
                        x_value = x_int;
                        y_value = pointer_as_unsigned_native(y_pointer);
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

                // TODO: Convert result basck into a pointer if it was a pointer.

                // TODO: Handle other overflow behaviors.
                if operation.overflow != sail::OverflowBehavior::Ignore {
                    todo!(
                        "unsupported overflow behavior {:?} for `add`",
                        operation.overflow
                    )
                }

                // TODO: Check that integer types are the same.
                Ok((x_value, y_value))
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
                sail::Instruction::Br { target, .. } => {
                    builder.build_unconditional_branch(lookup_branch_target(*target)?);
                }
                sail::Instruction::BrIf {
                    condition,
                    true_branch,
                    false_branch,
                    ..
                } => {
                    if let inkwell::values::BasicValueEnum::IntValue(comparison) =
                        lookup_indexed_register(*condition)?
                    {
                        builder.build_conditional_branch(
                            comparison,
                            lookup_branch_target(*true_branch)?,
                            lookup_branch_target(*false_branch)?,
                        );
                    } else {
                        return Err(Error::RegisterTypeMismatch {
                            register: *condition,
                        });
                    }
                }
                sail::Instruction::Call(call) => {
                    let callee = cache
                        .function_lookup
                        .get(block.declaring_module().load_function_raw(call.function)?)?;

                    let arguments = lookup_indexed_arguments(&call.arguments.0)?;

                    if let Some(return_value) = builder
                        .build_call(callee, &arguments, "")
                        .try_as_basic_value()
                        .left()
                    {
                        define_temporary(return_value)?;
                    }
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
                    let (x_value, y_value) = basic_arithmetic_operation(operation, true)?;
                    define_temporary(builder.build_int_add(x_value, y_value, "").into())?;
                }
                sail::Instruction::Sub(operation) => {
                    let (x_value, y_value) = basic_arithmetic_operation(operation, true)?;
                    define_temporary(builder.build_int_sub(x_value, y_value, "").into())?;
                }
                sail::Instruction::Mul(operation) => {
                    let (x_value, y_value) = basic_arithmetic_operation(operation, false)?;
                    define_temporary(builder.build_int_mul(x_value, y_value, "").into())?;
                }
                sail::Instruction::Cmp {
                    x: x_index,
                    kind,
                    y: y_index,
                } => {
                    let x_register = block.register_at(*x_index)?;
                    let x = lookup_register(x_register)?;
                    let y = lookup_indexed_register(*y_index)?;
                    let comparison;

                    match (x, y) {
                        (
                            inkwell::values::BasicValueEnum::IntValue(x_int),
                            inkwell::values::BasicValueEnum::IntValue(y_int),
                        ) => {
                            if let Ok(operand_type) =
                                sailar::format::type_system::Int::try_from(x_register.value_type())
                            {
                                let predicate = match kind {
                                    sail::ComparisonKind::Equal => inkwell::IntPredicate::EQ,
                                    sail::ComparisonKind::NotEqual => inkwell::IntPredicate::NE,
                                    sail::ComparisonKind::LessThan if operand_type.is_signed() => {
                                        inkwell::IntPredicate::SLT
                                    }
                                    sail::ComparisonKind::LessThan => inkwell::IntPredicate::ULT,
                                    sail::ComparisonKind::GreaterThan
                                        if operand_type.is_signed() =>
                                    {
                                        inkwell::IntPredicate::SGT
                                    }
                                    sail::ComparisonKind::GreaterThan => inkwell::IntPredicate::UGT,
                                    sail::ComparisonKind::LessThanOrEqual
                                        if operand_type.is_signed() =>
                                    {
                                        inkwell::IntPredicate::SLE
                                    }
                                    sail::ComparisonKind::LessThanOrEqual => {
                                        inkwell::IntPredicate::ULE
                                    }
                                    sail::ComparisonKind::GreaterThanOrEqual
                                        if operand_type.is_signed() =>
                                    {
                                        inkwell::IntPredicate::SGE
                                    }
                                    sail::ComparisonKind::GreaterThanOrEqual => {
                                        inkwell::IntPredicate::UGE
                                    }
                                };

                                comparison = builder.build_int_compare(predicate, x_int, y_int, "");
                            } else {
                                unreachable!(
                                    "register with integer value should have an integer type"
                                )
                            }
                        }
                        (_, inkwell::values::BasicValueEnum::IntValue(_)) => {
                            return Err(Error::RegisterTypeMismatch { register: *x_index })
                        }
                        _ => return Err(Error::RegisterTypeMismatch { register: *y_index }),
                    }

                    define_temporary(comparison.into())?;
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
                sail::Instruction::Alloca {
                    amount,
                    element_type,
                } => {
                    let amount_value = lookup_indexed_register(*amount)?.into_int_value();
                    let allocation_type = cache.type_lookup.get_type(
                        block
                            .declaring_module()
                            .load_type_signature(*element_type)?,
                    );

                    // TODO: Zero out alloca'ed memory by default.
                    define_temporary(
                        builder
                            .build_array_alloca(allocation_type, amount_value, "")
                            .into(),
                    )?;
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

                assert_eq!(
                    target.destination().input_registers()?.len(),
                    target_input_registers.len()
                );

                for (index, input_register) in target_input_registers.iter().enumerate() {
                    let input_value = lookup_register(input_register)?;
                    target_input_values[index].add_incoming(&[(
                        &input_value,
                        *block_lookup
                            .get(&ComparableRef(block))
                            .expect("all blocks should exist in lookup"),
                    )]);
                }
            }
        }
    }

    Ok(())
}

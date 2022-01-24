use crate::assembler::{self, *};

pub type CodeBlockLookup<'a> =
    lookup::IndexedMap<format::indices::CodeBlock, &'a ast::Identifier, CodeBlockAssembler<'a>>;

pub type FunctionCodeLookup<'a> =
    lookup::IndexedMap<format::indices::Code, &'a ast::Identifier, FunctionBodyAssembler<'a>>;

#[derive(Clone)]
pub struct CodeBlockAssembler<'a> {
    location: &'a ast::Position,
    input_registers: &'a [ast::RegisterSymbol],
    instructions: &'a [ast::Statement],
}

impl<'a> CodeBlockAssembler<'a> {
    pub(crate) fn assemble(
        &self,
        errors: &mut error::Builder,
        // TODO: Have struct to handle lookup in definition or imports.
        function_lookup: &definitions::FunctionLookup<'a>,
        block_lookup: &CodeBlockLookup<'a>,
        register_lookup: &mut lookup::RegisterMap<'a>,
    ) -> format::CodeBlock {
        use format::{
            instruction_set::{self, BlockIndex, FunctionIndex, Instruction},
            type_system,
        };

        fn lookup_block<'a>(
            errors: &mut error::Builder,
            block_lookup: &CodeBlockLookup<'a>,
            block: &'a ast::LocalSymbol,
        ) -> Option<BlockIndex> {
            let target = block_lookup.get(block.identifier());
            if target.is_none() {
                errors.push_with_location(
                    ErrorKind::UndefinedBlock(block.identifier().clone()),
                    block.location().clone(),
                )
            }
            target.map(|(index, _)| BlockIndex::from(index))
        }

        fn lookup_register<'a>(
            errors: &mut error::Builder,
            register_lookup: &mut lookup::RegisterMap<'a>,
            register: &'a ast::RegisterSymbol,
        ) -> Option<format::indices::Register> {
            let index = register_lookup.get(register);
            if index.is_none() {
                errors.push_with_location(
                    ErrorKind::UndefinedRegister(register.identifier().clone()),
                    register.location().clone(),
                );
            }
            index
        }

        fn lookup_many_registers<'a>(
            errors: &mut error::Builder,
            register_lookup: &mut lookup::RegisterMap<'a>,
            registers: &'a [ast::RegisterSymbol],
        ) -> Option<format::LenVec<format::indices::Register>> {
            let mut indices = Vec::with_capacity(registers.len());
            let mut success = true;

            for name in registers {
                match lookup_register(errors, register_lookup, name) {
                    Some(index) if success => indices.push(index),
                    Some(_) => (),
                    None => success = false,
                }
            }

            if success {
                Some(format::LenVec(indices))
            } else {
                None
            }
        }

        fn basic_arithmetic_operation<'a>(
            errors: &mut error::Builder,
            register_lookup: &mut lookup::RegisterMap<'a>,
            operation: &'a ast::BasicArithmeticOperation,
        ) -> Option<instruction_set::BasicArithmeticOperation> {
            Some(instruction_set::BasicArithmeticOperation {
                x: lookup_register(errors, register_lookup, &operation.x)?,
                y: lookup_register(errors, register_lookup, &operation.y)?,
                overflow: if operation.flag_overflow {
                    instruction_set::OverflowBehavior::Flag
                } else {
                    instruction_set::OverflowBehavior::Ignore
                },
            })
        }

        fn fixed_integer_type<'a>(
            errors: &mut error::Builder,
            primitive_type: &'a ast::Positioned<ast::PrimitiveType>,
        ) -> Option<type_system::FixedInt> {
            match primitive_type.0 {
                type_system::Primitive::Int(type_system::Int::Fixed(fixed_type)) => {
                    Some(fixed_type)
                }
                invalid_type => {
                    errors.push_with_location(
                        ErrorKind::InvalidConstantIntegerType(invalid_type),
                        primitive_type.1.clone(),
                    );
                    None
                }
            }
        }

        register_lookup.clear();

        for register in self.input_registers {
            if let Err(original) = register_lookup.insert_input(register) {
                errors.push_with_location(
                    ErrorKind::DuplicateRegister {
                        name: register.identifier().clone(),
                        original,
                    },
                    register.location().clone(),
                );
            }
        }

        let mut instructions = Vec::with_capacity(self.instructions.len());

        for statement in self.instructions {
            macro_rules! try_some {
                ($i: expr) => {
                    (|| Some($i))()
                };
            }

            let expected_return_count;
            let next_instruction;
            match &statement.instruction.0 {
                ast::Instruction::Nop => {
                    expected_return_count = 0;
                    next_instruction = Some(Instruction::Nop);
                }
                ast::Instruction::Ret(registers) => {
                    expected_return_count = 0;
                    next_instruction = lookup_many_registers(errors, register_lookup, registers)
                        .map(Instruction::Ret);
                }
                ast::Instruction::Switch {
                    comparison,
                    comparison_type,
                    default_target,
                    targets,
                } => {
                    let comparison_register = lookup_register(errors, register_lookup, comparison);
                    let default_target_block = lookup_block(errors, block_lookup, default_target);
                    let mut target_lookup =
                        instruction_set::SwitchLookupTable::with_capacity(targets.len());

                    let fixed_comparison_type = fixed_integer_type(errors, comparison_type);

                    for (value, target_block) in targets {
                        let target_block_index = lookup_block(errors, block_lookup, target_block);

                        macro_rules! constant_target_value {
                            ($integer_type: ty, $constant_case: ident) => {{
                                let result = <$integer_type>::try_from(value.0).ok();
                                if result.is_none() {
                                    errors.push_with_location(
                                        ErrorKind::ConstantIntegerOutOfRange(
                                            comparison_type.0,
                                            value.0,
                                        ),
                                        value.1.clone(),
                                    );
                                }
                                result.map(instruction_set::IntegerConstant::$constant_case)
                            }};
                        }

                        try_some!({
                            let target_value = match fixed_comparison_type? {
                                type_system::FixedInt::S8 => constant_target_value!(i8, S8),
                                type_system::FixedInt::U8 => constant_target_value!(u8, U8),
                                type_system::FixedInt::S16 => constant_target_value!(i16, S16),
                                type_system::FixedInt::U16 => constant_target_value!(u16, U16),
                                type_system::FixedInt::S32 => constant_target_value!(i32, S32),
                                type_system::FixedInt::U32 => constant_target_value!(u32, U32),
                                type_system::FixedInt::S64 => constant_target_value!(i64, S64),
                                type_system::FixedInt::U64 => constant_target_value!(u64, U64),
                            };

                            if !target_lookup.insert(target_value?, target_block_index?) {
                                errors.push_with_location(
                                    ErrorKind::DuplicateSwitchBranch(value.0),
                                    value.1.clone(),
                                );
                            }
                        });
                    }

                    expected_return_count = 0;
                    next_instruction = try_some!(Instruction::Switch {
                        comparison: comparison_register?,
                        comparison_type: fixed_comparison_type?,
                        default_target: default_target_block?,
                        target_lookup
                    });
                }
                ast::Instruction::Br { target, inputs } => {
                    let target_block = lookup_block(errors, block_lookup, target);
                    let input_registers = lookup_many_registers(errors, register_lookup, inputs);
                    expected_return_count = 0;
                    next_instruction = try_some!(Instruction::Br {
                        target: target_block?,
                        input_registers: input_registers?,
                    });
                }
                ast::Instruction::BrIf {
                    condition,
                    true_branch,
                    false_branch,
                    inputs,
                } => {
                    expected_return_count = 0;
                    let condition_register = lookup_register(errors, register_lookup, condition);
                    let true_target = lookup_block(errors, block_lookup, true_branch);
                    let false_target = lookup_block(errors, block_lookup, false_branch);
                    let input_registers = lookup_many_registers(errors, register_lookup, inputs);
                    next_instruction = try_some!(Instruction::BrIf {
                        condition: condition_register?,
                        true_branch: true_target?,
                        false_branch: false_target?,
                        input_registers: input_registers?,
                    });
                }
                ast::Instruction::Call {
                    function,
                    arguments,
                } => {
                    if let Some((function_index, function)) =
                        function_lookup.get(function.identifier())
                    {
                        expected_return_count = function.return_types.len();
                        next_instruction =
                            lookup_many_registers(errors, register_lookup, arguments).map(
                                move |arguments| {
                                    Instruction::Call(instruction_set::CallInstruction {
                                        function: FunctionIndex::Defined(function_index),
                                        arguments,
                                    })
                                },
                            );
                    } else {
                        errors.push_with_location(
                            ErrorKind::UndefinedGlobal(function.identifier().clone()),
                            statement.instruction.1.clone(),
                        );
                        next_instruction = None;
                        expected_return_count = 0;
                    }
                }
                ast::Instruction::Add(operation) => {
                    expected_return_count = operation.return_count();
                    next_instruction =
                        basic_arithmetic_operation(errors, register_lookup, &operation)
                            .map(Instruction::Add);
                }
                ast::Instruction::Sub(operation) => {
                    expected_return_count = operation.return_count();
                    next_instruction =
                        basic_arithmetic_operation(errors, register_lookup, &operation)
                            .map(Instruction::Sub);
                }
                ast::Instruction::Mul(operation) => {
                    expected_return_count = operation.return_count();
                    next_instruction =
                        basic_arithmetic_operation(errors, register_lookup, &operation)
                            .map(Instruction::Mul);
                }
                ast::Instruction::ConstI(constant_type, value) => {
                    use type_system::FixedInt;

                    let fixed_constant_type = fixed_integer_type(errors, constant_type);

                    macro_rules! convert_constant {
                        ($destination_type: ty, $constant: ident) => {{
                            let result = <$destination_type>::try_from(value.0)
                                .map(instruction_set::IntegerConstant::$constant)
                                .ok();
                            if result.is_none() {
                                errors.push_with_location(
                                    ErrorKind::ConstantIntegerOutOfRange(constant_type.0, value.0),
                                    value.1.clone(),
                                )
                            }
                            result
                        }};
                    }

                    expected_return_count = 1;
                    next_instruction = try_some!({
                        Instruction::ConstI(match fixed_constant_type? {
                            FixedInt::S8 => convert_constant!(i8, S8),
                            FixedInt::U8 => convert_constant!(u8, U8),
                            FixedInt::S16 => convert_constant!(i16, S16),
                            FixedInt::U16 => convert_constant!(u16, U16),
                            FixedInt::S32 => convert_constant!(i32, S32),
                            FixedInt::U32 => convert_constant!(u32, U32),
                            FixedInt::S64 => convert_constant!(i64, S64),
                            FixedInt::U64 => convert_constant!(u64, U64),
                        }?)
                    });
                }
            }

            let return_registers = &statement.results.0;
            let actual_return_count = return_registers.len();

            if actual_return_count == expected_return_count {
                if let Some(instruction) = next_instruction {
                    instructions.push(instruction)
                }
            } else {
                errors.push_with_location(
                    ErrorKind::InvalidReturnRegisterCount {
                        expected: expected_return_count,
                        actual: actual_return_count,
                    },
                    statement.results.1.clone(),
                )
            }

            for name in &statement.results.0 {
                if let Err(error) = register_lookup.insert_temporary(name) {
                    errors.push_with_location(
                        ErrorKind::DuplicateRegister {
                            name: name.identifier().clone(),
                            original: error,
                        },
                        name.location().clone(),
                    )
                }
            }
        }

        format::CodeBlock {
            input_register_count: format::numeric::UInteger(
                self.input_registers.len().try_into().unwrap(),
            ),
            exception_handler: None,
            instructions: format::LenVecBytes::from(instructions),
        }
    }
}

pub struct FunctionBodyAssembler<'a> {
    pub location: &'a ast::Position,
    pub declarations: &'a [ast::Positioned<ast::CodeDeclaration>],
}

impl<'a> FunctionBodyAssembler<'a> {
    pub(crate) fn assemble(
        &self,
        errors: &mut error::Builder,
        function_lookup: &mut definitions::FunctionLookup<'a>,
        block_lookup: &mut CodeBlockLookup<'a>,
        register_lookup: &mut lookup::RegisterMap<'a>,
    ) -> Option<format::Code> {
        block_lookup.clear();
        let mut entry_block_symbol = None;

        for declaration in self.declarations {
            match &declaration.0 {
                ast::CodeDeclaration::Entry(symbol) => {
                    if entry_block_symbol.is_none() {
                        entry_block_symbol = Some(symbol)
                    } else {
                        errors.push_with_location(
                            ErrorKind::DuplicateDirective,
                            declaration.1.clone(),
                        )
                    }
                }
                ast::CodeDeclaration::Block {
                    name,
                    arguments: input_registers,
                    instructions,
                } => {
                    let block_name = name.identifier();
                    if let Err(existing) = block_lookup.insert(
                        block_name,
                        CodeBlockAssembler {
                            input_registers,
                            instructions,
                            location: &declaration.1,
                        },
                    ) {
                        errors.push_with_location(
                            ErrorKind::DuplicateBlock {
                                name: block_name.clone(),
                                original: existing.location.clone(),
                            },
                            declaration.1.clone(),
                        )
                    }
                }
            }
        }

        if let Some(entry_block_name) = entry_block_symbol {
            Some(format::Code {
                entry_block: block_lookup
                    .get(entry_block_name.identifier())
                    .expect("entry block should exist in lookup")
                    .1
                    .assemble(errors, function_lookup, block_lookup, register_lookup),
                blocks: format::LenVec(assembler::assemble_items(
                    &block_lookup.values(),
                    |block| {
                        Some(block.assemble(errors, function_lookup, block_lookup, register_lookup))
                    },
                )),
            })
        } else {
            errors.push_with_location(
                ErrorKind::MissingDirective("missing entry block"),
                self.location.clone(),
            );
            None
        }
    }
}

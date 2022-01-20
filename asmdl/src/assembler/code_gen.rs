use crate::assembler::{self, *};

pub type CodeBlockLookup<'a> =
    lookup::IndexedMap<u32, &'a ast::Identifier, (&'a ast::Identifier, CodeBlockAssembler<'a>)>;

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
        _block_lookup: &mut CodeBlockLookup<'a>,
        register_lookup: &mut lookup::RegisterMap<'a>,
    ) -> format::CodeBlock {
        use format::instruction_set::{self, FunctionIndex, Instruction};

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
                    use format::instruction_set::{IntegerConstant, PrimitiveType};

                    macro_rules! convert_constant {
                        ($destination_type: ty, $constant: expr) => {
                            <$destination_type>::try_from(value.0)
                                .map($constant)
                                .map_err(|_| {
                                    Error::new(
                                        ErrorKind::ConstantIntegerOutOfRange(
                                            constant_type.0,
                                            value.0,
                                        ),
                                        Some(value.1.clone()),
                                    )
                                })
                        };
                    }

                    let constant = match constant_type.0 {
                        PrimitiveType::S8 => convert_constant!(i8, IntegerConstant::S8),
                        PrimitiveType::U8 => convert_constant!(u8, IntegerConstant::U8),
                        PrimitiveType::S16 => convert_constant!(i16, IntegerConstant::S16),
                        PrimitiveType::U16 => convert_constant!(u16, IntegerConstant::U16),
                        PrimitiveType::S32 => convert_constant!(i32, IntegerConstant::S32),
                        PrimitiveType::U32 => convert_constant!(u32, IntegerConstant::U32),
                        PrimitiveType::S64 => convert_constant!(i64, IntegerConstant::S64),
                        PrimitiveType::U64 => convert_constant!(u64, IntegerConstant::U64),
                        invalid_type => Err(Error::new(
                            ErrorKind::InvalidConstantIntegerType(invalid_type),
                            Some(constant_type.1.clone()),
                        )),
                    };

                    next_instruction = match constant {
                        Ok(literal) => Some(Instruction::ConstI(literal)),
                        Err(error) => {
                            errors.push(error);
                            None
                        }
                    };
                    expected_return_count = 1;
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
                        (
                            block_name,
                            CodeBlockAssembler {
                                input_registers,
                                instructions,
                                location: &declaration.1,
                            },
                        ),
                    ) {
                        errors.push_with_location(
                            ErrorKind::DuplicateBlock {
                                name: block_name.clone(),
                                original: existing.1.location.clone(),
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
                    .swap_remove(entry_block_name.identifier())
                    .expect("entry block should exist in lookup")
                    .assemble(errors, function_lookup, block_lookup, register_lookup),
                blocks: {
                    // Block lookup is cloned, otherwise it would need to be borrowed twice.
                    let definitions: Vec<_> = block_lookup.values().clone();
                    format::LenVec(assembler::assemble_items(&definitions, |(_, block)| {
                        Some(block.assemble(errors, function_lookup, block_lookup, register_lookup))
                    }))
                },
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

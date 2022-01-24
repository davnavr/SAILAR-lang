use crate::assembler::{self, error, lookup};
use crate::ast;
use registir::format::{self, indices};

pub type CodeBlockLookup<'a> = lookup::IndexedMap<u32, &'a ast::Identifier, CodeBlockAssembler<'a>>;

pub type FunctionCodeLookup<'a> =
    lookup::IndexedMap<indices::Code, &'a ast::Identifier, FunctionBodyAssembler<'a>>;

#[derive(Copy, Clone)]
struct BlockIndexLookup<'a, 'b> {
    entry_block: &'b CodeBlockAssembler<'a>,
    lookup: &'b CodeBlockLookup<'a>,
}

impl<'a, 'b> BlockIndexLookup<'a, 'b> {
    fn get(
        &self,
        name: &'a ast::LocalSymbol,
    ) -> Option<(indices::CodeBlock, &'b CodeBlockAssembler<'a>)> {
        let id = name.identifier();
        if id == self.entry_block.symbol {
            Some((indices::CodeBlock::from(0), self.entry_block))
        } else {
            self.lookup
                .get(name.identifier())
                .map(|(index, assembler)| (indices::CodeBlock::from(index + 1), assembler))
        }
    }

    fn get_index(
        &self,
        name: &'a ast::LocalSymbol,
        errors: &mut error::Builder,
    ) -> Option<indices::CodeBlock> {
        let result = self.get(name);
        if result.is_none() {
            errors.push_with_location(
                error::Kind::UndefinedBlock(name.identifier().clone()),
                name.location().clone(),
            )
        }
        result.map(|(index, _)| index)
    }
}

#[derive(Clone)]
pub struct CodeBlockAssembler<'a> {
    location: &'a ast::Position,
    symbol: &'a ast::Identifier,
    input_registers: &'a [ast::RegisterSymbol],
    instructions: &'a [ast::Statement],
}

impl<'a> lookup::KeyedValue<&'a ast::Identifier> for CodeBlockAssembler<'a> {
    fn key(&self) -> &'a ast::Identifier {
        self.symbol
    }
}

impl<'a> CodeBlockAssembler<'a> {
    fn assemble(
        &self,
        errors: &mut error::Builder,
        // TODO: Have struct to handle lookup in definition or imports.
        function_lookup: &assembler::definitions::FunctionLookup<'a>,
        block_index_lookup: BlockIndexLookup<'a, '_>,
        register_lookup: &mut lookup::RegisterMap<'a>,
    ) -> format::CodeBlock {
        use format::{
            instruction_set::{self, Instruction},
            type_system,
        };

        fn lookup_register<'a>(
            errors: &mut error::Builder,
            register_lookup: &mut lookup::RegisterMap<'a>,
            register: &'a ast::RegisterSymbol,
        ) -> Option<indices::Register> {
            let index = register_lookup.get(register);
            if index.is_none() {
                errors.push_with_location(
                    error::Kind::UndefinedRegister(register.identifier().clone()),
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

        fn integer_type(
            errors: &mut error::Builder,
            primitive_type: &ast::Positioned<ast::PrimitiveType>,
        ) -> Option<type_system::Int> {
            match primitive_type.0 {
                type_system::Primitive::Int(integer_type) => Some(integer_type),
                invalid_type => {
                    errors.push_with_location(
                        error::Kind::InvalidIntegerType(invalid_type),
                        primitive_type.1.clone(),
                    );
                    None
                }
            }
        }

        fn fixed_integer_type(
            errors: &mut error::Builder,
            primitive_type: &ast::Positioned<ast::PrimitiveType>,
        ) -> Option<type_system::FixedInt> {
            match primitive_type.0 {
                type_system::Primitive::Int(type_system::Int::Fixed(fixed_type)) => {
                    Some(fixed_type)
                }
                invalid_type => {
                    errors.push_with_location(
                        error::Kind::InvalidConstantIntegerType(invalid_type),
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
                    error::Kind::DuplicateRegister {
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
                    let default_target_block = block_index_lookup.get_index(default_target, errors);
                    let mut target_lookup =
                        instruction_set::SwitchLookupTable::with_capacity(targets.len());

                    let fixed_comparison_type = fixed_integer_type(errors, comparison_type);

                    for (value, target_block) in targets {
                        let target_block_index = block_index_lookup.get_index(target_block, errors);

                        macro_rules! constant_target_value {
                            ($integer_type: ty, $constant_case: ident) => {{
                                let result = <$integer_type>::try_from(value.0).ok();
                                if result.is_none() {
                                    errors.push_with_location(
                                        error::Kind::ConstantIntegerOutOfRange(
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
                                    error::Kind::DuplicateSwitchBranch(value.0),
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
                    let target_block = block_index_lookup.get_index(target, errors);
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
                    let true_target = block_index_lookup.get_index(true_branch, errors);
                    let false_target = block_index_lookup.get_index(false_branch, errors);
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
                                        function: indices::Function::Defined(function_index),
                                        arguments,
                                    })
                                },
                            );
                    } else {
                        errors.push_with_location(
                            error::Kind::UndefinedGlobal(function.identifier().clone()),
                            statement.instruction.1.clone(),
                        );
                        next_instruction = None;
                        expected_return_count = 0;
                    }
                }
                ast::Instruction::Add(operation) => {
                    expected_return_count = operation.return_count();
                    next_instruction =
                        basic_arithmetic_operation(errors, register_lookup, operation)
                            .map(Instruction::Add);
                }
                ast::Instruction::Sub(operation) => {
                    expected_return_count = operation.return_count();
                    next_instruction =
                        basic_arithmetic_operation(errors, register_lookup, operation)
                            .map(Instruction::Sub);
                }
                ast::Instruction::Mul(operation) => {
                    expected_return_count = operation.return_count();
                    next_instruction =
                        basic_arithmetic_operation(errors, register_lookup, operation)
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
                                    error::Kind::ConstantIntegerOutOfRange(
                                        constant_type.0,
                                        value.0,
                                    ),
                                    value.1.clone(),
                                )
                            }
                            result
                        }};
                    }

                    expected_return_count = 1;
                    next_instruction = try_some!(Instruction::ConstI(match fixed_constant_type? {
                        FixedInt::S8 => convert_constant!(i8, S8),
                        FixedInt::U8 => convert_constant!(u8, U8),
                        FixedInt::S16 => convert_constant!(i16, S16),
                        FixedInt::U16 => convert_constant!(u16, U16),
                        FixedInt::S32 => convert_constant!(i32, S32),
                        FixedInt::U32 => convert_constant!(u32, U32),
                        FixedInt::S64 => convert_constant!(i64, S64),
                        FixedInt::U64 => convert_constant!(u64, U64),
                    }?));
                }
                ast::Instruction::ConvI {
                    target_type,
                    flag_overflow,
                    operand,
                } => {
                    let actual_target_type = integer_type(errors, target_type);
                    let operand_register = lookup_register(errors, register_lookup, operand);
                    expected_return_count = if *flag_overflow { 2 } else { 1 };
                    next_instruction = try_some!(Instruction::ConvI {
                        target_type: actual_target_type?,
                        operand: operand_register?,
                        overflow: if *flag_overflow {
                            instruction_set::OverflowBehavior::Flag
                        } else {
                            instruction_set::OverflowBehavior::Ignore
                        },
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
                    error::Kind::InvalidReturnRegisterCount {
                        expected: expected_return_count,
                        actual: actual_return_count,
                    },
                    statement.results.1.clone(),
                )
            }

            for name in &statement.results.0 {
                if let Err(error) = register_lookup.insert_temporary(name) {
                    errors.push_with_location(
                        error::Kind::DuplicateRegister {
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
        function_lookup: &mut assembler::definitions::FunctionLookup<'a>,
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
                            error::Kind::DuplicateDirective,
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
                            symbol: block_name,
                            instructions,
                            location: &declaration.1,
                        },
                    ) {
                        errors.push_with_location(
                            error::Kind::DuplicateBlock {
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
            let entry_block_symbol = entry_block_name.identifier();

            let entry_block = block_lookup
                .swap_remove(entry_block_symbol)
                .expect("entry block should exist in lookup");

            let block_index_lookup = BlockIndexLookup {
                entry_block: &entry_block,
                lookup: block_lookup,
            };

            Some(format::Code {
                entry_block: entry_block.assemble(
                    errors,
                    function_lookup,
                    block_index_lookup,
                    register_lookup,
                ),
                blocks: format::LenVec({
                    block_lookup
                        .values()
                        .iter()
                        .map(|block| {
                            block.assemble(
                                errors,
                                function_lookup,
                                block_index_lookup,
                                register_lookup,
                            )
                        })
                        .collect()
                }),
            })
        } else {
            errors.push_with_location(
                error::Kind::MissingDirective("missing entry block"),
                self.location.clone(),
            );
            None
        }
    }
}

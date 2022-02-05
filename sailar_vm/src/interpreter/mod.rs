//! Contains the code for interpreting SAILAR code, managing the call stack, etc.

use sailar::format::{indices, instruction_set, type_system};
use sailar_get::loader;

pub mod call_stack;
pub mod debugger;
pub mod error;
pub mod ffi;
pub mod mem;
pub mod register;

pub use error::{Error, ErrorKind, LoaderError};

pub use mem::stack::Stack as ValueStack;

pub use register::Register;

pub type LoadedFunction<'l> = &'l loader::Function<'l>;

type Result<T> = std::result::Result<T, ErrorKind>;

/// Refers to a block in a method body, where a value of `None` refers to the entry block.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct BlockIndex(pub Option<usize>);

impl BlockIndex {
    pub const fn entry() -> Self {
        Self(None)
    }
}

impl From<indices::CodeBlock> for BlockIndex {
    fn from(index: indices::CodeBlock) -> Self {
        if index == indices::CodeBlock::from(0u32) {
            Self::entry()
        } else {
            Self(Some(usize::try_from(index.0).unwrap() - 1))
        }
    }
}

impl std::fmt::Display for BlockIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0.map(|index| index + 1).unwrap_or(0), f)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct InstructionLocation {
    pub block_index: BlockIndex,
    pub code_index: usize,
}

pub struct Interpreter<'l> {
    /// Contains the modules with the code that is being interpreted, used when the debugger looks up methods by name.
    loader: &'l loader::Loader<'l>,
    call_stack: call_stack::Stack<'l>,
    value_stack: ValueStack,
    debugger: Option<&'l mut dyn debugger::Debugger>,
}

impl<'l> Interpreter<'l> {
    fn initialize(
        loader: &'l loader::Loader<'l>,
        call_stack_capacity: call_stack::Capacity,
        value_stack_capacity: mem::stack::Capacity,
        debugger: Option<&'l mut dyn debugger::Debugger>,
    ) -> Self {
        Self {
            loader,
            call_stack: call_stack::Stack::new(call_stack_capacity),
            value_stack: ValueStack::new(value_stack_capacity),
            debugger,
        }
    }

    pub fn call_stack(&mut self) -> &mut call_stack::Stack<'l> {
        &mut self.call_stack
    }

    pub fn loader(&self) -> &'l loader::Loader<'l> {
        self.loader
    }

    fn next_instruction(&mut self) -> Result<Option<&'l instruction_set::Instruction>> {
        match self.call_stack.peek_mut() {
            Some(current_frame) => current_frame
                .instructions
                .next_instruction()
                .map(Some)
                .ok_or(ErrorKind::UnexpectedEndOfBlock),
            None => Ok(None),
        }
    }

    fn execute_instruction(
        &mut self,
        instruction: &'l instruction_set::Instruction,
        entry_point_results: &mut Vec<Register>,
    ) -> Result<()> {
        use instruction_set::{Instruction, OverflowBehavior};

        fn collect_registers_from<'l>(
            frame: &mut call_stack::Frame<'l>,
            indices: &'l [indices::Register],
        ) -> Result<Vec<Register>> {
            let mut registers: Vec<Register> = Vec::with_capacity(indices.len());
            for index in indices.iter() {
                registers.push(*frame.registers.get(*index)?);
            }
            Ok(registers)
        }

        fn handle_value_overflow(
            frame: &mut call_stack::Frame,
            behavior: OverflowBehavior,
            overflowed: bool,
        ) -> Result<()> {
            match behavior {
                OverflowBehavior::Ignore => (),
                OverflowBehavior::Flag => {
                    frame.registers.define_temporary(Register::from(overflowed))
                }
            }
            Ok(())
        }

        type BasicArithmeticOperation =
            fn(&Register, &Register) -> std::result::Result<(Register, bool), register::Type>;

        fn basic_arithmetic_operation<'l>(
            frame: &mut call_stack::Frame<'l>,
            operation: &'l instruction_set::BasicArithmeticOperation,
            o: BasicArithmeticOperation,
        ) -> Result<()> {
            let x = frame.registers.get(operation.x)?;
            let y = frame.registers.get(operation.y)?;
            let (result, overflowed) = o(x, y).map_err(|_| ErrorKind::RegisterTypeMismatch {
                expected: x.value_type(),
                actual: y.value_type(),
            })?; // TODO: Move check for equal register types into actual operation
            frame.registers.define_temporary(result);
            handle_value_overflow(frame, operation.overflow, overflowed)
        }

        match instruction {
            Instruction::Nop => (),
            Instruction::Ret(indices) => {
                let mut popped = self.call_stack.pop()?;
                let mut results = collect_registers_from(&mut popped, indices)?;

                if popped.result_count != results.len() {
                    return Err(ErrorKind::ResultCountMismatch {
                        expected: popped.result_count,
                        actual: results.len(),
                    });
                }

                match self.call_stack.peek_mut() {
                    Some(previous_frame) => {
                        previous_frame.registers.append_temporaries(&mut results)
                    }
                    None => *entry_point_results = results,
                }
            }
            Instruction::Switch {
                comparison,
                comparison_type,
                default_target,
                target_lookup,
            } => {
                let current_frame = self.call_stack.current_mut()?;
                let comparison_register = current_frame.registers.get(*comparison)?;

                match comparison_register.value_type() {
                    register::Type::Primitive(type_system::Primitive::Int(
                        type_system::Int::Fixed(comparison_register_type),
                    )) if &comparison_register_type == comparison_type => (),
                    comparison_register_type => {
                        return Err(ErrorKind::RegisterTypeMismatch {
                            actual: comparison_register_type,
                            expected: register::Type::from(*comparison_type),
                        })
                    }
                }

                let comparison_value =
                    instruction_set::IntegerConstant::try_from(comparison_register)
                        .expect("TODO: handle error when comparison register type is invalid");

                // TODO: Allow inputs to target branch.
                self.call_stack.current_jump_to(
                    target_lookup
                        .get(&comparison_value)
                        .unwrap_or(*default_target)
                        .into(),
                    &[],
                )?;
            }
            Instruction::Br {
                target,
                input_registers: input_register_indices,
            } => {
                let inputs =
                    collect_registers_from(self.call_stack.current_mut()?, input_register_indices)?;

                self.call_stack
                    .current_jump_to(BlockIndex::from(*target), &inputs)?
            }
            Instruction::BrIf {
                condition,
                true_branch: true_target,
                false_branch: false_target,
                input_registers: input_register_indices,
            } => {
                let current = self.call_stack.current_mut()?;
                let inputs = collect_registers_from(current, input_register_indices)?;

                let target = if current.registers.get(*condition)?.is_truthy() {
                    *true_target
                } else {
                    *false_target
                };

                self.call_stack
                    .current_jump_to(BlockIndex::from(target), &inputs)?
            }
            Instruction::Call(call) => {
                // TODO: Move creation of argument vector into call_stack.push
                let current_frame = self.call_stack.current()?;

                let callee = current_frame
                    .function()
                    .declaring_module()
                    .load_function_raw(call.function)?;

                // TODO: Validate signature of callee.
                // Parameter count is checked when callee is pushed onto call stack.
                let mut arguments: Vec<Register> = Vec::with_capacity(call.arguments.0.len());

                for index in &call.arguments.0 {
                    //match signture.parameter_types().get(i) {
                    arguments.push(*current_frame.registers.get(*index)?);
                }

                self.call_stack.push(callee, &arguments)?;
            }
            Instruction::Add(operation) => basic_arithmetic_operation(
                self.call_stack.current_mut()?,
                operation,
                Register::overflowing_add,
            )?,
            Instruction::Sub(operation) => basic_arithmetic_operation(
                self.call_stack.current_mut()?,
                operation,
                Register::overflowing_sub,
            )?,
            Instruction::Mul(operation) => basic_arithmetic_operation(
                self.call_stack.current_mut()?,
                operation,
                Register::overflowing_mul,
            )?,
            Instruction::ConstI(value) => self
                .call_stack
                .current_mut()?
                .registers
                .define_temporary(Register::from(*value)),
            Instruction::ConvI {
                target_type,
                operand,
                overflow,
            } => {
                let current_frame = self.call_stack.current_mut()?;
                let (converted, overflowed) = current_frame
                    .registers
                    .get(*operand)?
                    .convert_to_integer(*target_type);
                current_frame.registers.define_temporary(converted);
                handle_value_overflow(current_frame, *overflow, overflowed)?;
            }
            Instruction::Cmp { x, kind, y } => {
                use instruction_set::ComparisonKind;
                use std::cmp::Ordering;

                let current_frame = self.call_stack.current_mut()?;
                let x_register = current_frame.registers.get(*x)?;
                let y_register = current_frame.registers.get(*y)?;
                let result = x_register.compare_to(y_register).map_err(|actual| {
                    ErrorKind::RegisterTypeMismatch {
                        expected: x_register.value_type(),
                        actual,
                    }
                })?;

                current_frame
                    .registers
                    .define_temporary(Register::from(match kind {
                        ComparisonKind::Equal => result == Ordering::Equal,
                        ComparisonKind::NotEqual => result != Ordering::Equal,
                        ComparisonKind::LessThan => result == Ordering::Less,
                        ComparisonKind::GreaterThan => result == Ordering::Greater,
                        ComparisonKind::LessThanOrEqual => {
                            result == Ordering::Less || result == Ordering::Equal
                        }
                        ComparisonKind::GreaterThanOrEqual => {
                            result == Ordering::Greater || result == Ordering::Equal
                        }
                    }));
            }
            Instruction::Field { field, object } => {
                let current_frame = self.call_stack.current_mut()?;
                let target_field = current_frame
                    .function()
                    .declaring_module()
                    .load_field_raw(*field)?;
                let object_register = current_frame.registers.get(*object)?;

                if let register::Register::Pointer(pointer) = object_register {
                    let (address, overflowed) =
                        pointer.overflowing_add(target_field.offset().try_into().unwrap());

                    if overflowed {
                        todo!("how to deal with overflow when returning field address?");
                    }

                    current_frame
                        .registers
                        .define_temporary(Register::Pointer(address))
                } else {
                    return Err(ErrorKind::RegisterTypeMismatch {
                        expected: register::Type::Pointer(
                            target_field
                                .declaring_struct()
                                .total_size()?
                                .try_into()
                                .unwrap(),
                        ),
                        actual: object_register.value_type(),
                    });
                }
            }
            Instruction::MemInit {
                destination,
                source,
            } => {
                let current_frame = self.call_stack.current_mut()?;
                let destination_register = current_frame.registers.get(*destination)?;

                let destination_address = match destination_register {
                    register::Register::Pointer(address) => address,
                    _ => {
                        return Err(ErrorKind::RegisterTypeMismatch {
                            expected: register::Type::Pointer(0),
                            actual: destination_register.value_type(),
                        })
                    }
                };

                match source {
                    // Source is from a slice, so it is always valid and aligned.
                    instruction_set::MemoryInitializationSource::FromData(data_index) => unsafe {
                        let data = current_frame
                            .function()
                            .declaring_module()
                            .load_data_raw(*data_index)?
                            .bytes();

                        // TODO: Undefined behavior when destination is unaligned.
                        std::ptr::copy(data.as_ptr(), destination_address.address(), data.len());
                    },
                }
            }
            Instruction::Alloca {
                element_type,
                amount,
            } => {
                let current_frame = self.call_stack.current_mut()?;

                // TODO: Have error case for type size too large.
                let element_size: usize = current_frame
                    .function()
                    .declaring_module()
                    .load_type_signature(*element_type)?
                    .size()?
                    .try_into()
                    .unwrap();

                let address = usize::try_from(current_frame.registers.get(*amount)?)
                    .ok()
                    .and_then(|element_count| unsafe {
                        self.value_stack
                            .allocate(element_size * element_count)
                            .map(std::ptr::NonNull::as_ptr)
                    })
                    .unwrap_or_else(std::ptr::null_mut);

                current_frame.registers.define_temporary(Register::Pointer(
                    register::PointerVal::new(address, element_size),
                ));
            }
            Instruction::Break => {
                self.debugger_loop();
                self.call_stack.update_current_breakpoints()?;
            }
        }

        Ok(())
    }

    fn debugger_loop(&mut self) {
        if let Some(debugger) = self.debugger.take() {
            match debugger.inspect(self) {
                debugger::Reply::Continue => self.debugger = Some(debugger),
                debugger::Reply::Detach => {
                    self.call_stack().breakpoints_mut().clear();
                    self.debugger = None;
                }
            }
        }
    }

    fn execute_entry_point(
        &mut self,
        arguments: &[Register],
        entry_point: LoadedFunction<'l>,
    ) -> Result<Vec<Register>> {
        // Wait for the debugger, if one is attached, to tell the application to start.
        self.debugger_loop();

        self.call_stack.push(entry_point, arguments)?;

        let mut entry_point_results = Vec::new();
        while let Some(instruction) = self.next_instruction()? {
            self.execute_instruction(instruction, &mut entry_point_results)?;
        }
        Ok(entry_point_results)
    }
}

pub fn run<'l>(
    loader: &'l loader::Loader<'l>,
    arguments: &[Register], //FnOnce(the heap) -> Vec<Register>
    entry_point: LoadedFunction<'l>,
    call_stack_capacity: call_stack::Capacity,
    value_stack_capacity: mem::stack::Capacity,
    debugger: Option<&'l mut (dyn debugger::Debugger + 'l)>,
) -> std::result::Result<Vec<Register>, Error> {
    let mut interpreter =
        Interpreter::initialize(loader, call_stack_capacity, value_stack_capacity, debugger);
    interpreter
        .execute_entry_point(arguments, entry_point)
        .map_err(|kind| Error::new(kind, interpreter.call_stack.stack_trace()))
}

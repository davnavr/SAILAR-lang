use getmdl::loader;
use registir::format;

pub mod call_stack;
pub mod debugger;
pub mod error;
pub mod mem;
pub mod register;

pub use format::{
    instruction_set,
    instruction_set::{
        DivideByZeroBehavior, Instruction, IntegerConstant, JumpTarget, OverflowBehavior,
        RegisterIndex,
    },
    type_system,
    type_system::PrimitiveType,
};

pub use call_stack::{
    Frame as StackFrame, Stack as CallStack, StackCapacity as CallStackCapacity,
    Trace as StackTrace, TraceFrame as StackTraceFrame,
};

pub use error::{Error, ErrorKind, LoaderError};

pub use mem::stack::Stack as ValueStack;

pub use register::{NumericType, Register, RegisterType};

pub type LoadedFunction<'l> = &'l loader::Function<'l>;

type Result<T> = std::result::Result<T, ErrorKind>;

/// Refers to a block in a method body, where a value of `None` refers to the entry block.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct BlockIndex(pub Option<usize>);

impl BlockIndex {
    pub const fn entry() -> Self {
        Self(None)
    }

    pub fn to_raw(self) -> usize {
        self.0.map(|index| index + 1).unwrap_or(0)
    }
}

impl std::fmt::Display for BlockIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.to_raw(), f)
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
    call_stack: CallStack<'l>,
    value_stack: ValueStack,
    debugger: Option<&'l mut dyn debugger::Debugger>,
}

impl<'l> Interpreter<'l> {
    fn initialize(
        loader: &'l loader::Loader<'l>,
        call_stack_capacity: CallStackCapacity,
        value_stack_capacity: mem::stack::Capacity,
        debugger: Option<&'l mut dyn debugger::Debugger>,
    ) -> Self {
        Self {
            loader,
            call_stack: CallStack::new(call_stack_capacity),
            value_stack: ValueStack::new(value_stack_capacity),
            debugger,
        }
    }

    pub fn call_stack(&mut self) -> &mut CallStack<'l> {
        &mut self.call_stack
    }

    pub fn loader(&self) -> &'l loader::Loader<'l> {
        self.loader
    }

    fn next_instruction(&mut self) -> Result<Option<&'l Instruction>> {
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
        instruction: &'l Instruction,
        entry_point_results: &mut Vec<Register>,
    ) -> Result<()> {
        fn collect_registers_from<'l>(
            frame: &mut StackFrame<'l>,
            indices: &'l [RegisterIndex],
        ) -> Result<Vec<Register>> {
            let mut registers = Vec::with_capacity(indices.len());
            for index in indices.iter() {
                registers.push(frame.registers.get(*index)?.clone());
            }
            Ok(registers)
        }

        fn handle_value_overflow(
            frame: &mut StackFrame,
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
            fn(&Register, &Register) -> std::result::Result<(Register, bool), RegisterType>;

        fn basic_arithmetic_operation<'l>(
            frame: &mut StackFrame<'l>,
            operation: &'l instruction_set::BasicArithmeticOperation,
            o: BasicArithmeticOperation,
        ) -> Result<()> {
            let x = frame.registers.get(operation.x)?;
            let y = frame.registers.get(operation.y)?;
            let (result, overflowed) = o(x, y).map_err(|_| ErrorKind::RegisterTypeMismatch {
                expected: x.value_type,
                actual: y.value_type,
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
            Instruction::Phi(lookup) => {
                let current_frame = self.call_stack.current_mut()?;

                let previous_block = current_frame
                    .instructions
                    .previous_block()
                    .ok_or(ErrorKind::PhiInstructionInEntryBlock)?;

                let register_indices = lookup
                    .get(JumpTarget::try_from(previous_block.to_raw() - 1).unwrap())
                    .ok_or(ErrorKind::MissingPhiInstructionEntry {
                        missing: previous_block,
                    })?;

                let mut registers = collect_registers_from(current_frame, register_indices)?;
                current_frame.registers.append_temporaries(&mut registers);
            }
            Instruction::Switch {
                comparison,
                comparison_type,
                default_target,
                target_lookup,
            } => {
                let current_frame = self.call_stack.current_mut()?;
                let comparison_register = current_frame.registers.get(*comparison)?;

                if comparison_register.value_type != RegisterType::Primitive(*comparison_type) {
                    return Err(ErrorKind::InvalidSwitchComparisonType(
                        comparison_register.value_type,
                    ));
                }

                let comparison_value =
                    instruction_set::IntegerConstant::try_from(comparison_register)
                        .expect("TODO: handle error when comparison register type is invalid");

                // TODO: Allow inputs to target branch.
                self.call_stack.current_jump_to(
                    target_lookup
                        .get(&comparison_value)
                        .unwrap_or(*default_target),
                    &[],
                )?;
            }
            Instruction::Br {
                target,
                input_registers: input_register_indices,
            } => {
                let inputs =
                    collect_registers_from(self.call_stack.current_mut()?, input_register_indices)?;

                self.call_stack.current_jump_to(*target, &inputs)?
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

                self.call_stack.current_jump_to(target, &inputs)?
            }
            Instruction::Call(call) => {
                let current_frame = self.call_stack.current()?;

                let callee = current_frame
                    .function()
                    .declaring_module()
                    .load_function_raw(call.function)?;

                // TODO: Validate signature of callee.
                let mut arguments =
                    Vec::with_capacity(callee.raw_signature()?.parameter_types.len());

                if arguments.capacity() != call.arguments.len() {
                    return Err(ErrorKind::InputCountMismatch {
                        expected: arguments.len(),
                        actual: call.arguments.len(),
                    });
                }

                for index in &call.arguments.0 {
                    arguments.push(current_frame.registers.get(*index)?.clone());
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
                    .size();

                let address = usize::try_from(current_frame.registers.get(*amount)?)
                    .ok()
                    .and_then(|element_count| unsafe {
                        self.value_stack
                            .allocate(element_size * element_count)
                            .map(std::ptr::NonNull::as_ptr)
                    })
                    .unwrap_or_else(std::ptr::null_mut);

                current_frame
                    .registers
                    .define_temporary(Register::from_pointer(
                        address,
                        element_size.try_into().unwrap(),
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
    call_stack_capacity: CallStackCapacity,
    value_stack_capacity: mem::stack::Capacity,
    debugger: Option<&'l mut (dyn debugger::Debugger + 'l)>,
) -> std::result::Result<Vec<Register>, Error> {
    let mut interpreter =
        Interpreter::initialize(loader, call_stack_capacity, value_stack_capacity, debugger);
    interpreter
        .execute_entry_point(arguments, entry_point)
        .map_err(|kind| Error::new(kind, interpreter.call_stack.stack_trace()))
}

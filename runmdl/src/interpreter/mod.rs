use getmdl::loader;
use registir::format;

pub mod call_stack;
pub mod debugger;
pub mod error;
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
    debugger: Option<&'l mut dyn debugger::Debugger>,
}

impl<'l> Interpreter<'l> {
    fn initialize(
        loader: &'l loader::Loader<'l>,
        call_stack_capacity: CallStackCapacity,
        debugger: Option<&'l mut dyn debugger::Debugger>,
    ) -> Self {
        Self {
            loader,
            call_stack: CallStack::new(call_stack_capacity),
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
        fn require_equal_register_types(x: &Register, y: &Register) -> Result<RegisterType> {
            if x.value_type == y.value_type {
                Ok(x.value_type)
            } else {
                Err(ErrorKind::RegisterTypeMismatch {
                    expected: x.value_type,
                    actual: y.value_type,
                })
            }
        }

        fn handle_value_overflow<'l>(
            frame: &mut StackFrame<'l>,
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

        fn basic_arithmetic_operation<
            'l,
        >(
            frame: &mut StackFrame<'l>,
            operation: &'l instruction_set::BasicArithmeticOperation,
            o: fn(RegisterType, &Register, &Register) -> (Register, bool),
        ) -> Result<()> {
            let x = frame.registers.get(operation.x)?;
            let y = frame.registers.get(operation.y)?;
            let (result, overflowed) = o(require_equal_register_types(x, y)?, x, y); // TODO: Move check for equal register types into actual operation
            frame.registers.define_temporary(result);
            handle_value_overflow(frame, operation.overflow, overflowed)
        }

        match instruction {
            Instruction::Nop => (),
            Instruction::Ret(indices) => {
                let popped = self.call_stack.pop()?;
                let mut results = Vec::with_capacity(indices.len());

                for index in indices.iter() {
                    results.push(popped.registers.get(*index)?.clone());
                }

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
                debugger::Reply::Detach => self.debugger = None,
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
    debugger: Option<&'l mut (dyn debugger::Debugger + 'l)>,
) -> std::result::Result<Vec<Register>, Error> {
    let mut interpreter = Interpreter::initialize(loader, call_stack_capacity, debugger);
    interpreter
        .execute_entry_point(arguments, entry_point)
        .map_err(|kind| Error::new(kind, interpreter.call_stack.stack_trace()))
}

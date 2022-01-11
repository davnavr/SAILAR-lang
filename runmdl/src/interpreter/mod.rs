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
    Frame as StackFrame, Stack as CallStack, Trace as StackTrace, TraceFrame as StackTraceFrame,
};

pub use error::{Error, ErrorKind, ProgramHalt};

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

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct InstructionLocation {
    pub block_index: BlockIndex,
    pub code_index: usize,
}

struct Interpreter<'l> {
    /// Contains the modules with the code that is being interpreted, used when the debugger looks up methods by name.
    loader: &'l loader::Loader<'l>,
    call_stack: CallStack<'l>,
    debugger: Option<debugger::Debugger<'l>>,
}

impl<'l> Interpreter<'l> {
    fn initialize(
        loader: &'l loader::Loader<'l>,
        call_stack_capacity: usize,
        debugger_receiver: Option<debugger::MessageReceiver>,
    ) -> Self {
        Self {
            loader,
            call_stack: CallStack::new(call_stack_capacity),
            debugger: debugger_receiver
                .map(|message_source| debugger::Debugger::new(message_source)),
        }
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

    fn handle_value_overflow(
        frame: &mut StackFrame<'l>,
        behavior: OverflowBehavior,
        overflowed: bool,
    ) -> Result<()> {
        match behavior {
            OverflowBehavior::Ignore => (),
            OverflowBehavior::Flag => frame.registers.define_temporary(Register::from(overflowed)),
            OverflowBehavior::Halt => {
                if overflowed {
                    return Err(ErrorKind::Halt(ProgramHalt::IntegerOverflow));
                }
            }
        }
        Ok(())
    }

    fn basic_arithmetic_operation<
        O: FnOnce(RegisterType, &Register, &Register) -> (Register, bool),
    >(
        frame: &mut StackFrame<'l>,
        operation: &'l instruction_set::BasicArithmeticOperation,
        o: O,
    ) -> Result<()> {
        let (result, overflowed) = o(
            RegisterType::from(operation.return_type),
            frame.registers.get(operation.x)?,
            frame.registers.get(operation.y)?,
        );
        frame.registers.define_temporary(result);
        Self::handle_value_overflow(frame, operation.overflow, overflowed)
    }

    fn basic_division_operation<
        O: FnOnce(RegisterType, &Register, &Register) -> Option<(Register, bool)>,
    >(
        frame: &mut StackFrame<'l>,
        operation: &instruction_set::DivisionOperation,
        o: O,
    ) -> Result<()> {
        let numerator = frame.registers.get(operation.numerator)?;
        let denominator = frame.registers.get(operation.denominator)?;
        match o(
            RegisterType::from(operation.return_type),
            numerator,
            denominator,
        ) {
            Some((result, overflowed)) => {
                frame.registers.define_temporary(result);
                Self::handle_value_overflow(frame, operation.overflow, overflowed)
            }
            None => match operation.divide_by_zero {
                #[allow(unused_variables)]
                DivideByZeroBehavior::Return(return_index) => {
                    // TODO: Use a function that converts the register value to operation.return_type
                    todo!("handle div return for division by zero")
                }
                DivideByZeroBehavior::Halt => Err(ErrorKind::Halt(ProgramHalt::DivideByZero)),
            },
        }
    }

    fn basic_bitwise_operation<
        O: FnOnce(register::NumericType, &Register, &Register) -> Register,
    >(
        frame: &mut StackFrame<'l>,
        operation: &'l instruction_set::BitwiseOperation,
        o: O,
    ) -> Result<()> {
        frame.registers.define_temporary(o(
            operation.result_type,
            frame.registers.get(operation.x)?,
            frame.registers.get(operation.y)?,
        ));
        Ok(())
    }

    fn bitwise_shift_operation<
        O: FnOnce(register::NumericType, &Register, &Register) -> Register,
    >(
        frame: &mut StackFrame<'l>,
        operation: &'l instruction_set::BitwiseShiftOperation,
        o: O,
    ) -> Result<()> {
        frame.registers.define_temporary(o(
            *operation.result_type(),
            frame.registers.get(*operation.value())?,
            frame.registers.get(*operation.amount())?,
        ));
        Ok(())
    }

    fn execute_instruction(&mut self, instruction: &'l Instruction) -> Result<Vec<Register>> {
        let current_frame = self.call_stack.current_mut()?;
        let mut entry_point_results = Vec::new();

        match instruction {
            Instruction::Nop => (),
            Instruction::Ret(indices) => {
                let mut results = self.call_stack.pop()?;

                match self.call_stack.peek_mut() {
                    Some(previous_frame) => {
                        previous_frame.registers.append_temporaries(&mut results)
                    }
                    None => entry_point_results = results,
                }
            }
            Instruction::ConstI(value) => current_frame
                .registers
                .define_temporary(Register::from(*value)),
            Instruction::Break => {
                // let current_method = self.current_frame()?.current_method;
                // self.debugger_message_loop(current_method);
                // self.set_debugger_breakpoints();
            }
        }

        Ok(entry_point_results)
    }

    fn expect_debugger_message(&mut self) -> Option<debugger::Message> {
        let message = self
            .debugger
            .as_ref()
            .and_then(|debugger| debugger.receive_message().ok());
        if message.is_none() {
            self.debugger = None;
        }
        message
    }

    fn debugger_message_loop(&mut self, default_method: LoadedFunction<'l>) {
        loop {
            match self.expect_debugger_message() {
                Some(message) => match message.message() {
                    // debugger::MessageKind::SetBreakpoint(breakpoint) => {
                    //     let method = breakpoint
                    //         .method
                    //         .as_ref()
                    //         .map(|method_name| self.loader.lookup_method(method_name))
                    //         .map(|methods| {
                    //             if methods.len() != 1 {
                    //                 todo!("how to handle method not found for breakpoint?")
                    //             } else {
                    //                 methods[0]
                    //             }
                    //         })
                    //         .unwrap_or(default_method);
                    //     self.debugger
                    //         .as_mut()
                    //         .unwrap()
                    //         .set_breakpoint(method, &breakpoint.instruction_location())
                    // }
                    // debugger::MessageKind::GetBreakpoints => {
                    //     message.reply(debugger::MessageReply::Breakpoints(
                    //         self.debugger.as_ref().unwrap().breakpoints(),
                    //     ))
                    // }
                    debugger::MessageKind::GetStackTrace => message.reply(
                        debugger::MessageReply::StackTrace(self.call_stack.stack_trace()),
                    ),
                    debugger::MessageKind::GetRegisters => {
                        let frame = self.call_stack.current().unwrap();
                        message.reply(debugger::MessageReply::Registers(Vec::from(
                            frame.registers.temporaries(),
                        )))
                    }
                    debugger::MessageKind::Continue => return, // TODO: When continue is recevied, store the reply_channel somewhere so a reply can be sent when breakpoint is hit?
                },
                None => return,
            }
        }
    }

    fn set_debugger_breakpoints(&mut self) {
        if let Some(debugger) = &self.debugger {
            // for frame in &mut self.stack_frames {
            //     frame.breakpoints.source =
            //         debugger.breakpoints_in_block(frame.current_method, frame.block_index);
            // }
        }
    }

    fn execute_entry_point(
        &mut self,
        arguments: &[Register],
        entry_point: LoadedFunction<'l>,
    ) -> Result<Vec<Register>> {
        // Wait for the debugger, if one is attached, to tell the application to start.
        self.debugger_message_loop(entry_point);

        self.call_stack.push(entry_point, arguments)?;

        let mut entry_point_results = Vec::new();
        while let Some(instruction) = self.next_instruction()? {
            // TODO: Check if a breakpoint has been hit.
            entry_point_results = self.execute_instruction(instruction)?;
        }
        Ok(entry_point_results)
    }
}

pub fn run<'l>(
    loader: &'l loader::Loader<'l>,
    arguments: &[Register],
    entry_point: LoadedFunction<'l>,
    call_stack_capacity: usize,
    debugger_message_channel: Option<debugger::MessageReceiver>,
) -> std::result::Result<Vec<Register>, Error> {
    let mut interpreter =
        Interpreter::initialize(loader, call_stack_capacity, debugger_message_channel);
    interpreter
        .execute_entry_point(arguments, entry_point)
        .map_err(|kind| Error::new(kind, interpreter.call_stack.stack_trace()))
}

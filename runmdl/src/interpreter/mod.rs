use getmdl::loader;
use registir::format;

pub mod debugger;
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

pub use register::{NumericType, Register, RegisterType};

pub type LoadedFunction<'l> = &'l loader::Function<'l>;

#[derive(Debug)]
#[non_exhaustive]
pub enum ProgramHalt {
    IntegerOverflow,
    DivideByZero,
}

impl std::fmt::Display for ProgramHalt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::IntegerOverflow => "arithmetic operation resulted in an integer overflow",
            Self::DivideByZero => "attempt to divide number by zero",
        })
    }
}

impl std::error::Error for ProgramHalt {}

#[derive(Debug)]
#[non_exhaustive]
pub enum ErrorKind {
    LoadError(loader::Error),
    CallStackUnderflow,
    CallStackOverflow,
    UnexpectedEndOfBlock,
    UndefinedRegister(RegisterIndex),
    UndefinedBlock(JumpTarget),
    InputCountMismatch { expected: usize, actual: usize },
    ResultCountMismatch { expected: usize, actual: usize },
    Halt(ProgramHalt),
}

macro_rules! error_kind_conversion {
    ($source_type: ty, $case: ident) => {
        impl From<$source_type> for ErrorKind {
            fn from(error: $source_type) -> Self {
                Self::$case(error)
            }
        }
    };
}

error_kind_conversion!(loader::Error, LoadError);
error_kind_conversion!(ProgramHalt, Halt);

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LoadError(error) => std::fmt::Display::fmt(error, f),
            Self::CallStackUnderflow => f.write_str("call stack underflow occured"),
            Self::CallStackOverflow => f.write_str("exceeded maximum call stack depth"),
            Self::UnexpectedEndOfBlock => write!(f, "end of block unexpectedly reached"),
            Self::UndefinedRegister(RegisterIndex::Input(index)) => write!(f, "undefined input register {}", index),
            Self::UndefinedRegister(RegisterIndex::Temporary(index)) => write!(f, "undefined temporary register {}", index),
            Self::UndefinedBlock(index) => write!(f, "undefined block {}", index.0),
            Self::InputCountMismatch { expected, actual } => write!(f, "expected {} input values but got {}", expected, actual),
            Self::ResultCountMismatch { expected, actual } => write!(f, "expected {} result values but got {}", expected, actual),
            Self::Halt(reason) => write!(f, "program execution halted, {}", reason),
        }
    }
}

impl std::error::Error for ErrorKind {}

type Result<T> = std::result::Result<T, ErrorKind>;

/// Describes a stack frame in the call stack.
#[derive(Clone, Debug)]
pub struct StackTrace {
    depth: usize,
    location: InstructionLocation,
    method: debugger::FullMethodIdentifier,
    input_registers: Box<[Register]>,
    temporary_registers: Box<[Register]>,
}

impl StackTrace {
    pub fn depth(&self) -> usize {
        self.depth
    }

    pub fn location(&self) -> &InstructionLocation {
        &self.location
    }

    pub fn method(&self) -> &debugger::FullMethodIdentifier {
        &self.method
    }

    pub fn input_registers(&self) -> &[Register] {
        &self.input_registers
    }

    pub fn temporary_registers(&self) -> &[Register] {
        &self.temporary_registers
    }
}

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

static BREAK_INSTRUCTION: &'static Instruction = &Instruction::Break;

#[derive(Default)]
struct StackFrameBreakPoints {
    source: Option<debugger::BreakpointsReference>,
    index: usize,
}

impl StackFrameBreakPoints {
    fn is_empty(&self) -> bool {
        match self.source {
            None => true,
            Some(ref indices) => indices.borrow().len() == self.index,
        }
    }

    fn next(&mut self) -> Option<usize> {
        self.source.as_ref().and_then(|indices| {
            let index = indices.borrow().get(self.index).copied();
            index
        })
    }
}

struct StackFrame<'l> {
    depth: usize,
    input_registers: Vec<Register>,
    result_registers: std::rc::Rc<std::cell::RefCell<Vec<Register>>>,
    current_method: LoadedFunction<'l>,
    code: &'l format::Code,
    block_index: BlockIndex,
    instructions: &'l [Instruction],
    breakpoints: StackFrameBreakPoints,
    temporary_registers: Vec<Register>,
    //register_arena: typed_arena::Arena<Register>, // TODO: If arena is used for a frame's registers, have the arena be tied to the frame's lifetime.
}

impl<'l> StackFrame<'l> {
    /// Creates a new stack frame for executing the specified method's entry block.
    fn new(
        depth: usize,
        input_registers: Vec<Register>,
        result_registers: Vec<Register>,
        current_method: LoadedFunction<'l>,
        code: &'l format::Code,
    ) -> Self {
        Self {
            depth,
            input_registers,
            result_registers: std::rc::Rc::new(std::cell::RefCell::new(result_registers)),
            current_method,
            code,
            block_index: BlockIndex::entry(),
            instructions: &code.entry_block.instructions,
            breakpoints: StackFrameBreakPoints::default(),
            temporary_registers: Vec::new(),
        }
    }

    fn code_index(&self) -> usize {
        (self.instructions.as_ptr() as usize
            - self.current_block().instructions.0.as_ptr() as usize)
            / std::mem::size_of::<Instruction>()
    }

    fn breakpoint_hit(&mut self) -> bool {
        if self.breakpoints.is_empty() {
            false
        } else {
            let current_index = self.code_index();
            match self.breakpoints.next() {
                Some(offset) => {
                    if offset <= current_index {
                        self.breakpoints.index += 1;
                    }
                    offset == current_index
                }
                None => false,
            }
        }
    }

    fn next_instruction(&mut self) -> Option<&'l Instruction> {
        if self.breakpoint_hit() {
            Some(BREAK_INSTRUCTION)
        } else {
            let next = self.instructions.first();
            if next.is_some() {
                self.instructions = &self.instructions[1..];
            }
            next
        }
    }

    fn define_temporary(&mut self, temporary: Register) {
        self.temporary_registers.push(temporary)
    }

    fn register(&self, index: RegisterIndex) -> Result<&Register> {
        macro_rules! lookup_register {
            ($register_index: expr, $register_lookup: expr) => {{
                let raw_index = usize::try_from($register_index)
                    .map_err(|_| ErrorKind::UndefinedRegister(index))?;
                $register_lookup
                    .get(raw_index)
                    .ok_or(ErrorKind::UndefinedRegister(index))
            }};
        }

        match index {
            RegisterIndex::Temporary(temporary_index) => {
                lookup_register!(temporary_index, self.temporary_registers)
            }
            RegisterIndex::Input(input_index) => {
                lookup_register!(input_index, self.input_registers)
            }
        }
    }

    fn many_registers(&self, indices: &[RegisterIndex]) -> Result<Vec<&Register>> {
        let mut registers = Vec::with_capacity(indices.len());
        for index in indices {
            registers.push(self.register(*index)?);
        }
        Ok(registers)
    }

    fn current_block(&self) -> &'l format::CodeBlock {
        match self.block_index {
            BlockIndex(None) => &self.code.entry_block,
            BlockIndex(Some(other_index)) => &self.code.blocks[other_index],
        }
    }

    fn location(&self) -> InstructionLocation {
        InstructionLocation {
            block_index: self.block_index,
            code_index: self.code_index(),
        }
    }

    fn stack_trace(&self) -> StackTrace {
        StackTrace {
            depth: self.depth,
            location: self.location(),
            method: self.current_method.symbol().unwrap(),
            input_registers: self.input_registers.clone().into_boxed_slice(),
            temporary_registers: self.temporary_registers.clone().into_boxed_slice(),
        }
    }
}

struct Interpreter<'l> {
    /// Contains the modules with the code that is being interpreted, used when the debugger looks up methods by name.
    loader: &'l loader::Loader<'l>,
    stack_frames: Vec<StackFrame<'l>>,
    max_stack_depth: usize,
    debugger: Option<debugger::Debugger<'l>>,
}

impl<'l> Interpreter<'l> {
    fn initialize(
        loader: &'l loader::Loader<'l>,
        debugger_receiver: Option<debugger::MessageReceiver>,
    ) -> Self {
        Self {
            loader,
            stack_frames: Vec::new(),
            max_stack_depth: 0xFF,
            debugger: debugger_receiver
                .map(|message_source| debugger::Debugger::new(message_source)),
        }
    }

    fn current_frame(&mut self) -> Result<&mut StackFrame<'l>> {
        self.stack_frames
            .last_mut()
            .ok_or(ErrorKind::CallStackUnderflow)
    }

    fn stack_trace(&self) -> Vec<StackTrace> {
        self.stack_frames
            .iter()
            .map(|frame| frame.stack_trace())
            .collect()
    }

    /// Pushes a new stack frame for calling the specified method.
    ///
    /// Returns a vector containing the registers that will contain the results of executing the method.
    fn invoke_method(
        &mut self,
        arguments: &[Register],
        method: LoadedFunction<'l>,
    ) -> Result<std::rc::Rc<std::cell::RefCell<Vec<Register>>>> {
        let signature = method.signature()?;
        let mut argument_registers = Register::initialize_many(signature.parameter_types().into_iter().copied());
        let result_registers = Register::initialize_many(signature.return_types().into_iter().copied());

        if argument_registers.len() != arguments.len() {
            return Err(ErrorKind::InputCountMismatch {
                expected: argument_registers.len(),
                actual: arguments.len(),
            });
        }

        for (i, register) in argument_registers.iter_mut().enumerate() {
            register.value = arguments[i].value.clone();
        }

        match method.raw_body() {
            format::FunctionBody::Defined(code_index) => {
                let code = method.declaring_module().load_code_raw(*code_index)?;
                let call_stack_depth = self.stack_frames.len();

                if call_stack_depth > self.max_stack_depth {
                    return Err(ErrorKind::CallStackOverflow);
                }

                self.stack_frames.push(StackFrame::new(
                    self.stack_frames.len(),
                    argument_registers,
                    result_registers,
                    method,
                    code,
                ));

                self.set_debugger_breakpoints();

                Ok(self.current_frame()?.result_registers.clone())
            }
            format::FunctionBody::External { .. } => todo!("TODO: add support for external calls"),
        }
    }

    fn next_instruction(&mut self) -> Result<Option<&'l Instruction>> {
        match self.stack_frames.last_mut() {
            Some(current_frame) => current_frame
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
            OverflowBehavior::Flag => frame.define_temporary(Register::from(overflowed)),
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
            frame.register(operation.x)?,
            frame.register(operation.y)?,
        );
        frame.define_temporary(result);
        Self::handle_value_overflow(frame, operation.overflow, overflowed)
    }

    fn basic_division_operation<
        O: FnOnce(RegisterType, &Register, &Register) -> Option<(Register, bool)>,
    >(
        frame: &mut StackFrame<'l>,
        operation: &instruction_set::DivisionOperation,
        o: O,
    ) -> Result<()> {
        let numerator = frame.register(operation.numerator)?;
        let denominator = frame.register(operation.denominator)?;
        match o(
            RegisterType::from(operation.return_type),
            numerator,
            denominator,
        ) {
            Some((result, overflowed)) => {
                frame.define_temporary(result);
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
        frame.define_temporary(o(
            operation.result_type,
            frame.register(operation.x)?,
            frame.register(operation.y)?,
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
        frame.define_temporary(o(
            *operation.result_type(),
            frame.register(*operation.value())?,
            frame.register(*operation.amount())?,
        ));
        Ok(())
    }

    fn jump_to_block(
        current_frame: &mut StackFrame<'l>,
        debugger: Option<&debugger::Debugger<'l>>,
        target: JumpTarget,
        inputs: &[RegisterIndex],
    ) -> Result<()> {
        // Replace input registers with new inputs.
        current_frame.input_registers = {
            let mut new_inputs = vec![Register::uninitialized(); inputs.len()];
            Register::copy_many_raw(&current_frame.many_registers(inputs)?, &mut new_inputs);
            new_inputs
        };

        current_frame.temporary_registers.clear();

        // Index into the method body's other blocks, NONE of the indices refer to the entry block.
        let new_block_index = usize::try_from(target).unwrap();
        let new_block = current_frame
            .code
            .blocks
            .get(new_block_index)
            .ok_or(ErrorKind::UndefinedBlock(target))?;

        {
            let expected = usize::try_from(new_block.input_register_count).unwrap();
            if expected != inputs.len() {
                return Err(ErrorKind::InputCountMismatch {
                    expected,
                    actual: inputs.len(),
                });
            }
        }

        current_frame.block_index = BlockIndex(Some(new_block_index));
        current_frame.instructions = &new_block.instructions;

        if let Some(debugger) = debugger {
            current_frame.breakpoints.source = debugger
                .breakpoints_in_block(current_frame.current_method, current_frame.block_index);
            current_frame.breakpoints.index = 0;
        }

        Ok(())
    }

    fn execute_instruction(&mut self, instruction: &'l Instruction) -> Result<()> {
        match instruction {
            Instruction::Nop => (),
            Instruction::Ret(indices) => {
                let current_frame = self.current_frame()?;
                // Copy results into the registers of the previous frame.
                Register::copy_many_raw(
                    &current_frame.many_registers(indices)?,
                    current_frame.result_registers.borrow_mut().as_mut_slice(),
                );
                self.stack_frames.pop();
            }
            Instruction::Br(target, input_registers) => {
                let current_frame = self
                    .stack_frames
                    .last_mut()
                    .ok_or(ErrorKind::CallStackUnderflow)?;
                // self.current_frame()?;
                Self::jump_to_block(
                    current_frame,
                    self.debugger.as_ref(),
                    *target,
                    input_registers,
                )?
            }
            Instruction::BrIf {
                condition,
                true_branch,
                false_branch,
                input_registers,
            } => {
                let current_frame = self
                    .stack_frames
                    .last_mut()
                    .ok_or(ErrorKind::CallStackUnderflow)?;
                // self.current_frame()?;

                let target = if current_frame.register(*condition)?.is_truthy() {
                    *true_branch
                } else {
                    *false_branch
                };

                Self::jump_to_block(
                    current_frame,
                    self.debugger.as_ref(),
                    target,
                    input_registers,
                )?
            }
            Instruction::Add(operation) => Self::basic_arithmetic_operation(
                self.current_frame()?,
                operation,
                Register::overflowing_add,
            )?,
            Instruction::Sub(operation) => Self::basic_arithmetic_operation(
                self.current_frame()?,
                operation,
                Register::overflowing_sub,
            )?,
            Instruction::Mul(operation) => Self::basic_arithmetic_operation(
                self.current_frame()?,
                operation,
                Register::overflowing_mul,
            )?,
            Instruction::Div(operation) => Self::basic_division_operation(
                self.current_frame()?,
                operation,
                Register::overflowing_div,
            )?,
            Instruction::And(operation) => Self::basic_bitwise_operation(
                self.current_frame()?,
                operation,
                Register::bitwise_add,
            )?,
            Instruction::Or(operation) => Self::basic_bitwise_operation(
                self.current_frame()?,
                operation,
                Register::bitwise_or,
            )?,
            Instruction::Not(result_type, value) => {
                let frame = self.current_frame()?;
                frame.define_temporary(Register::bitwise_not(*result_type, frame.register(*value)?))
            }
            Instruction::Xor(operation) => Self::basic_bitwise_operation(
                self.current_frame()?,
                operation,
                Register::bitwise_xor,
            )?,
            Instruction::ShL(operation) => {
                Self::bitwise_shift_operation(self.current_frame()?, operation, Register::shl)?
            }
            Instruction::ShR(operation) => {
                Self::bitwise_shift_operation(self.current_frame()?, operation, Register::shr)?
            }
            Instruction::RotL(operation) => Self::bitwise_shift_operation(
                self.current_frame()?,
                operation,
                Register::rotate_left,
            )?,
            Instruction::RotR(operation) => Self::bitwise_shift_operation(
                self.current_frame()?,
                operation,
                Register::rotate_right,
            )?,
            Instruction::ConstI(value) => self
                .current_frame()?
                .define_temporary(Register::from(*value)),
            Instruction::Break => {
                let current_method = self.current_frame()?.current_method;
                self.debugger_message_loop(current_method);
                self.set_debugger_breakpoints();
            }
        }
        Ok(())
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
                    debugger::MessageKind::SetBreakpoint(breakpoint) => {
                        let method = breakpoint
                            .method
                            .as_ref()
                            .map(|method_name| self.loader.lookup_method(method_name))
                            .map(|methods| {
                                if methods.len() != 1 {
                                    todo!("how to handle method not found for breakpoint?")
                                } else {
                                    methods[0]
                                }
                            })
                            .unwrap_or(default_method);
                        self.debugger
                            .as_mut()
                            .unwrap()
                            .set_breakpoint(method, &breakpoint.instruction_location())
                    }
                    debugger::MessageKind::GetBreakpoints => {
                        message.reply(debugger::MessageReply::Breakpoints(
                            self.debugger.as_ref().unwrap().breakpoints(),
                        ))
                    }
                    debugger::MessageKind::GetStackTrace => {
                        message.reply(debugger::MessageReply::StackTrace(self.stack_trace()))
                    }
                    debugger::MessageKind::GetRegisters => {
                        let frame = self.current_frame().unwrap();
                        message.reply(debugger::MessageReply::Registers(
                            frame.temporary_registers.clone(),
                        ))
                    }
                    debugger::MessageKind::Continue => return, // TODO: When continue is recevied, store the reply_channel somewhere so a reply can be sent when breakpoint is hit?
                },
                None => return,
            }
        }
    }

    fn set_debugger_breakpoints(&mut self) {
        if let Some(debugger) = &self.debugger {
            for frame in &mut self.stack_frames {
                frame.breakpoints.source =
                    debugger.breakpoints_in_block(frame.current_method, frame.block_index);
            }
        }
    }

    fn execute_entry_point(
        &mut self,
        arguments: &[Register],
        entry_point: LoadedFunction<'l>,
    ) -> Result<Vec<Register>> {
        // Wait for the debugger, if one is attached, to tell the application to start.
        self.debugger_message_loop(entry_point);

        let entry_point_results = self.invoke_method(arguments, entry_point)?;

        while let Some(instruction) = self.next_instruction()? {
            // TODO: Check if a breakpoint has been hit.
            self.execute_instruction(instruction)?;
        }

        Ok(entry_point_results.take())
    }
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    stack_trace: Vec<StackTrace>,
}

impl Error {
    pub(crate) fn with_no_stack_trace(kind: ErrorKind) -> Self {
        Self {
            kind,
            stack_trace: Vec::new(),
        }
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn stack_trace(&self) -> &[StackTrace] {
        &self.stack_trace
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl std::error::Error for Error {}

pub fn run<'l>(
    loader: &'l loader::Loader<'l>,
    arguments: &[Register],
    entry_point: LoadedFunction<'l>,
    debugger_message_channel: Option<debugger::MessageReceiver>,
) -> std::result::Result<Vec<Register>, Error> {
    let mut interpreter = Interpreter::initialize(loader, debugger_message_channel);
    interpreter
        .execute_entry_point(arguments, entry_point)
        .map_err(|kind| Error {
            kind,
            stack_trace: interpreter.stack_trace(),
        })
}

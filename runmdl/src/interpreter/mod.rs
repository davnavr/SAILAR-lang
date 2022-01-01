use getmdl::loader;
use registir::format;

pub mod debugger;
pub mod register;

pub use format::{
    instruction_set,
    instruction_set::{
        DivideByZeroBehavior, Instruction, IntegerConstant, OverflowBehavior, RegisterIndex,
    },
    type_system,
    type_system::PrimitiveType,
};
pub use register::{Register, RegisterType};

pub type LoadedMethod<'l> = &'l loader::Method<'l>;

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
pub enum Error /*Kind*/ {
    LoadError(loader::LoadError),
    ArgumentCountMismatch { expected: usize, actual: usize },
    CallStackUnderflow,
    CallStackOverflow,
    DirectAbstractMethodCall,
    UnexpectedEndOfBlock,
    UndefinedRegister(RegisterIndex),
    ResultCountMismatch { expected: usize, actual: usize },
    Halt(ProgramHalt),
}

macro_rules! error_kind_conversion {
    ($source_type: ty, $case: ident) => {
        impl From<$source_type> for Error {
            fn from(error: $source_type) -> Self {
                Self::$case(error)
            }
        }
    };
}

error_kind_conversion!(loader::LoadError, LoadError);
error_kind_conversion!(ProgramHalt, Halt);

/*
pub struct Error {
    frame: StackFrame,
    error: ErrorKind,
}
*/

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LoadError(error) => std::fmt::Display::fmt(error, f),
            Self::ArgumentCountMismatch { expected, actual } => write!(f, "expected {} arguments but got {}", expected, actual),
            Self::CallStackUnderflow => f.write_str("call stack underflow occured"),
            Self::CallStackOverflow => f.write_str("exceeded maximum call stack depth"),
            Self::DirectAbstractMethodCall => write!(f, "attempt to call abstract method with call, when call.virt should have been used instead"),
            Self::UnexpectedEndOfBlock => write!(f, "end of block unexpectedly reached"),
            Self::UndefinedRegister(RegisterIndex::Input(index)) => write!(f, "undefined input register {}", index),
            Self::UndefinedRegister(RegisterIndex::Temporary(index)) => write!(f, "undefined temporary register {}", index),
            Self::ResultCountMismatch { expected, actual } => write!(f, "expected {} result values but got {}", expected, actual),
            Self::Halt(reason) => write!(f, "program execution halted, {}", reason),
        }
    }
}

impl std::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;

pub struct StackTrace {
    location: InstructionLocation,
    method: debugger::FullMethodIdentifier,
}

impl StackTrace {
    pub fn location(&self) -> &InstructionLocation {
        &self.location
    }

    pub fn method(&self) -> &debugger::FullMethodIdentifier {
        &self.method
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

#[derive(Debug, Eq, PartialEq, Hash)]
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
    current_method: LoadedMethod<'l>,
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
        current_method: LoadedMethod<'l>,
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
                    .map_err(|_| Error::UndefinedRegister(index))?;
                $register_lookup
                    .get(raw_index)
                    .ok_or(Error::UndefinedRegister(index))
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
            location: self.location(),
            method: self.current_method.identifier().unwrap(),
        }
    }
}

struct Interpreter<'l> {
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
            .ok_or(Error::CallStackUnderflow)
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
        method: LoadedMethod<'l>,
    ) -> Result<std::rc::Rc<std::cell::RefCell<Vec<Register>>>> {
        let signature = method.raw_signature_types()?;
        let mut argument_registers = Register::initialize_many(signature.parameter_types);
        let result_registers = Register::initialize_many(signature.return_types);

        if argument_registers.len() != arguments.len() {
            return Err(Error::ArgumentCountMismatch {
                expected: argument_registers.len(),
                actual: arguments.len(),
            });
        }

        for (i, register) in argument_registers.iter_mut().enumerate() {
            register.value = arguments[i].value.clone();
        }

        match method.raw_body() {
            format::MethodBody::Defined(code_index) => {
                let code = method.declaring_module().load_code_raw(*code_index)?;
                let call_stack_depth = self.stack_frames.len();

                if call_stack_depth > self.max_stack_depth {
                    return Err(Error::CallStackOverflow);
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
            format::MethodBody::Abstract => Err(Error::DirectAbstractMethodCall),
            format::MethodBody::External { .. } => todo!("TODO: add support for external calls"),
        }
    }

    fn next_instruction(&mut self) -> Result<Option<&'l Instruction>> {
        match self.stack_frames.last_mut() {
            Some(current_frame) => current_frame
                .next_instruction()
                .map(Some)
                .ok_or(Error::UnexpectedEndOfBlock),
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
                    return Err(Error::Halt(ProgramHalt::IntegerOverflow));
                }
            }
        }
        Ok(())
    }

    fn basic_arithmetic_operation<
        O: FnOnce(RegisterType, &Register, &Register) -> (Register, bool),
    >(
        frame: &mut StackFrame<'l>,
        operation: &instruction_set::BasicArithmeticOperation,
        o: O,
    ) -> Result<()> {
        let x = frame.register(operation.x)?;
        let y = frame.register(operation.y)?;
        let (result, overflowed) = o(RegisterType::from(operation.return_type), x, y);
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
                DivideByZeroBehavior::Halt => Err(Error::Halt(ProgramHalt::DivideByZero)),
            },
        }
    }

    fn execute_instruction(&mut self, instruction: &'l Instruction) -> Result<()> {
        match instruction {
            Instruction::Nop => (),
            Instruction::Ret(indices) => {
                let current_frame = self.current_frame()?;
                // Copy results into the registers of the previous frame.
                let registers = current_frame.many_registers(indices)?;
                Register::copy_many_raw(
                    &registers,
                    current_frame.result_registers.borrow_mut().as_mut_slice(),
                );
                self.stack_frames.pop();
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

    fn debugger_message_loop(&mut self, default_method: LoadedMethod<'l>) {
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
}

pub fn run<'l>(
    loader: &'l loader::Loader<'l>, // TOOD: Loader not necessary?
    arguments: &[Register],
    entry_point: LoadedMethod<'l>,
    debugger_message_channel: Option<debugger::MessageReceiver>,
) -> Result<Vec<Register>> {
    let mut interpreter = Interpreter::initialize(loader, debugger_message_channel);

    // Wait for the debugger, if one is attached, to tell the application to start.
    interpreter.debugger_message_loop(entry_point);

    let entry_point_results = interpreter.invoke_method(arguments, entry_point)?;

    while let Some(instruction) = interpreter.next_instruction()? {
        // TODO: Check if a breakpoint has been hit.
        interpreter.execute_instruction(instruction)?;
    }

    Ok(entry_point_results.take())
}

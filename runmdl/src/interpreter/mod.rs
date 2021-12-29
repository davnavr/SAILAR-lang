use getmdl::loader;
use registir::format;

pub mod register;

pub use format::{
    instruction_set,
    instruction_set::{Instruction, IntegerConstant, OverflowBehavior, RegisterIndex},
    type_system,
    type_system::PrimitiveType,
};
pub use register::{Register, RegisterType};

type LoadedMethod<'l> = &'l loader::Method<'l>;

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

//struct StackTrace

struct StackFrame<'l> {
    depth: usize,
    input_registers: Vec<Register>,
    result_registers: std::rc::Rc<std::cell::RefCell<Vec<Register>>>,
    current_method: LoadedMethod<'l>,
    code: &'l format::Code,
    block_index: Option<usize>,
    instructions: &'l [format::instruction_set::Instruction],
    temporary_registers: Vec<Register>,
    //register_arena: typed_arena::Arena<Register>, // TODO: If arena is used for a frame's registers, have the arena be tied to the frame's lifetime.
}

impl<'l> StackFrame<'l> {
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
            block_index: None,
            instructions: &code.entry_block.instructions,
            temporary_registers: Vec::new(),
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
}

pub struct Interpreter<'l> {
    loader: &'l loader::Loader<'l>,
    stack_frames: Vec<StackFrame<'l>>,
}

impl<'l> Interpreter<'l> {
    pub fn initialize(loader: &'l loader::Loader<'l>) -> Self {
        Self {
            loader,
            stack_frames: Vec::new(),
        }
    }

    fn current_frame(&mut self) -> Result<&mut StackFrame<'l>> {
        self.stack_frames
            .last_mut()
            .ok_or(Error::CallStackUnderflow)
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
                self.stack_frames.push(StackFrame::new(
                    self.stack_frames.len(),
                    argument_registers,
                    result_registers,
                    method,
                    code,
                ));
                Ok(self.current_frame()?.result_registers.clone())
            }
            format::MethodBody::Abstract => Err(Error::DirectAbstractMethodCall),
            format::MethodBody::External { .. } => todo!("TODO: add support for external calls"),
        }
    }

    fn next_instruction(&mut self) -> Result<Option<&'l Instruction>> {
        match self.stack_frames.last_mut() {
            Some(current_frame) => match current_frame.instructions.first() {
                Some(instruction) => {
                    current_frame.instructions = &current_frame.instructions[1..];
                    Ok(Some(instruction))
                }
                None => Err(Error::UnexpectedEndOfBlock),
            },
            None => Ok(None),
        }
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

        match operation.overflow {
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

    fn execute_instruction(&mut self, instruction: &'l Instruction) -> Result<()> {
        let current_frame = self.current_frame()?;

        match instruction {
            Instruction::Nop => (),
            Instruction::Ret(indices) => {
                // Copy results into the registers of the previous frame.
                let registers = current_frame.many_registers(indices)?;
                Register::copy_many_raw(
                    &registers,
                    current_frame.result_registers.borrow_mut().as_mut_slice(),
                );
                self.stack_frames.pop();
            }
            Instruction::Add(operation) => Self::basic_arithmetic_operation(
                current_frame,
                operation,
                Register::overflowing_add,
            )?,
            Instruction::Sub(operation) => Self::basic_arithmetic_operation(
                current_frame,
                operation,
                Register::overflowing_sub,
            )?,
            Instruction::Mul(operation) => Self::basic_arithmetic_operation(
                current_frame,
                operation,
                Register::overflowing_mul,
            )?,
            Instruction::ConstI(value) => current_frame.define_temporary(Register::from(*value)),
            _ => todo!("Interpretation of {:?} is not yet implemented", instruction),
        }

        Ok(())
    }
}

pub fn run<'l>(
    loader: &'l loader::Loader<'l>,
    arguments: &[Register],
    entry_point: LoadedMethod<'l>,
) -> Result<Vec<Register>> {
    let mut interpreter = Interpreter::initialize(loader);
    let entry_point_results = interpreter.invoke_method(arguments, entry_point)?;

    while let Some(instruction) = interpreter.next_instruction()? {
        interpreter.execute_instruction(instruction)?;
    }

    Ok(entry_point_results.take())
}

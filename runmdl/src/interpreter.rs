use getmdl::loader;
use registir::format;

pub use format::{
    instruction_set,
    instruction_set::{Instruction, IntegerConstant},
    type_system,
    type_system::PrimitiveType,
};

#[derive(Debug, Clone, Copy)]
enum RegisterType {
    Primitive(PrimitiveType),
    //Pointer(usize),
    //Object
}

#[derive(Clone, Copy)]
union RegisterValue {
    s_byte: i8,
    u_byte: u8,
    s_short: i16,
    u_short: u16,
    s_int: i32,
    u_int: u32,
    s_long: i64,
    u_long: u64,
    f_single: f32,
    f_double: f64,
    s_native: isize,
    u_native: usize,
}

impl Default for RegisterValue {
    fn default() -> Self {
        Self { u_long: 0 }
    }
}

pub struct Register {
    value: RegisterValue,
    value_type: RegisterType,
}

impl Register {
    fn initialize(value_type: &type_system::AnyType) -> Self {
        use type_system::{AnyType, HeapType, SimpleType};

        Self {
            value: RegisterValue::default(),
            value_type: match value_type {
                AnyType::Heap(HeapType::Val(SimpleType::Primitive(primitive_type))) => {
                    RegisterType::Primitive(*primitive_type)
                }
                _ => todo!("Unsupported register type"),
            },
        }
    }

    fn initialize_many<'l, T: std::iter::IntoIterator<Item = &'l type_system::AnyType>>(
        types: T,
    ) -> Vec<Register> {
        types.into_iter().map(Self::initialize).collect()
    }
}

macro_rules! register_conversion_from {
    ($source_type: ty, $value_field: ident, $register_type: expr) => {
        impl From<$source_type> for Register {
            fn from(value: $source_type) -> Self {
                Self {
                    value: RegisterValue {
                        $value_field: value,
                    },
                    value_type: $register_type,
                }
            }
        }
    };
}

register_conversion_from!(i8, s_byte, RegisterType::Primitive(PrimitiveType::S8));
register_conversion_from!(u8, u_byte, RegisterType::Primitive(PrimitiveType::U8));
register_conversion_from!(i16, s_short, RegisterType::Primitive(PrimitiveType::S16));
register_conversion_from!(u16, u_short, RegisterType::Primitive(PrimitiveType::U16));
register_conversion_from!(i32, s_int, RegisterType::Primitive(PrimitiveType::S32));
register_conversion_from!(u32, u_int, RegisterType::Primitive(PrimitiveType::U32));
register_conversion_from!(i64, s_long, RegisterType::Primitive(PrimitiveType::S64));
register_conversion_from!(u64, u_long, RegisterType::Primitive(PrimitiveType::U64));
register_conversion_from!(
    isize,
    s_native,
    RegisterType::Primitive(PrimitiveType::SNative)
);
register_conversion_from!(
    usize,
    u_native,
    RegisterType::Primitive(PrimitiveType::UNative)
);
register_conversion_from!(f32, f_single, RegisterType::Primitive(PrimitiveType::F32));
register_conversion_from!(f64, f_double, RegisterType::Primitive(PrimitiveType::F64));

impl From<IntegerConstant> for Register {
    fn from(value: IntegerConstant) -> Self {
        match value {
            IntegerConstant::U8(value) => Self::from(value),
            IntegerConstant::S8(value) => Self::from(value),
            IntegerConstant::U16(value) => Self::from(value),
            IntegerConstant::S16(value) => Self::from(value),
            IntegerConstant::U32(value) => Self::from(value),
            IntegerConstant::S32(value) => Self::from(value),
            IntegerConstant::U64(value) => Self::from(value),
            IntegerConstant::S64(value) => Self::from(value),
        }
    }
}

/// The error type returned when a conversion from a register value fails.
#[derive(Debug, Clone)]
pub struct TryFromRegisterValueError {
    expected: RegisterType,
    actual: RegisterType,
}

impl std::fmt::Display for TryFromRegisterValueError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "expected register to contain a value of type {:?} but got {:?}",
            self.expected, self.actual
        )
    }
}

impl std::error::Error for TryFromRegisterValueError {}

impl TryFrom<&Register> for i32 {
    type Error = TryFromRegisterValueError;

    fn try_from(register: &Register) -> std::result::Result<i32, Self::Error> {
        match &register.value_type {
            RegisterType::Primitive(PrimitiveType::S32) => unsafe { Ok(register.value.s_int) },
            actual => Err(Self::Error {
                expected: RegisterType::Primitive(PrimitiveType::S32),
                actual: *actual,
            }),
        }
    }
}

type LoadedMethod<'l> = &'l loader::Method<'l>;

#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    //ErrorKind
    LoadError(loader::LoadError),
    ArgumentCountMismatch { expected: usize, actual: usize },
    CallStackUnderflow,
    DirectAbstractMethodCall,
    UnexpectedEndOfBlock,
}

/*
pub struct Error {
    frame: StackFrame,
    error: ErrorKind,
}
*/

impl From<loader::LoadError> for Error {
    fn from(error: loader::LoadError) -> Self {
        Self::LoadError(error)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LoadError(error) => std::fmt::Display::fmt(error, f),
            Self::ArgumentCountMismatch { expected, actual } => write!(f, "expected {} arguments but got {}", expected, actual),
            Self::CallStackUnderflow => f.write_str("call stack underflow occured"),
            Self::DirectAbstractMethodCall => write!(f, "attempt to call abstract method with call, when call.virt should have been used instead"),
            Self::UnexpectedEndOfBlock => write!(f, "end of block unexpectedly reached"),
        }
    }
}

impl std::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;

//struct StackTrace

struct StackFrame<'l> {
    depth: usize,
    argument_registers: Vec<Register>,
    result_registers: Vec<Register>,
    current_method: LoadedMethod<'l>,
    code: &'l format::Code,
    block_index: Option<usize>,
    instructions: &'l [format::instruction_set::Instruction],
    temporary_registers: Vec<Register>,
}

impl<'l> StackFrame<'l> {
    fn new(
        depth: usize,
        argument_registers: Vec<Register>,
        result_registers: Vec<Register>,
        current_method: LoadedMethod<'l>,
        code: &'l format::Code,
    ) -> Self {
        Self {
            depth,
            argument_registers,
            result_registers,
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

    //fn lookup_register
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
    ) -> Result<&[Register]> {
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
                Ok(&self.current_frame()?.result_registers)
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

    fn execute_instruction(&mut self, instruction: &'l Instruction) -> Result<()> {
        let current_frame = self.current_frame()?;

        match instruction {
            Instruction::Nop => (),
            Instruction::Ret(indices) => {
                todo!()
            }
            Instruction::ConstI(value) => current_frame.define_temporary(Register::from(*value)),
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

    todo!()
}

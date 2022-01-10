use super::{JumpTarget, RegisterIndex, StackTrace};

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
    LoadError(getmdl::loader::Error),
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

error_kind_conversion!(getmdl::loader::Error, LoadError);
error_kind_conversion!(ProgramHalt, Halt);

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LoadError(error) => std::fmt::Display::fmt(error, f),
            Self::CallStackUnderflow => f.write_str("call stack underflow occured"),
            Self::CallStackOverflow => f.write_str("exceeded maximum call stack depth"),
            Self::UnexpectedEndOfBlock => write!(f, "end of block unexpectedly reached"),
            Self::UndefinedRegister(RegisterIndex::Input(index)) => {
                write!(f, "undefined input register {}", index)
            }
            Self::UndefinedRegister(RegisterIndex::Temporary(index)) => {
                write!(f, "undefined temporary register {}", index)
            }
            Self::UndefinedBlock(index) => write!(f, "undefined block {}", index.0),
            Self::InputCountMismatch { expected, actual } => {
                write!(f, "expected {} input values but got {}", expected, actual)
            }
            Self::ResultCountMismatch { expected, actual } => {
                write!(f, "expected {} result values but got {}", expected, actual)
            }
            Self::Halt(reason) => write!(f, "program execution halted, {}", reason),
        }
    }
}

impl std::error::Error for ErrorKind {}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    stack_trace: Vec<StackTrace>,
}

impl Error {
    pub(crate) fn new(kind: ErrorKind, stack_trace: Vec<StackTrace>) -> Self {
        Self { kind, stack_trace }
    }

    pub(crate) fn with_no_stack_trace(kind: ErrorKind) -> Self {
        Self::new(kind, Vec::new())
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

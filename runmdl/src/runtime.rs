use crate::interpreter;
use getmdl::loader;

pub struct Runtime<'l> {
    loader: &'l loader::Loader<'l>,
    program: &'l loader::Module<'l>,
}

#[derive(Default)]
pub struct Initializer<'l> {
    runtime: Option<Runtime<'l>>,
    loader: Option<loader::Loader<'l>>,
}

impl<'l> Initializer<'l> {
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    MissingEntryPoint,
    InterpreterError(interpreter::Error),
    InvalidReturnValueCount(usize),
    InvalidReturnValueType(interpreter::register::TryFromRegisterValueError),
}

impl From<loader::LoadError> for Error {
    fn from(error: loader::LoadError) -> Self {
        Self::InterpreterError(interpreter::Error::LoadError(error))
    }
}

impl From<interpreter::Error> for Error {
    fn from(error: interpreter::Error) -> Self {
        Self::InterpreterError(error)
    }
}

impl<'l> Runtime<'l> {
    pub fn initialize(
        initializer: &'l mut Initializer<'l>,
        application: registir::format::Module,
    ) -> &'l Self {
        let (loader, program) = loader::Loader::initialize(&mut initializer.loader, application);
        initializer.runtime.insert(Self { loader, program })
    }

    /// Interprets the entry point of the program, supplying the specified arguments.
    pub fn invoke_entry_point(
        &'l self,
        argv: &[&str],
        //max_stack_capacity: usize,
    ) -> Result<i32, Error> {
        if !argv.is_empty() {
            todo!("Command line arguments are not yet supported")
        }

        let entry_point = self
            .program
            .entry_point()?
            .ok_or(Error::MissingEntryPoint)?;

        let results = interpreter::run(&self.loader, &[], entry_point)?;

        match results.as_slice() {
            [] => Ok(0),
            [exit_code] => i32::try_from(exit_code).map_err(Error::InvalidReturnValueType),
            _ => Err(Error::InvalidReturnValueCount(results.len())),
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingEntryPoint => {
                f.write_str("the entry point method of the module is not defined")
            }
            Self::InterpreterError(error) => std::fmt::Display::fmt(error, f),
            Self::InvalidReturnValueCount(count) => {
                write!(f, "expected at most 1 return values but got {}", count)
            }
            Self::InvalidReturnValueType(error) => {
                write!(f, "invalid return value type, {}", error)
            }
        }
    }
}

impl std::error::Error for Error {}

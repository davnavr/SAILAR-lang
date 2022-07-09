//! Contains types representing errors encountered during execution of SAILAR byte code.

///// Represents an error that occured during execution of SAILAR byte code.
//pub struct InterpreterError(Box<InterpreterErrorInner>);

//impl InterpreterError {
//    pub fn stack_trace
//}

/// The union of all errors that can occur during execution of the SAILAR virtual machine.
///
/// Typically handled by terminating execution of the interpreter.
#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum RuntimeError {
    #[error(transparent)]
    LoaderError(#[from] sailar_load::error::LoaderError),
}

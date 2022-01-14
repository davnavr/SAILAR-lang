use crate::interpreter;

pub use interpreter::{
    BlockIndex, InstructionLocation, LoadedFunction, Register, StackTrace,
};

pub use getmdl::loader::{FunctionSymbol, ModuleIdentifier, Symbol};

#[derive(Clone, Copy, Debug)]
pub enum Reply {
    /// Indicates that the interpreter can continue execution until another breakpoint is hit.
    Continue,
    Detach,
    /// Indicates that execution should not continue.
    Wait,
}

pub trait Debugger {
    fn inspect(&mut self, interpreter: &mut interpreter::Interpreter) -> Reply;
}

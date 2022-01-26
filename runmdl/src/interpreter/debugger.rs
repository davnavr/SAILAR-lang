use crate::interpreter;

pub use interpreter::{
    call_stack::{Breakpoint, Trace as StackTrace},
    BlockIndex, InstructionLocation, LoadedFunction, Register,
};

pub use getmdl::loader::{FunctionSymbol, ModuleIdentifier, ModuleSymbol, Symbol};

#[derive(Clone, Copy, Debug)]
pub enum Reply {
    /// Indicates that the interpreter can continue execution until another breakpoint is hit.
    Continue,
    Detach,
}

pub trait Debugger {
    fn inspect(&mut self, interpreter: &mut interpreter::Interpreter) -> Reply;
}

impl<T> Debugger for T
where
    T: FnMut(&mut interpreter::Interpreter) -> Reply,
{
    fn inspect(&mut self, interpreter: &mut interpreter::Interpreter) -> Reply {
        self(interpreter)
    }
}

impl Debugger for () {
    fn inspect(&mut self, _: &mut interpreter::Interpreter) -> Reply {
        Reply::Detach
    }
}

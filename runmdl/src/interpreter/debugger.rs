pub use crate::interpreter::{
    BlockIndex, InstructionLocation, Interpreter, LoadedFunction, Register, StackTrace,
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

pub struct Debugger<'l> {
    handler: Option<Box<dyn Fn(&mut Interpreter<'l>) -> Reply + 'l>>,
}

impl<'l> Debugger<'l> {
    pub fn new<D>(handler: D) -> Self
    where
        D: (Fn(&mut Interpreter<'l>) -> Reply) + 'l,
    {
        Self {
            handler: Some(Box::new(handler)),
        }
    }

    pub(crate) fn is_attached(&self) -> bool {
        self.handler.is_some()
    }

    pub(crate) fn detach(&mut self) {
        self.handler = None;
    }

    pub(crate) fn run(&self, interpreter: &mut Interpreter<'l>) -> Reply {
        match &self.handler {
            Some(handler) => handler(interpreter),
            None => Reply::Continue,
        }
    }
}

impl<'l> Default for Debugger<'l> {
    fn default() -> Self {
        Self { handler: None }
    }
}

use super::{InstructionLocation, Register};

/// Describes a stack frame in the call stack.
#[derive(Clone, Debug)]
pub struct StackTrace { // TODO: fields here should be private.
    pub(crate) depth: usize,
    pub(crate) location: InstructionLocation,
    //pub(crate) method: debugger::FullMethodIdentifier,
    pub(crate) input_registers: Box<[Register]>,
    pub(crate) temporary_registers: Box<[Register]>,
}

impl StackTrace {
    pub fn depth(&self) -> usize {
        self.depth
    }

    pub fn location(&self) -> &InstructionLocation {
        &self.location
    }

    // pub fn method(&self) -> &debugger::FullMethodIdentifier {
    //     &self.method
    // }

    pub fn input_registers(&self) -> &[Register] {
        &self.input_registers
    }

    pub fn temporary_registers(&self) -> &[Register] {
        &self.temporary_registers
    }
}

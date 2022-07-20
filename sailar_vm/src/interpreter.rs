//! The SAILAR virtual machine bytecode interpreter, responsible for executing code.

use crate::call_stack;
use crate::error;
use crate::runtime::{self, Runtime};
use crate::value::Value;
use sailar_load::code_block::TypedInstruction;
use std::fmt::{Debug, Formatter};
use std::sync::Arc;

/// Encapsulates all thread-local state needed to execute a single thread of SAILAR virtual machine bytecode.
pub struct State {
    runtime: Arc<Runtime>,
    call_stack: call_stack::Stack,
}

impl State {
    pub(crate) fn new(
        runtime: Arc<Runtime>,
        entry_point: runtime::Function,
        arguments: Box<[Value]>,
    ) -> Result<Self, error::RuntimeError> {
        let mut call_stack = call_stack::Stack::with_size(runtime.call_stack_size());
        call_stack.push_new(entry_point, arguments)?;
        Ok(Self { runtime, call_stack })
    }

    pub fn runtime(&self) -> &Arc<Runtime> {
        &self.runtime
    }

    pub fn call_stack(&self) -> &call_stack::Stack {
        &self.call_stack
    }

    pub(crate) fn step(&mut self) -> Result<Option<Box<[Value]>>, error::RuntimeError> {
        // TODO: How to generate list of stack frames in debugger if current_frame is popped?
        let mut current_frame = self.call_stack.pop();

        enum ControlFlow {
            Nothing,
            //Branch(Arc<sailar_load::code_block::Code>),
            Return(Box<[Value]>),
        }

        let control_flow = match current_frame.kind_mut() {
            call_stack::FrameKind::Defined(code) => match code.next_instruction()?.expect("missing terminator instruction") {
                TypedInstruction::Nop => ControlFlow::Nothing,
                TypedInstruction::Break => {
                    // TODO: Add support for breakpoints
                    ControlFlow::Nothing
                }
                TypedInstruction::Return(return_values) => {
                    ControlFlow::Return(code.map_many_typed_values(return_values.iter(), self.runtime.endianness()))
                }
            },
        };

        match control_flow {
            ControlFlow::Nothing => self.call_stack.push(current_frame),
            ControlFlow::Return(return_values) => {
                // Frame was already popped, so stack doesn't need to be manipulated.
                if !self.call_stack.is_execution_ended() {
                    let previous_frame = self.call_stack.pop();
                    let expected_result_types = current_frame.return_types()?;

                    assert_eq!(return_values.len(), expected_result_types.len());

                    // TODO: Add check to ensure value has correct type when returning from a foreign function
                    // .zip(expected_result_types)
                    for value in return_values.iter() {
                        // TODO: Call helper that defines temporary registers to store return_values in previous_frame
                    }

                    todo!("handle normal returns");
                    self.call_stack.push(previous_frame);
                } else {
                    return Ok(Some(return_values));
                }
            }
        }

        Ok(None)
    }
}

impl Debug for State {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("State").field("call_stack", &self.call_stack).finish()
    }
}

//! The SAILAR virtual machine bytecode interpreter, responsible for executing code.

use crate::call_stack;
use crate::error;
use crate::runtime::{self, Runtime};
use crate::value::Value;
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
        call_stack.push(entry_point, arguments)?;
        Ok(Self { runtime, call_stack })
    }

    pub fn call_stack(&self) -> &call_stack::Stack {
        &self.call_stack
    }

    pub(crate) fn step(&mut self) -> Result<Option<Box<[Value]>>, error::RuntimeError> {
        let mut current_frame = self.call_stack.pop();
        let pop_frame;

        match current_frame.location_mut() {
            call_stack::FrameLocation::Defined(code) => {
                match code.next_instruction()?.expect("missing terminator instruction") {
                bad => todo!("interpret {:?}", bad),
            }

            pop_frame = false;
        },
        }

        if !pop_frame {
            //self.call_stack.push(current_frame, arguments: Box<[value::Value]>)
            todo!("handle return instruction")
        } else if self.call_stack.is_execution_ended() {
            return Ok(Some(todo!("some return value")));
        }

        Ok(None)
    }
}

impl Debug for State {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("State").field("call_stack", &self.call_stack).finish()
    }
}

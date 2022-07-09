//! Module to interact with the SAILAR virtual machine runtime.

use crate::call_stack;
use crate::error;
use crate::interpreter;
use crate::value;
use std::sync::Arc;

pub type Result<T> = std::result::Result<T, error::RuntimeError>;

/// The loader is responsible for resolving references to and validating SAILAR modules.
pub type Loader = Arc<sailar_load::state::State>;

pub type Function = Arc<sailar_load::function::Instantiation>;

/// Encapsulates all state needed to execute the SAILAR virtual machine.
#[derive(Debug)]
pub struct Runtime {
    call_stack_size: call_stack::Size,
    // TODO: Could have hash_map that maps threads to their interpreter state?
}

impl Runtime {
    pub fn call_stack_size(&self) -> call_stack::Size {
        self.call_stack_size
    }

    fn execute_entry_point(
        self: &Arc<Self>,
        entry_point: Function,
        arguments: Box<[value::Value]>,
    ) -> Result<Box<[value::Value]>> {
        let mut state = interpreter::State::new(self.clone(), entry_point, arguments)?;

        loop {
            match state.step()? {
                Some(return_values) => return Ok(return_values),
                None => continue,
            }
        }
    }

    /// Interprets the specified function in the current thread, providing the specified `arguments`.
    pub fn execute<E, A>(self: &Arc<Self>, entry_point: E, arguments: A) -> Result<Box<[value::Value]>>
    where
        E: Into<Function>,
        A: Into<Box<[value::Value]>>,
    {
        self.execute_entry_point(entry_point.into(), arguments.into())
    }
}

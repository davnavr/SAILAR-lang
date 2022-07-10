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

/// Used configuring the properties of the SAILAR virtual machine.
#[derive(Clone, Debug)]
#[must_use = "must eventually initialize the runtime"]
pub struct Configuration {
    call_stack_size: call_stack::Size,
}

impl Configuration {
    pub fn new() -> Self {
        Self {
            call_stack_size: call_stack::Size::DEFAULT,
        }
    }

    /// Sets the maximum number of frames in the call stack before a stack overflow occurs.
    pub fn call_stack_size(self, size: call_stack::Size) -> Self {
        Self {
            call_stack_size: size,
            ..self
        }
    }

    pub fn initialize_runtime(self) -> Runtime {
        Runtime {
            call_stack_size: self.call_stack_size,
        }
    }
}

impl Default for Configuration {
    fn default() -> Self {
        Self::new()
    }
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

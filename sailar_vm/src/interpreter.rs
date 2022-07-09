//! The SAILAR virtual machine bytecode interpreter, responsible for executing code.

use crate::call_stack;
use crate::error;
use crate::value::Value;

/// Encapsulates all thread-local state needed to execute a single thread of SAILAR virtual machine bytecode.
#[derive(Debug)]
pub struct State {
    call_stack: call_stack::Stack,
}

fn execute_entry_point(entry_point: crate::Function, arguments: Box<[Value]>) -> crate::Result<Box<[Value]>> {
    let mut state = State {
        call_stack: call_stack::Stack::new(), // TODO: Allow a maximum stack size.
    };

    state.call_stack.push(entry_point, arguments);

    todo!()
}

/// The entry point for the execution of SAILAR byte code. Provides the specified `arguments` to the `entry_point` function, and
/// returns the function's return values.
pub fn execute<E, A>(entry_point: E, arguments: A) -> crate::Result<Box<[Value]>>
where
    E: Into<crate::Function>,
    A: Into<Box<[Value]>>,
{
    execute_entry_point(entry_point.into(), arguments.into())
}

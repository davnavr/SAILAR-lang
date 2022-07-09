//! The SAILAR interpreted virtual machine.

pub mod call_stack;
pub mod error;
pub mod interpreter;
pub mod value;

pub type Result<T> = std::result::Result<T, error::RuntimeError>;

/// The loader is responsible for resolving references to and validating SAILAR modules.
pub type Loader = std::sync::Arc<sailar_load::state::State>;

pub type Function = std::sync::Arc<sailar_load::function::Instantiation>;

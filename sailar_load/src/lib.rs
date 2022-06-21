//! Library for loading, validating, and resolving modules and their imports.
//!
//! The types provided by this module are thread-safe, to allow dependents such as interpreters to use multiple threads.

mod module;
mod source;
mod state;

// pub mod function;
pub mod resolver;

pub use module::{Module, ModuleIdentifier};
pub use source::Source;
pub use state::State;
pub use resolver::Resolver;

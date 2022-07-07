//! Library for loading, validating, and resolving modules and their imports.
//!
//! The types provided by this module are thread-safe, to allow dependents such as interpreters to use multiple threads.

mod state;

pub mod error;
pub mod function;
pub mod module;
pub mod resolver;
pub mod source;
pub mod symbol;
pub mod type_system;

pub use resolver::Resolver;
pub use source::Source;
pub use state::State;

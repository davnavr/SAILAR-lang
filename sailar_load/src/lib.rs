//! Library for loading, validating, and resolving modules and their imports.
//!
//! The types provided by this module are thread-safe, to allow dependents such as interpreters to use multiple threads.

pub mod code_block;
pub mod error;
pub mod function;
pub mod module;
pub mod resolver;
pub mod source;
pub mod state;
pub mod symbol;
pub mod type_system;

pub use resolver::Resolver;

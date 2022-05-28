//! Module for loading, validating, and resolving modules and their imports.
//!
//! The types provided by this module are thread-safe, to allow dependents such as interpreters to use multi-threading.

mod module;
mod source;
mod state;

pub mod function;

pub use module::{Module, ModuleIdentifier};
pub use source::Source;
pub use state::{ModuleLoadError, ModuleLoadResult, State};

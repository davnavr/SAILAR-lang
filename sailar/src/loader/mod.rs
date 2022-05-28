//! Module for loading, validating, and resolving modules and their imports.

mod module;
mod source;
mod state;

pub mod function;

pub use module::{Module, ModuleIdentifier};
pub use source::Source;
pub use state::{ModuleLoadError, ModuleLoadResult, State};

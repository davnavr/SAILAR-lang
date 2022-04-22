//! Responsible for loading and resolving references to SAILAR modules.

mod function;
mod loader;
mod module;

pub mod error;
pub mod resolver;

pub use error::SymbolNotFoundError;
pub use function::ResolvedFunction;
pub use loader::ModuleLoader;
pub use module::{ResolvedModule, ResolvedDefinition};
pub use resolver::Resolver;

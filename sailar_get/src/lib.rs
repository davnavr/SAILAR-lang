//! Responsible for loading and resolving references to SAILAR modules.

mod block;
mod function;
mod loader;
mod module;

pub mod error;
pub mod resolver;

pub use block::{Block as ResolvedBlock, ResolvedInstruction};
pub use error::SymbolNotFoundError;
pub use function::ResolvedFunction;
pub use loader::ModuleLoader;
pub use module::{ResolvedDefinition, ResolvedModule};
pub use resolver::Resolver;

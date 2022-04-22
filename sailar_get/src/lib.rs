//! Responsible for loading and resolving references to SAILAR modules.

mod loader;
mod module;

pub mod resolver;

pub use module::ResolvedModule;
pub use resolver::Resolver;

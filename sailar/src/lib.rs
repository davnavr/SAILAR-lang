//! Contains types for reading and writing binary modules.

mod buffers;
/// High-level interface for assembling binary modules.
pub mod builder;
/// Contains types representing the file format of modules.
pub mod format;
pub mod hashing;
/// Contains functions for reading binary modules.
pub mod parser;
/// Contains functions for writing binary modules.
pub mod writer;

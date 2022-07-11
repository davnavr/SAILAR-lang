//! Provides an ahead-of-time compiler for SAILAR bytecode using LLVM.

mod function;
mod helper;
mod signature;
mod transpiler;

pub mod compilation;
pub mod error;

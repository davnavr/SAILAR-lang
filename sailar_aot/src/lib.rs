//! Provides an ahead-of-time compiler for SAILAR bytecode using [LLVM](https://llvm.org/).
//!
//! # Usage
//!
//! Inputs to the compiler are provided using the [`Inputs`] struct, which contains information about the target
//! platform, the SAILAR modules used as input, and other information. These inputs are then compiled by calling
//! [`Inputs::compile_in_context`] or one of ts related functions, to produce an LLVM module as output.
//!
//! # Producing Object Files
//! The main use case for the compiler's output is to produce object files. The resulting object files can then be linked
//! with a linker (such as [`lld`](https://lld.llvm.org/) or your system linker) or even provided to some C compilers to produce
//! an executable or library.
//!
//! # Saving LLVM Bitcode
//! Although not the main way the output of compilation is intended to be executed, the resulting LLVM bitcode can be written to
//! disk by calling [`inkwell::module::Module::write_bitcode_to_file`]. This LLVM bitcode can then be compiled into assembly
//! language using tools included with LLVM such as [`llc`](https://www.llvm.org/docs/CommandGuide/llc.html), or interpreted using
//! [`lli`](https://www.llvm.org/docs/CommandGuide/lli.html).
//!
//! [`Inputs`]: compilation::Inputs
//! [`Inputs::compile_in_context`]: compilation::Inputs::compile_in_context

mod function;
mod helper;
mod name_mangling;
mod signature;
mod transpiler;

pub mod compilation;
pub mod error;
pub mod target;

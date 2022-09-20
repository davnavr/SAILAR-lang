# [DEVELOPMENT HAS BEEN MOVED](https://github.com/davnavr/il4il)

# SAILAR
**S**tatic **A**ssignment **I**ntermediate **L**anguage **A**nd **R**epresentation

A virtual machine and bytecode format, like Java bytecode, WebAssembly, or CIL, but with registers.

This project aims to eventually provide:
- Easy compilation to other languages, including LLVM IR, WebAssembly, CIL, and JVM bytecode
- Reified generics while also not limiting the type system of the source language
- Bindings to allow generation of modules and bytecode in languages other than Rust (C, C#, and other languages with FFI)
- A simple debugging information format that allows easy translation to other formats (DWARF, Windows PDBs, etc.)

# NEW NAME NEEDED

This project's name is currently way too similar to [SAIL](https://github.com/rems-project/sail), so a new name to replace SAILAR
will be made eventually.

# Getting Started

## Building

First, install [the Rust programming language](https://www.rust-lang.org/tools/install) on your system, then navigate to the
SAILAR repository and run the following:

```bash
cargo build --release
```

If you need support for AOT compilation, install LLVM 13.0.0 before building [`sailar_aot`](../main/sailar_aot/). Make sure the
`LLVM_SYS_130_PREFIX` environment variable is set before running `cargo build`.

A command line application to compile SAILAR programs will eventually be implemented.

## Writing Programs

Currently, the assembly language is very incomplete after the restructure, so the only way to write SAILAR code is with the
`sailar` crate.

For sample programs that create SAILAR modules, see [`sailar_samples`](../main/sailar_samples/).

## Running Programs

A command line application for interpreting programs is currently not implemented, but [`sailar_vm`](../main/sailar_vm/) can
currently be used to allow using the interpreter as a library.

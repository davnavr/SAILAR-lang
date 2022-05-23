# SAILAR
**S**tatic **A**ssignment **I**ntermediate **L**anguage **A**nd **R**epresentation

A virtual machine and bytecode format, like Java bytecode, WebAssembly, or CIL, but with registers.

This project aims to eventually provide:
- Easy compilation to other languages, including LLVM IR, WebAssembly, CIL, and JVM bytecode
- Reified generics while also not limiting the type system of the source language
- Bindings to allow generation of modules and bytecode in languages other than Rust (C, C#, and other languages with FFI)
- A simple debugging information format that allows easy translation to other formats (DWARF, Windows PDBs, etc.)

# NEW NAME NEEDED

This project's name is currently way too similar to [SAIL](https://github.com/rems-project/sail), so a new name to replace SAILAR will be made eventually.

# Getting Started

## Building

If the interpreter and bytecode reading/writing library is all you need, simply install [the Rust programming language](https://www.rust-lang.org/tools/install) on your system, then navigate to the SAILAR repository and run the following:
```bash
cargo build --release
```

Whem support for AOT compilation is needed in the future, install LLVM 13.0.0 before building `sailar_aot`. Currently, the [llvm-sys crate](https://crates.io/crates/llvm-sys) is used to call the LLVM C APIs, so make sure the `LLVM_SYS_130_PREFIX` environment variable is set before running `cargo build`. After the restructure, support for AOT compilation via LLVM is not yet implemented.

## Writing Programs

Currently, the assembly language is very incomplete after the restructure, so the only way to write SAILAR code is with the `sailar` crate.

## Running Programs

SAILAR module files are interpreted using [`saili`](../main/saili/). After the restructure, the interpreter currently is not implemented.

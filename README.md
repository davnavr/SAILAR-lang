# SAILAR
**S**tatic **A**ssignment **I**ntermediate **L**anguage **A**nd **R**epresentation

A virtual machine and bytecode format, like Java bytecode, WebAssembly, or CIL, but with registers.

# Getting Started

## Building

If the interpreter and bytecode reading/writing library is all you need, simply install [the Rust programming language](https://www.rust-lang.org/tools/install) on your system, then navigate to the SAILAR repository and run the following:
```bash
cargo build --release
```

If support for AOT compilation is needed, install LLVM 13.0.0 before building `sailar_aot`. Currently, the [llvm-sys crate](https://crates.io/crates/llvm-sys) is used to call the LLVM C APIs, so make sure the `LLVM_SYS_130_PREFIX` environment variable is set before running `cargo build`.

## Writing Programs

Currently, the assembly language is outdated, so the only way to write SAILAR code is with the `sailar` crate.

Examples using the builder API to generate SAILAR code programatically is [available here](../main/sailar_vm/examples/).

## Running Programs

SAILAR module files are interpreted using [`saili`](../main/saili/).

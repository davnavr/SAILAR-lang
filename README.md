# SAILAR
**S**tatic **A**ssignment **I**ntermediate **L**anguage **A**nd **R**epresentation

A virtual machine and bytecode format, like Java bytecode, WebAssembly, or CIL, but with registers.

# Getting Started

## Building

First, install [the Rust programming language](https://www.rust-lang.org/tools/install) on your system, then navigate to the SAILAR repository and run the following:
```bash
cargo build --release
```

## Writing Programs

The [`sailas`](../main/sailas/) tool assembles `.txtmdl` files into `.binmdl` files. For sample programs, see [the samples directory](../main/sailas/samples/).

## Running Programs

Assembled `.binmdl` files are then interpreted with [`saili`](../main/saili/).

A "server-less" interactive web version of `sailas`, the SAILAR module assembler.

# Building

To build and serve, the following dependencies are needed:
- [`wasm-pack`](https://rustwasm.github.io/wasm-pack/) 
 - [`https`](https://crates.io/crates/https) to serve the application
 - [`just`](https://crates.io/crates/just)
 
Simply run the following command:
```bash
just serve
```

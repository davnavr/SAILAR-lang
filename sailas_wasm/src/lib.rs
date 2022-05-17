#![doc = include_str!("../README.md")]

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct Output;

#[wasm_bindgen]
pub fn assemble(input: &str) -> Output {
    todo!("output {}", input)
}

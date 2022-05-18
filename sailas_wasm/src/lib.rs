#![doc = include_str!("../README.md")]

use wasm_bindgen::prelude::*;

type JsArray = js_sys::Array;
type JsFunction = js_sys::Function;
type JsResult<T> = Result<T, JsValue>;

struct PrintWrapper {
    output_function: JsFunction,
    error_function: JsFunction,
}

impl PrintWrapper {
    fn print_output(&self, message: &str) -> JsResult<()> {
        self.output_function
            .call1(&JsValue::NULL, &JsValue::from_str(message))
            .map(|_| ())
    }

    fn print_error(&self, message: &str, location: Option<&sailasm::ast::LocationRange>) -> JsResult<()> {
        let js_location = location.map(|location| {
            JsArray::from_iter([
                &JsValue::from(location.start().line.get()),
                &JsValue::from(location.start().column.get()),
                &JsValue::from(location.end().line.get()),
                &JsValue::from(location.end().column.get()),
            ])
        });

        self.error_function
            .call2(
                &JsValue::NULL,
                &JsValue::from_str(message),
                js_location
                    .as_ref()
                    .map(|location| location.as_ref())
                    .unwrap_or(&JsValue::NULL),
            )
            .map(|_| ())
    }
}

#[wasm_bindgen]
pub fn register_panic_hook() {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));
}

#[wasm_bindgen]
pub fn assemble(input: &str, print_output: JsFunction, print_error: JsFunction) -> JsResult<()> {
    let print_wrapper = PrintWrapper {
        output_function: print_output,
        error_function: print_error,
    };

    match sailasm::assemble(input) {
        Ok(module) => Ok(()), // TODO: Output the module.
        Err(errors) => {
            use std::fmt::Write as _;

            let mut message_buffer = String::default();
            for e in errors.iter() {
                message_buffer.clear();
                write!(&mut message_buffer, "{}", e.kind()).unwrap();
                print_wrapper.print_error(&message_buffer, e.location())?;
            }

            Ok(())
        }
    }
}

#![doc = include_str!("../README.md")]

use wasm_bindgen::prelude::*;

type JsArray = js_sys::Array;
type JsFunction = js_sys::Function;
type JsResult<T> = Result<T, JsValue>;

struct PrintWrapper {
    error_function: JsFunction,
}

impl PrintWrapper {
    fn print_error(&self, message: &str, location: Option<&sailasm::ast::LocationRange>) -> JsResult<()> {
        fn convert_location_number(number: sailasm::ast::LocationNumber) -> JsResult<JsValue> {
            Ok(JsValue::from(
                u32::try_from(number.get()).map_err(|e| JsValue::from(e.to_string()))?,
            ))
        }

        let js_location = match location {
            Some(location) => Some(JsArray::from_iter([
                &convert_location_number(location.start().line)?,
                &convert_location_number(location.start().column)?,
                &convert_location_number(location.end().line)?,
                &convert_location_number(location.end().column)?,
            ])),
            None => None,
        };

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
pub fn assemble(input: &str, print_error: JsFunction, write_output: JsFunction) -> JsResult<()> {
    let print_wrapper = PrintWrapper {
        error_function: print_error,
    };

    let mut parse_tree = None;

    match sailasm::assemble(input, &mut parse_tree) {
        Ok(module) => {
            let mut buffer = Vec::default();
            module.write_to(&mut buffer).unwrap();
            write_output
                .call1(&JsValue::NULL, &js_sys::Uint8Array::from(buffer.as_slice()))
                .map(|_| ())
        }
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

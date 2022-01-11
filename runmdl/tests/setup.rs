use runmdl::runtime::{self, Runtime};

pub fn initialize_from_str<F: for<'a> FnOnce(&'a Runtime<'a>) -> ()>(module: &str, body: F) {
    let mut initializer = runtime::Initializer::new();
    let runtime = Runtime::initialize(
        &mut initializer,
        asmdl::assembler::assemble_from_str(module).unwrap(),
    );
    body(&runtime)
}

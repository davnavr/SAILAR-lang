use sailar_vm::runtime::{self, Runtime};

pub fn initialize_from_str<
    D,
    S: FnOnce(&sailar::format::Module, &mut runtime::Initializer) -> D,
    F: for<'a> FnOnce(&'a mut D, &'a Runtime<'a>) -> (),
>(
    module: &str,
    setup: S,
    body: F,
) {
    let program = sailar_asm::assembler::assemble_from_str(module).unwrap();
    let mut initializer = runtime::Initializer::new();
    let mut debugger = setup(&program, &mut initializer);
    let runtime = Runtime::initialize(&mut initializer, program);

    body(&mut debugger, &runtime);
}

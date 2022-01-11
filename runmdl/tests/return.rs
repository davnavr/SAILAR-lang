use getmdl::loader::Identifier;
use runmdl::runtime::{self, Runtime};

fn initialize_from_str<F: for<'a> FnOnce(&'a Runtime<'a>) -> ()>(module: &str, body: F) {
    let mut initializer = runtime::Initializer::new();
    let runtime = Runtime::initialize(
        &mut initializer,
        asmdl::assembler::assemble_from_str(module).unwrap(),
    );
    body(&runtime)
}

#[test]
fn has_entry_point_symbol() {
    initialize_from_str(
        include_str!(r"../../asmdl_cli/samples/return.txtmdl"),
        |runtime| {
            assert!(runtime
                .program()
                .lookup_function(Identifier::try_from("TODO: Use different example").unwrap())
                .is_some())
        },
    );
}

#[test]
fn returns_exit_code() {
    initialize_from_str(
        include_str!(r"../../asmdl_cli/samples/return.txtmdl"),
        |runtime| assert_eq!(0, runtime.invoke_entry_point(&[], None).unwrap()),
    );
}

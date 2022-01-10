#[test]
fn returns_exit_code() {
    let mut initializer = runmdl::runtime::Initializer::new();
    let runtime = runmdl::runtime::Runtime::initialize(
        &mut initializer,
        asmdl::assembler::assemble_from_str(include_str!(r"../../asmdl_cli/samples/return.txtmdl"))
            .unwrap(),
    );

    assert_eq!(0, runtime.invoke_entry_point(&[], None).unwrap());
}

use sailar::{
    builder,
    format::{self, type_system},
};

/// Showcases building and executing an extremely simple program that returns an exit code.
fn main() -> Result<(), Box<dyn std::error::Error>> {
    const EXPECTED_EXIT_CODE: i32 = 1;

    let program = {
        let mut builder = builder::Builder::new(format::Identifier::try_from("True")?);

        let entry_code = {
            let code = builder.code().define(Vec::new(), 1);
            let entry_block = code.entry_block();
            let exit_code = entry_block.const_i(EXPECTED_EXIT_CODE);
            entry_block.ret([exit_code])?;
            code
        };

        let entry_point = builder.definitions().functions().define(
            format::Identifier::try_from("Main")?,
            builder.function_signatures().insert(
                vec![builder
                    .type_signatures()
                    .primitive(type_system::FixedInt::S32)],
                Vec::new(),
            ),
            builder::FunctionBody::Defined(entry_code),
        );

        builder.set_entry_point(entry_point);

        builder.finish()
    };

    let exit_code = sailar_vm::runtime::execute::<(), _>(|_, _| (), program, &[])?;
    assert_eq!(exit_code, EXPECTED_EXIT_CODE);

    Ok(())
}

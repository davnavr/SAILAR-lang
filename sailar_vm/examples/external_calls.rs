use sailar::{
    builder,
    format::{self, type_system},
};

/// Showcases building and executing an extremely simple program that returns an exit code.
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program = {
        let mut builder = builder::Builder::new(format::Identifier::try_from("Hello")?);
        let message = builder.data().define(Box::new(b"Hello World!\n".clone()));

        let helper = builder.definitions().functions().define(
            format::Identifier::try_from("Helper")?,
            builder.function_signatures().insert(
                vec![builder
                    .type_signatures()
                    .primitive_type(type_system::FixedInt::S32)],
                Vec::new(),
            ),
            builder::FunctionBody::from(builder::ExternalFunction::new(
                std::rc::Rc::new(format::Identifier::try_from("saili")?),
                format::Identifier::try_from("print")?,
            )),
        );

        builder.finish()
    };

    dbg!(&program);

    let exit_code = sailar_vm::runtime::execute::<(), _>(|_, _| (), program, &[])?;
    assert_eq!(exit_code, 0);

    Ok(())
}

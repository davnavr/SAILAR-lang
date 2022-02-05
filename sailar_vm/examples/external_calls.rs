use sailar::{
    builder,
    format::{self, type_system},
};

/// Showcases building and executing an extremely simple program that returns an exit code.
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program = {
        let mut builder = builder::Builder::new(format::Identifier::try_from("Hello")?);
        let message = builder.data().define(Box::new(b"Hello World!\n".clone()));

        builder.finish()
    };

    dbg!(&program);

    let exit_code = sailar_vm::runtime::execute::<(), _>(|_, _| (), program, &[])?;
    assert_eq!(exit_code, 0);

    Ok(())
}

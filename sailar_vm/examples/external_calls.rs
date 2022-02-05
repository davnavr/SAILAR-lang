use sailar::{
    builder,
    format::{self, type_system},
};

/// Showcases building and executing an extremely simple program that returns an exit code.
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program = {
        let mut builder = builder::Builder::new(format::Identifier::try_from("Hello")?);

        let byte_type = builder
            .type_signatures()
            .primitive(type_system::FixedInt::U8);

        let message_data = builder.data().define(Box::new(b"Hello World!\n".clone()));

        let helper = builder.definitions().functions().define(
            format::Identifier::try_from("Print")?,
            builder.function_signatures().insert(
                Vec::new(),
                vec![builder.type_signatures().native_pointer(byte_type.clone()), builder.type_signatures().primitive(type_system::Int::UNative)],
            ),
            builder::FunctionBody::from(builder::ExternalFunction::new(
                std::rc::Rc::new(format::Identifier::try_from("saili")?),
                format::Identifier::try_from("print")?,
            )),
        );

        let entry_code = {
            let code = builder.code().define(Vec::new(), 1);
            let entry_block = code.entry_block();
            let message_register = entry_block.alloca(
                entry_block.const_i(u32::try_from(message_data.bytes().len()).unwrap()),
                byte_type,
            );
            entry_block.mem_init_from_data(message_register, message_data);
            entry_block.call(&builder::Function::Defined(helper), [message_register])?;
            entry_block.ret(&[])?;
            code
        };

        let entry_point = builder.definitions().functions().define(
            format::Identifier::try_from("Main")?,
            builder.function_signatures().insert(Vec::new(), Vec::new()),
            builder::FunctionBody::Defined(entry_code),
        );

        builder.set_entry_point(entry_point);

        builder.finish()
    };

    let exit_code = sailar_vm::runtime::execute::<(), _>(|_, _| (), program, &[])?;
    assert_eq!(exit_code, 0);

    // TODO: Check stdout

    Ok(())
}

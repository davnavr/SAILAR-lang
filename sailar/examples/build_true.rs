use sailar::{builder, format};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program = {
        let mut builder = builder::Builder::new(format::Identifier::try_from("True")?);

        let entry_code = {
            let code = builder.code().define(0, 1);
            let entry_block = code.entry_block();
            let exit_code = entry_block.const_i(1);
            entry_block.ret([exit_code])?;
            code
        };

        // let entry_point;
        // {
        //     let mut definitions = builder.definitions();
        //     let mut functions = definitions.functions();
        //     let mut main_function = functions.define(
        //         format::Identifier::try_from("Main")?,
        //         &builder::FunctionBody::Defined(entry_code),
        //     );

        //     entry_point = main_function.finish();
        // }

        builder.finish()
    };

    todo!("{:?}", program);
    Ok(())
}

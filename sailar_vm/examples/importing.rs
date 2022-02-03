use sailar::{
    builder,
    format::{self, type_system},
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let helper_name = format::Identifier::try_from("Helper")?;

    let library = {
        let mut builder = builder::Builder::new(format::Identifier::try_from("Library")?);

        let helper_code = {
            let code = builder.code().define(
                vec![builder
                    .type_signatures()
                    .primitive_type(type_system::FixedInt::S32)],
                1,
            );
            let entry_block = code.entry_block();
            // TODO: Get input register
            code
        };

        builder.definitions().functions().define(
            helper_name.clone(),
            {
                let signature = vec![builder
                    .type_signatures()
                    .primitive_type(type_system::FixedInt::S32)];
                builder
                    .function_signatures()
                    .insert(signature.clone(), signature)
            },
            builder::FunctionBody::Defined(helper_code),
        );

        builder.finish()
    };

    let application = {
        let mut builder = builder::Builder::new(format::Identifier::try_from("True")?);

        let library_import = builder
            .imports()
            .modules()
            .get_or_import(&library.header.0.identifier);

        let helper = builder
            .imports()
            .functions()
            .import(library_import, helper_name, {
                let signature = vec![builder
                    .type_signatures()
                    .primitive_type(format::type_system::FixedInt::S32)];
                builder
                    .function_signatures()
                    .insert(signature.clone(), signature)
            });

        let entry_code = {
            let code = builder.code().define(Vec::new(), 1);
            let entry_block = code.entry_block();
            let value = entry_block.const_i(9);
            let result = entry_block.call(&builder::Function::Imported(helper), [value])?;
            entry_block.ret([result[0]])?;
            code
        };

        let entry_point = builder.definitions().functions().define(
            format::Identifier::try_from("Main")?,
            builder.function_signatures().insert(
                vec![builder
                    .type_signatures()
                    .primitive_type(type_system::FixedInt::S32)],
                Vec::new(),
            ),
            builder::FunctionBody::Defined(entry_code),
        );

        builder.set_entry_point(entry_point);

        builder.finish()
    };

    //let exit_code = sailar_vm::runtime::execute(|_| (), program, &[])?;
    //assert_eq!(exit_code, EXPECTED_EXIT_CODE);
    todo!();
    Ok(())
}
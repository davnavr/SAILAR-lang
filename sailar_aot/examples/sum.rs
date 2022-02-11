type SumFn = unsafe extern "C" fn(i32, i32) -> i32;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program = {
        use sailar::builder;
        use sailar::format::{self, type_system};

        let mut builder = builder::Builder::new(format::Identifier::try_from("Example")?);
        let type_int = builder
            .type_signatures()
            .primitive(type_system::FixedInt::S32);

        builder.definitions().functions().define(
            format::Identifier::try_from("Sum")?,
            builder
                .function_signatures()
                .insert(vec![type_int.clone(), type_int.clone()], vec![type_int.clone()]),
            builder::FunctionBody::Defined({
                let code = builder
                    .code()
                    .define(vec![type_int.clone(), type_int.clone()], 1);
                let entry_block = code.entry_block();
                let inputs = entry_block.input_registers();
                let result = entry_block.add_overflowing(&inputs[0], &inputs[1])?;
                entry_block.ret([result])?;
                code
            }),
        );

        builder.finish()
    };

    let sum: SumFn = {
        use inkwell::context::Context;
        let context = Context::create();
        let module = sailar_aot::compile(program, &mut (), &context, todo!("pick the target"));

        todo!()
    };

    assert_eq!(unsafe { sum(9, 10) }, 9i32 + 10);

    Ok(())
}

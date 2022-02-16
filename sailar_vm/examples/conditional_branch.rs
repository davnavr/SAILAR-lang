use sailar::{
    builder,
    format::{self, type_system},
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let test_function_name: sailar_get::loader::Symbol =
        std::borrow::Cow::Owned(format::Identifier::try_from("Test")?);

    let program = {
        let builder = builder::Builder::new(format::Identifier::try_from("If")?);

        let int_type = builder
            .type_signatures()
            .primitive(type_system::FixedInt::S32);

        let entry_code = {
            let code = builder
                .code()
                .define(vec![int_type.clone()], vec![int_type.clone()]);
            let true_branch = code.define_block(&[int_type.clone()]);
            let false_branch = code.define_block(&[]);

            {
                let entry_block = code.entry_block();
                let argument = &entry_block.input_registers()[0];
                entry_block.branch_if(argument, true_branch, [argument], false_branch, [])?;
            }

            {
                let argument = &true_branch.input_registers()[0];
                true_branch.ret([true_branch.add_overflowing(argument, argument)?])?;
            }

            {
                false_branch.ret([false_branch.const_i(1i32)])?;
            }

            code
        };

        builder.definitions().functions().define(
            std::borrow::Cow::<format::Identifier>::into_owned(test_function_name.clone()),
            builder
                .function_signatures()
                .insert(vec![int_type.clone()], vec![int_type]),
            builder::FunctionBody::Defined(entry_code),
        );

        builder.finish()
    };

    let mut initializer = sailar_vm::runtime::Initializer::new();
    let mut resolver = ();
    let runtime = sailar_vm::runtime::Runtime::initialize(&mut initializer, &mut resolver, program);

    let test_function = runtime
        .program()
        .lookup_function(test_function_name)
        .expect("test function should be defined");

    assert_eq!(
        runtime.invoke(
            test_function,
            &[sailar_vm::interpreter::Register::from(3i32)],
            None
        )?,
        vec![sailar_vm::interpreter::Register::from(6i32)]
    );

    assert_eq!(
        runtime.invoke(
            test_function,
            &[sailar_vm::interpreter::Register::from(0i32)],
            None
        )?,
        vec![sailar_vm::interpreter::Register::from(1i32)]
    );

    Ok(())
}

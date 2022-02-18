/// Showcases the translation of branching instructions with inputs.
fn main() -> Result<(), Box<dyn std::error::Error>> {
    type TestFn = unsafe extern "C" fn(i32) -> i32;

    let program = {
        use sailar::builder;
        use sailar::format::{self, type_system};

        let builder = builder::Builder::new(format::Identifier::try_from("Branching")?);

        let int_type = builder
            .type_signatures()
            .primitive(type_system::FixedInt::S32);

        let square = builder.definitions().functions().define(
            format::Identifier::try_from("Square")?,
            builder
                .function_signatures()
                .insert(vec![int_type.clone()], vec![int_type.clone()]),
            builder::FunctionBody::Defined({
                let code = builder
                    .code()
                    .define(vec![int_type.clone()], vec![int_type.clone()]);

                let entry_block = code.entry_block();
                let result_block = code.define_block(&[int_type.clone()]);
                entry_block.branch(result_block, [&entry_block.input_registers()[0]])?;
                let result = result_block.mul_overflowing(
                    &result_block.input_registers()[0],
                    &result_block.input_registers()[0],
                )?;
                result_block.ret([result])?;
                code
            }),
        );

        //let factorial =

        square.is_export(true);

        builder.finish()
    };

    let context = inkwell::context::Context::create();
    let optimization_level = inkwell::OptimizationLevel::None;

    let target = {
        use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};

        Target::initialize_native(&InitializationConfig::default()).unwrap();

        let triple = TargetMachine::get_default_triple();

        Target::from_triple(&triple)
            .unwrap()
            .create_target_machine(
                &triple,
                &TargetMachine::get_host_cpu_name().to_string(),
                &TargetMachine::get_host_cpu_features().to_string(),
                optimization_level,
                RelocMode::Default,
                CodeModel::JITDefault,
            )
            .expect("valid target machine")
    };

    let module = sailar_aot::compile(program, &mut (), &context, &target)?;

    println!("{}", module.print_to_string().to_string());

    let execution_engine = module
        .create_jit_execution_engine(optimization_level)
        .unwrap();

    let square: inkwell::execution_engine::JitFunction<TestFn> =
        unsafe { execution_engine.get_function("Branching_Square")? };

    assert_eq!(9i32, unsafe { square.call(3i32) });
    assert_eq!(100i32, unsafe { square.call(10i32) });

    Ok(())
}

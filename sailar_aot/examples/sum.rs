type SumFn = unsafe extern "C" fn(i32, i32) -> i32;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program = {
        use sailar::builder;
        use sailar::format::{self, type_system};

        let builder = builder::Builder::new(format::Identifier::try_from("Example")?);
        let type_int = builder
            .type_signatures()
            .primitive(type_system::FixedInt::S32);

        let sum = builder.definitions().functions().define(
            format::Identifier::try_from("Sum")?,
            builder.function_signatures().insert(
                vec![type_int.clone()],
                vec![type_int.clone(), type_int.clone()],
            ),
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

        sum.is_export(true);

        builder.finish()
    };

    let context = inkwell::context::Context::create();
    let module;
    let execution_engine;

    let sum: inkwell::execution_engine::JitFunction<SumFn> = {
        let optimization_level = inkwell::OptimizationLevel::None;

        let target = {
            use inkwell::targets::{
                CodeModel, InitializationConfig, RelocMode, Target, TargetMachine,
            };

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

        module = sailar_aot::compile(program, &mut (), &context, &target)?;
        execution_engine = module
            .create_jit_execution_engine(optimization_level)
            .unwrap();

        unsafe { execution_engine.get_function("Example_Sum")? }
    };

    assert_eq!(unsafe { sum.call(9, 10) }, 9i32 + 10);
    println!("{}", module.print_to_string().to_string());

    Ok(())
}

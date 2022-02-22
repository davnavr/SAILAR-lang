/// Showcases the translation of branching instructions with inputs.
fn main() -> Result<(), Box<dyn std::error::Error>> {
    type TestFn = unsafe extern "C" fn(i32) -> i32;

    let program = {
        use sailar::builder;
        use sailar::format::{self, instruction_set::ComparisonKind, type_system};

        let builder = builder::Builder::new(format::Identifier::try_from("Branching")?);

        let int_type = builder
            .type_signatures()
            .primitive(type_system::FixedInt::S32);

        let test_function_signature = builder
            .function_signatures()
            .insert(vec![int_type.clone()], vec![int_type.clone()]);

        let square = builder.definitions().functions().define(
            format::Identifier::try_from("Square")?,
            test_function_signature.clone(),
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

        let factorial = builder.definitions().functions().define(
            format::Identifier::try_from("Factorial")?,
            test_function_signature,
            builder::FunctionBody::Defined({
                let code = builder
                    .code()
                    .define(vec![int_type.clone()], vec![int_type.clone()]);

                let entry_block = code.entry_block();
                let one_block = code.define_block(&[]);
                let loop_body = code.define_block(&[int_type.clone(), int_type.clone()]);
                let loop_exit = code.define_block(&[int_type.clone()]);

                {
                    let input = &entry_block.input_registers()[0];
                    let one = entry_block.const_i(1i32);
                    let is_le_1 = entry_block.cmp(
                        input,
                        one,
                        ComparisonKind::LessThanOrEqual,
                    )?;

                    entry_block.branch_if(
                        is_le_1,
                        one_block,
                        [],
                        loop_body,
                        [input, entry_block.const_i(1i32)],
                    )?;
                }

                {
                    let inputs = loop_body.input_registers();
                    let n_0 = &inputs[0];
                    let acc_0 = &inputs[1];
                    let acc_1 = loop_body.mul_overflowing(acc_0, n_0)?;
                    let one = loop_body.const_i(1i32);
                    let n_1 = loop_body.sub_overflowing(n_0, one)?;

                    loop_body.branch_if(
                        loop_body.cmp(one, n_1, ComparisonKind::Equal)?,
                        loop_exit,
                        [acc_1],
                        loop_body,
                        [n_1, acc_1],
                    )?;
                }

                one_block.ret([one_block.const_i(1i32)])?;
                loop_exit.ret([&loop_exit.input_registers()[0]])?;
                code
            }),
        );

        square.is_export(true);
        factorial.is_export(true);
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

    let factorial: inkwell::execution_engine::JitFunction<TestFn> =
        unsafe { execution_engine.get_function("Branching_Factorial")? };

    assert_eq!(1i32, unsafe { factorial.call(0i32) });
    assert_eq!(1i32, unsafe { factorial.call(1i32) });
    assert_eq!(6i32, unsafe { factorial.call(3i32) });
    assert_eq!(5040i32, unsafe { factorial.call(7i32) });

    Ok(())
}

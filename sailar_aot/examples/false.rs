fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program = {
        use sailar::builder;
        use sailar::format::{type_system, Identifier};

        let mut builder = builder::Builder::new(Identifier::try_from("False")?);

        let type_int = builder
            .type_signatures()
            .primitive(type_system::FixedInt::S32);

        let main = builder.definitions().functions().define(
            Identifier::try_from("Main")?,
            builder
                .function_signatures()
                .insert(vec![type_int.clone()], Vec::new()),
            builder::FunctionBody::Defined({
                let code = builder.code().define(Vec::new(), vec![type_int]);

                let entry_block = code.entry_block();
                entry_block.ret([entry_block.const_i(1i32)])?;
                code
            }),
        );

        builder.set_entry_point(main);
        builder.finish()
    };

    let context = inkwell::context::Context::create();

    let module = {
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
                    CodeModel::Default,
                )
                .expect("valid target machine")
        };

        sailar_aot::compile(program, &mut (), &context, &target)?
    };

    assert!(module.write_bitcode_to_path(&std::path::Path::new("false.bc")));
    Ok(())
}

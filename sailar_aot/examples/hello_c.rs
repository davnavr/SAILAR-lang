/// Showcases the usage of external functions when compiling to LLVM.
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program = {
        use sailar::{
            builder,
            format::{self, type_system},
        };

        let mut builder = builder::Builder::new(format::Identifier::try_from("Message")?);

        let byte_type = builder
            .type_signatures()
            .primitive(type_system::FixedInt::U8);

        // TODO: Make sure this matches the C `int` type.
        let int_type = builder
            .type_signatures()
            .primitive(type_system::FixedInt::S32);

        let message_data = builder.data().define(Box::new(b"Hello World!\n".clone()));

        let helper = builder.definitions().functions().define(
            format::Identifier::try_from("PrintC")?,
            builder
                .function_signatures()
                .insert(vec![int_type.clone()], vec![int_type.clone()]),
            builder::FunctionBody::from(builder::ExternalFunction::new(
                std::rc::Rc::new(format::Identifier::try_from("libc")?),
                format::Identifier::try_from("putchar")?,
            )),
        );

        let entry_code = {
            let code = builder.code().define(Vec::new(), Vec::new());
            let entry_block = code.entry_block();

            let message_length = entry_block.conv_i_overflowing(
                entry_block.const_i(u32::try_from(message_data.bytes().len()).unwrap()),
                type_system::Int::UNative,
            )?;

            let message_register = entry_block.alloca(message_length, byte_type);
            entry_block.mem_init_from_data(message_register, message_data);

            // entry_block.call(
            //     &builder::Function::Defined(helper),
            //     [entry_block.const_i(60i32)],
            // )?;

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

    println!("{}", module.print_to_string());
    todo!("llvm");

    Ok(())
}

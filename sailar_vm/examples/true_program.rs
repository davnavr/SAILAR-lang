use sailar::builder;
use sailar::identifier::Id;
use sailar::index;
use sailar::instruction::{self, Instruction};
use sailar::num::VarU28;
use sailar::record;
use sailar::signature;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // A program that returns an exit code of 0 (success).
    let mut program: Vec<record::Record<'static>> = {
        let mut builder = builder::Builder::new();

        static PROGRAM_VERSION: &'static [VarU28] = &[VarU28::from_u8(1), VarU28::from_u8(0)];

        builder.add_record(record::MetadataField::ModuleIdentifier(
            record::ModuleIdentifier::new_borrowed(Id::try_from_str("true")?, PROGRAM_VERSION),
        ));

        let integer_type = {
            builder.add_record(signature::Type::from(signature::IntegerType::S32));
            index::TypeSignature::from(0)
        };

        let main_signature = {
            builder.add_record(signature::Function::new([].as_slice(), vec![integer_type]));
            index::FunctionSignature::from(0)
        };

        let main_code = {
            let instructions = vec![Instruction::Ret(
                vec![instruction::ConstantInteger::I8(0).into()].into_boxed_slice(),
            )];

            builder.add_record(record::CodeBlock::new(
                [].as_slice(),
                vec![integer_type],
                [].as_slice(),
                instructions,
            ));
            index::CodeBlock::from(0)
        };

        builder.add_record(record::FunctionDefinition::new(
            record::Export::new_export_borrowed(Id::try_from_str("main")?),
            main_signature,
            record::FunctionBody::Definition(main_code),
        ));

        let entry_point = {
            builder.add_record(record::FunctionInstantiation::from_template(index::FunctionTemplate::from(0)));
            index::FunctionInstantiation::from(0)
        };

        builder.add_record(record::MetadataField::EntryPoint(entry_point));

        builder.into_records()
    };

    let state = sailar_load::state::Builder::new().create();

    let module = state.force_load_module(&mut program)?.unwrap();

    let main = module.entry_point()?.ok_or("expected entry point to be present")?;
    let runtime = sailar_vm::runtime::Configuration::new().initialize_runtime();

    let return_values = runtime.execute(main.clone(), Box::default())?;

    dbg!(return_values);

    Ok(())
}

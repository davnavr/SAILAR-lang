use sailar::builder;
use sailar::identifier::Id;
use sailar::index;
use sailar::instruction::{self, Instruction};
use sailar::num::VarU28;
use sailar::record;
use sailar::signature;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // A program that returns an exit code of 1 (failure).
    let program: Vec<record::Record<'static>> = {
        let mut builder = builder::Builder::new();

        static PROGRAM_VERSION: &'static [VarU28] = &[VarU28::from_u8(1), VarU28::from_u8(1)];

        builder.add_record(record::MetadataField::ModuleIdentifier(
            record::ModuleIdentifier::new_borrowed(Id::try_from_str("False")?, PROGRAM_VERSION),
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
                vec![instruction::ConstantInteger::I8(1).into()].into_boxed_slice(),
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
            main_signature,
            record::FunctionBody::Definition(main_code),
        ));

        let entry_point = {
            builder.add_record(record::FunctionInstantiation::from_template(
                record::Export::new_export_borrowed(Id::try_from_str("main")?),
                index::FunctionTemplate::from(0),
            ));
            index::FunctionInstantiation::from(0)
        };

        builder.add_record(record::MetadataField::EntryPoint(entry_point));

        builder.into_records()
    };

    inkwell::targets::Target::initialize_all(&inkwell::targets::InitializationConfig::default());

    let mut context = None;

    // Compile for the current target platform
    let output = sailar_aot::compilation::Inputs::new()
        .with_modules([program])
        .compile(&mut context)?;

    // Print the LLVM assembly of the produced module
    output.output_module().print_to_stderr();

    // Write an object file to disk
    output.write_object_code_to_file(inkwell::targets::FileType::Object, "false.o")?;

    Ok(())
}

use sailar::builder;
use sailar::identifier::Id;
use sailar::index;
use sailar::num::VarU28;
use sailar::record;
use sailar::signature;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // A program that returns an exit code of 0 (success).
    let program: Vec<record::Record<'static>> = {
        let mut builder = builder::Builder::new();

        static program_version: &'static [VarU28] = &[VarU28::from_u8(1), VarU28::from_u8(0)];

        builder.add_record(record::MetadataField::ModuleIdentifier(
            record::ModuleIdentifier::new_borrowed(Id::try_from_str("true")?, program_version),
        ));

        let integer_type = {
            builder.add_record(signature::Type::S32);
            index::TypeSignature::from(0)
        };

        let main_signature = {
            builder.add_record(signature::Function::new([].as_slice(), [integer_type].as_slice()));
            index::FunctionSignature::from(0)
        };

        let main_code = {
            //builder.add_record(record::CodeBlock::from_types(register_types: CowBox<'a, [index::TypeSignature]>, input_count: usize, result_count: usize, instructions: CowBox<'a, [instruction::Instruction]>))
            index::CodeBlock::from(0)
        };

        let main_definition = {
            builder.add_record(record::FunctionDefinition::new(
                record::Export::new_export_borrowed(Id::try_from_str("main")?),
                main_signature,
                record::FunctionBody::Definition(main_code),
            ));
        };

        builder.into_records()
    };

    println!("{:?}", program);

    Ok(())
}

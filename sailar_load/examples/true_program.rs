use sailar::builder;
use sailar::identifier::Id;
use sailar::num::VarU28;
use sailar::record;
use sailar::signature;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // A program that returns an exit code of 0 (success).
    let program = {
        let mut builder = builder::Builder::new();

        builder.add_record(record::MetadataField::ModuleIdentifier(
            record::ModuleIdentifier::new_borrowed(Id::try_from_str("true")?, &[VarU28::from_u8(1), VarU28::from_u8(0)]),
        ));

        let integer_type = {
            builder.add_record(signature::Type::S32);
            VarU28::from_u8(0).into()
        };

        let main_signature = {
            builder.add_record(signature::Function::new(&[], &[integer_type]));
            VarU28::from_u8(0).into()
        };

        let main_definition = {
            builder.add_record(record::FunctionDefinition::new(
                record::Export::new_export_borrowed(Id::try_from_str("main")),
                main_signature,
                body: FunctionBody<'a>,
            ));
        };

        builder.into_records();
    };

    println!("{:?}", program);

    Ok(())
}

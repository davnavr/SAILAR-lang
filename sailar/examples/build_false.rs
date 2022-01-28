use sailar::builder::{self, Identifier};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program = {
        let mut builder = builder::Builder::new(Identifier::try_from("False")?);
        builder.finish()
    };

    todo!();
    Ok(())
}

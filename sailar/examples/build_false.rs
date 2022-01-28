use sailar::builder::{self, Identifier};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program = {
        let mut builder = builder::Builder::new(Identifier::try_from("False")?);

        let code = builder.code().define(0, 1);

        {
            let entry_block = code.entry_block();
            let exit_code = entry_block.const_i(1);
        }

        builder.finish()
    };

    todo!();
    Ok(())
}

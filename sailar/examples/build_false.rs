use sailar::builder::{self, Identifier};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program = {
        let mut setup = None;
        let mut builder = builder::Builder::new(&mut setup, Identifier::try_from("False")?);

        let code = builder.code().define(0, 1);

        {
            let entry_block = code.entry_block();
            let exit_code = entry_block.const_i(1);
            entry_block.ret([exit_code])?;
        }

        builder.finish()
    };

    todo!("{:?}", program);
    Ok(())
}

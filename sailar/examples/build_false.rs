use sailar::builder::{self, Name};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program = {
        let mut builder = builder::Builder::new(Name::try_from("False")?);

        {
            let mut code_definitions = builder.code();
            let mut code = code_definitions.define(0, 1);
            let entry_block = code.entry_block();
            let exit_code = entry_block.const_i(1);
            entry_block.ret([exit_code])?;
        }

        builder.finish()
    };

    todo!("{:?}", program);
    Ok(())
}

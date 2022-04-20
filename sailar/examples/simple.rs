use sailar::block;
use sailar::function;
use sailar::module::Export;
use sailar::type_system;
use sailar::Identifier;
use std::sync::Arc;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut module = sailar::Module::new(Identifier::from_str("Simple")?, vec![0, 1].into_boxed_slice());

    {
        let mut block_cache = block::BuilderCache::new();
        let mut main_block_builder = block_cache.builder(vec![type_system::FixedInt::S32.into()], []);
        let result = main_block_builder.emit_add::<i32, i32>(1, 5)?;
        let main_block = Arc::new(main_block_builder.emit_ret([result.into()])?);

        let main_function = module.add_function(
            Identifier::from_str("main")?,
            Arc::new(function::Signature::new(
                vec![type_system::FixedInt::S32.into()],
                Box::default(),
            )),
            function::Kind::Defined(function::Definition::new(function::Body::Defined(main_block), Export::Yes)),
        )?;
    }

    let contents = module.raw_contents(None);
    println!("{}", contents.hex_dump_to_string());

    let parsed_module = sailar::Module::from_slice(&contents, None)?;
    assert_eq!(module, parsed_module);

    Ok(())
}

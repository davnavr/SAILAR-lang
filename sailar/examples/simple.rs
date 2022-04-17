use sailar::block;
use sailar::function;
use sailar::module::Export;
use sailar::type_system;
use sailar::{Identifier, Module};
use std::sync::Arc;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut module = sailar::Module::new(Identifier::from_str("Simple")?, vec![0, 1].into_boxed_slice());

    let mut block_cache = block::BuilderCache::new();
    let mut main_block_builder = block_cache.builder();
    main_block_builder.emit_nop();
    let main_block = main_block_builder.emit_ret(Box::default())?;

    let main_function = module.add_function(
        Identifier::from_str("main")?,
        Arc::new(function::Signature::new(
            vec![type_system::FixedInt::S32.into()].into_boxed_slice(),
            Box::default(),
        )),
        function::Kind::Defined(function::Definition::new(
            function::Body::Defined(Arc::new(main_block)),
            Export::Yes,
        )),
    )?;

    println!("{:?}", module);
    Ok(())
}

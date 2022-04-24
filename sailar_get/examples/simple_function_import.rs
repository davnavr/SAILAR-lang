fn main() -> Result<(), Box<dyn std::error::Error>> {
    use sailar::Identifier;

    let library = {
        let mut module = sailar::ModuleDefinition::new(Identifier::from_str("Library")?, Default::default());

        // TODO: Add helper function.

        module
    };

    let resolver = sailar_get::Resolver::new(sailar_get::resolver::HOST_POINTER_SIZE);
    let library_module = resolver.force_load_module(library)?;

    Ok(())
}

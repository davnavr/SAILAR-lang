fn main() -> Result<(), Box<dyn std::error::Error>> {
    // A program that returns an exit code of 0 (success).
    let program: Vec<sailar::record::Record<'static>> =
        sailar_samples::exit_with(sailar::identifier::Identifier::try_from("true")?, 0).into_records();

    let state = sailar_load::state::Builder::new().create();

    let module = state.force_load_module(program)?.unwrap();

    let main = module.entry_point()?.ok_or("expected entry point to be present")?;
    let runtime = sailar_vm::runtime::Configuration::new().initialize_runtime();

    let return_values = runtime.execute(main.clone(), Box::default())?;

    dbg!(return_values);

    Ok(())
}

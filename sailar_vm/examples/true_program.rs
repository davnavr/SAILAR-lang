fn main() -> Result<(), Box<dyn std::error::Error>> {
    // A program that returns an exit code of 0 (success).
    let program = sailar_samples::exit_with(sailar::identifier::Identifier::try_from("true")?, 0);

    let state = sailar_load::state::Configuration::new().create_state();

    let module = state
        .load_module(sailar::validation::ValidModule::from_builder(program)?)
        .unwrap();

    let main = module.entry_point().ok_or("expected entry point to be present")?;
    let runtime = sailar_vm::runtime::Configuration::new().initialize_runtime();

    let return_values = runtime.execute(main.clone(), Box::default())?;

    dbg!(return_values);

    Ok(())
}

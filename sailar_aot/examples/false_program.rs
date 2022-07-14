fn main() -> Result<(), Box<dyn std::error::Error>> {
    // A program that returns an exit code of 1 (failure).
    let program: Vec<sailar::record::Record<'static>> =
        sailar_samples::exit_with(sailar::identifier::Identifier::try_from("False")?, 1).into_records();

    inkwell::targets::Target::initialize_all(&inkwell::targets::InitializationConfig::default());

    let mut context = None;

    // Compile for the current target platform
    let output = sailar_aot::compilation::Inputs::new()
        .with_modules([program])
        .compile(&mut context)?;

    // Print the LLVM assembly of the produced module
    output.output_module().print_to_stderr();

    // Write an object file to disk, this can then be linked with a C standard library to produce an executable
    output.write_object_code_to_path(inkwell::targets::FileType::Object, "false.o")?;

    // Write the LLVM bitcode to disk, which can then be interpreted with lli or compiled with llc
    output.write_llvm_bitcode_to_path("false.bc")?;

    Ok(())
}

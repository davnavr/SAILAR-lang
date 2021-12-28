use runmdl::runtime;
use structopt::StructOpt;

#[derive(StructOpt)]
struct Arguments {
    /// Path to the program to run.
    #[structopt(long, short)]
    program: std::path::PathBuf,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let all_arguments: Vec<String> = std::env::args().collect();
    let application_arguments_start = all_arguments.iter().position(|arg| arg == "--");

    let interpreter_arguments = Arguments::from_iter(
        &all_arguments[0..application_arguments_start.unwrap_or(all_arguments.len())],
    );

    let application_arguments = application_arguments_start
        .map(|start| all_arguments[start + 1..all_arguments.len()].iter().map(String::as_str).collect())
        .unwrap_or(Vec::default());

    let application =
        registir::parser::parse_module(&mut std::fs::File::open(interpreter_arguments.program)?)?;

    let mut initializer = runtime::Initializer::new();
    let runtime = runtime::Runtime::initialize(&mut initializer, application);

    std::process::exit(runtime.invoke_entry_point(application_arguments.as_ref())?)
}

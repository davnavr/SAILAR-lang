use runmdl::runtime;
use structopt::StructOpt;

mod debugging;

#[derive(StructOpt)]
struct Arguments {
    /// Path to the program to run.
    #[structopt(long, short)]
    program: std::path::PathBuf,
    /// If set, launches the debugger.
    #[structopt(long)]
    interactive: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let all_arguments: Vec<String> = std::env::args().collect();
    let application_arguments_start = all_arguments.iter().position(|arg| arg == "--");

    let interpreter_arguments = Arguments::from_iter(
        &all_arguments[0..application_arguments_start.unwrap_or(all_arguments.len())],
    );

    let result = {
        let application_arguments = application_arguments_start
            .map(|start| {
                all_arguments[start + 1..all_arguments.len()]
                    .iter()
                    .map(String::as_str)
                    .collect()
            })
            .unwrap_or(Vec::default());

        let application = registir::parser::parse_module(&mut std::fs::File::open(
            interpreter_arguments.program,
        )?)?;

        let mut initializer = runtime::Initializer::new();
        let runtime = runtime::Runtime::initialize(&mut initializer, application);

        let mut debugger;

        runtime.invoke_entry_point(
            application_arguments.as_ref(),
            if interpreter_arguments.interactive {
                debugger = debugging::CommandLineDebugger::new();
                Some(&mut debugger as &mut dyn runmdl::interpreter::debugger::Debugger)
            } else {
                None
            },
        )
    };

    match result {
        Ok(exit_code) => std::process::exit(exit_code),
        Err(runtime::Error::InterpreterError(error)) => {
            eprintln!("Error: {}", error);
            for frame in error.stack_trace() {
                let location = frame.location();
                eprintln!(
                    "- {} at block {} instruction {}",
                    "function name here", //frame.method(),
                    location.block_index,
                    location.code_index
                );
                // TODO: Have option to hide register values.
                for (index, input) in frame.input_registers().iter().enumerate() {
                    eprintln!(" > %i{} = {}", index, input);
                }
                for (index, temporary) in frame.temporary_registers().iter().enumerate() {
                    eprintln!(" > %t{} = {}", index, temporary);
                }
            }
            std::process::exit(1)
        }
        Err(error) => Err(Box::new(error)),
    }
}

use clap::{self, Parser as _};
use sailar_vm::runtime;

mod debugging;

#[derive(clap::Parser, Debug)]
#[clap()]
struct Arguments {
    /// Path to the program to run.
    #[clap(long, short)]
    program: std::path::PathBuf,
    /// If set, launches the debugger.
    #[clap(long)]
    interactive: bool,
    /// Sets the maximum number of memory that can be allocated on the stack, in bytes.
    #[clap(long)]
    stack_memory_capacity: Option<std::num::NonZeroUsize>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let all_arguments: Vec<String> = std::env::args().collect();
    let application_arguments_start = all_arguments.iter().position(|arg| arg == "--");

    let interpreter_arguments = Arguments::parse_from(
        &all_arguments[0..application_arguments_start.unwrap_or(all_arguments.len())],
    );

    let result = {
        let application_arguments: Vec<_> = application_arguments_start
            .map(|start| {
                all_arguments[start + 1..all_arguments.len()]
                    .iter()
                    .map(String::as_str)
                    .collect()
            })
            .unwrap_or_default();

        let (application, _) =
            sailar::parser::parse_module(&mut std::fs::File::open(interpreter_arguments.program)?)?;

        let mut initializer = runtime::Initializer::new();

        if let Some(capacity) = interpreter_arguments.stack_memory_capacity {
            initializer.set_value_stack_capacity(capacity);
        }

        let mut resolver = (); // TODO: Add resolver that reads modules passed in as imports.

        let runtime = runtime::Runtime::initialize(&mut initializer, &mut resolver, application);

        let mut debugger;

        runtime.invoke_entry_point(
            application_arguments.as_ref(),
            if interpreter_arguments.interactive {
                debugger = debugging::CommandLineDebugger::new();
                Some(&mut debugger as &mut dyn sailar_vm::interpreter::debugger::Debugger)
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
                eprint!("- {} ", debugging::FunctionSymbol(frame.function()));

                match frame.location() {
                    Some(location) => eprintln!(
                        "at block {} instruction {}",
                        location.block_index, location.code_index
                    ),
                    None => eprintln!("(external call)"),
                }

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

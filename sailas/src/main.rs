//! Executable for assembling SAILAR assembly files

/// SAILAR bytecode assembler
#[derive(Debug, clap::Parser)]
#[clap(version, about)]
struct Arguments {
    /// Path to the SAILAR assembly file
    #[clap(long, short)]
    input: std::path::PathBuf,
    /// Path to the file containing the assembled SAILAR binary module.
    #[clap(long, short)]
    output: Option<std::path::PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let arguments: Arguments = clap::Parser::parse();
    let input = std::fs::read_to_string(&arguments.input)?;

    match sailasm::assemble(&input) {
        Ok(module) => {
            let default_output;
            let output = if let Some(path) = &arguments.output {
                path
            } else {
                default_output = arguments.input.with_extension(".sail");
                &default_output
            };

            module.write_to(std::fs::File::create(output)?).map_err(Box::from)
        }
        Err(errors) => {
            for e in errors.iter() {
                eprintln!("{}", e);
            }

            Err(Box::from(format!("failed with {} errors", errors.len())))
        }
    }
}

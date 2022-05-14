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

    todo!("assemble it");
}

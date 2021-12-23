use structopt::StructOpt;

#[derive(StructOpt)]
struct Arguments {
    /// The input binary module file to assemble.
    #[structopt(long, short)]
    input: std::path::PathBuf,
    /// The path to the file containing the disassembled text module.
    #[structopt(long, short)]
    output: Option<std::path::PathBuf>,
    //indentation: dasmdl::disassembler::Indentation,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Arguments::from_args();
    
    println!("Hello, world!");
    Ok(())
}

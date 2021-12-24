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
    let output_path = args.output.unwrap_or_else(|| args.input.with_extension("txtmdl"));
    let module = {
        let mut input = std::fs::File::open(args.input)?;
        registir::parser::parse_module(&mut input)?
    };

    let mut output = std::fs::File::create(output_path)?;
    dasmdl::disassembler::disassemble(&mut output, &module, dasmdl::disassembler::Indentation::default())?;
    Ok(())
}

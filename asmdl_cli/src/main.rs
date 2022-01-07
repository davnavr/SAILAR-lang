use structopt::StructOpt;

#[derive(StructOpt)]
struct Arguments {
    /// The input text module file to assemble.
    #[structopt(long, short)]
    input: std::path::PathBuf,
    /// The path to the file containing the assembled binary module.
    #[structopt(long, short)]
    output: Option<std::path::PathBuf>,
}

fn main() -> Result<(), std::io::Error> {
    let arguments = Arguments::from_args();
    let input = std::fs::read_to_string(&arguments.input)?;
    let (syntax_tree, lexer_errors, parser_errors) = asmdl::parser::tree_from_str(&input);

    // let module = asmdl::assembler::assemble_declarations(&syntax_tree)
    //     .map_err::<Vec<Box<dyn std::error::Error>>, _>(|errors| {
    //         errors
    //             .into_iter()
    //             .map(|error| Box::new(WrappedError(error)) as Box<dyn std::error::Error>)
    //             .collect()
    //     })?;

    if !(lexer_errors.is_empty() && parser_errors.is_empty()) {
        use ariadne::{Label, Report, ReportKind, Source};
        // NOTE: ariadne currently does not work with CRLF

        let source_path = arguments
            .input
            .file_name()
            .and_then(|file_name| file_name.to_str())
            .unwrap_or("txtmdl");

        let mut source = (source_path, Source::from(&input));

        let input_errors = lexer_errors
            .into_iter()
            .map(|error| error.map(|msg| msg.to_string()))
            .chain(
                parser_errors
                    .into_iter()
                    .map(|error| error.map(|msg| msg.to_string())),
            );

        for error in input_errors {
            let position = error.span();
            Report::<(&str, asmdl::ast::Position)>::build(
                ReportKind::Error,
                source_path,
                position.start,
            )
            .with_label(Label::new((source_path, position)).with_message(error))
            .finish()
            .eprint(&mut source)?;
        }
        
        std::process::exit(1)
    }

    let mut output = std::fs::File::create(
        arguments
            .output
            .as_ref()
            .unwrap_or(&arguments.input.with_extension("binmdl")),
    )?;

    //registir::writer::write_module(&module, &mut output)
    Ok(())
}

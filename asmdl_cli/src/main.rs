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

trait Error {
    type Description: std::fmt::Display;

    fn position(&self) -> Option<asmdl::ast::Position>;

    fn error(&self) -> &Self::Description;
}

struct WrappedError<E: Error>(E);

impl<E: Error> std::fmt::Display for WrappedError<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("at ")?;
        match self.0.position() {
            Some(position) => write!(f, "line {} column {}", position.line, position.column),
            None => f.write_str("end of file"),
        }?;
        write!(f, ": {}", self.0.error())
    }
}

impl<E: Error> std::fmt::Debug for WrappedError<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self, f)
    }
}

impl<E: Error> std::error::Error for WrappedError<E> {}

impl Error for asmdl::parser::PositionedParserError {
    type Description = asmdl::parser::Error;

    fn error(&self) -> &Self::Description {
        &self.error
    }

    fn position(&self) -> Option<asmdl::ast::Position> {
        self.position
    }
}

impl Error for asmdl::assembler::Error {
    type Description = asmdl::assembler::Error;

    fn error(&self) -> &Self::Description {
        self
    }

    fn position(&self) -> Option<asmdl::ast::Position> {
        asmdl::assembler::Error::position(self).cloned()
    }
}

fn assemble(args: &Arguments) -> Result<(), Vec<Box<dyn std::error::Error>>> {
    let syntax_tree = std::fs::read_to_string(&args.input)
        .map_err(|error| vec![Box::new(error) as Box<dyn std::error::Error>])?;

    let input = asmdl::parser::parse(&syntax_tree).map_err::<Vec<Box<dyn std::error::Error>>, _>(
        |errors| {
            errors
                .into_iter()
                .map(|error| Box::new(WrappedError(error)) as Box<dyn std::error::Error>)
                .collect()
        },
    )?;

    let module = asmdl::assembler::assemble_declarations(&input)
        .map_err::<Vec<Box<dyn std::error::Error>>, _>(|errors| {
            errors
                .into_iter()
                .map(|error| Box::new(WrappedError(error)) as Box<dyn std::error::Error>)
                .collect()
        })?;

    let mut output = std::fs::File::create(
        args.output
            .as_ref()
            .unwrap_or(&args.input.with_extension("binmdl")),
    )
    .map_err(|error| vec![Box::new(error) as Box<dyn std::error::Error>])?;

    registir::writer::write_module(&module, &mut output)
        .map_err(|error| vec![Box::new(error) as Box<dyn std::error::Error>])
}

fn main() -> Result<(), ()> {
    if let Err(errors) = assemble(&Arguments::from_args()) {
        for message in errors {
            eprintln!("ERR {}", message)
        }

        Err(())
    } else {
        Ok(())
    }
}

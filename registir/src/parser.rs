
#[derive(Debug)]
#[non_exhaustive]
pub enum ParseError {
    InputOutputError(std::io::Error),
}

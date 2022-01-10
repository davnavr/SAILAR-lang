use registir::format;

#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    IndexOutOfBounds(format::numeric::UInteger),
    Other(Box<dyn std::error::Error>),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IndexOutOfBounds(index) => write!(f, "index {} is out of bounds", index),
            Self::Other(error) => std::fmt::Display::fmt(error, f),
        }
    }
}

impl std::error::Error for Error {}

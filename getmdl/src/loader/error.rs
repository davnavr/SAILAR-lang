use registir::format;

#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum Error {
    #[error("index {0} is out of bounds")]
    IndexOutOfBounds(format::numeric::UInteger),
    #[error(transparent)]
    Other(#[from] Box<dyn std::error::Error>),
}

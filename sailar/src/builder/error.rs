use crate::builder;

#[derive(thiserror::Error, Clone, Debug)]
#[non_exhaustive]
pub enum Error {
    #[error(transparent)]
    InvalidInstruction(#[from] builder::InvalidInstruction),
}

pub type Result<T> = std::result::Result<T, Error>;

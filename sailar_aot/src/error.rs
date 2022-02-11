#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum Error {
    #[error(transparent)]
    Load(#[from] sailar_get::loader::Error),
    #[error("the entry point function could not be found")]
    MissingEntryPoint,
}

pub type Result<T> = std::result::Result<T, Error>;

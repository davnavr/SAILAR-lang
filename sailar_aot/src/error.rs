#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum Error {
    #[error(transparent)]
    Load(#[from] sailar_get::loader::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

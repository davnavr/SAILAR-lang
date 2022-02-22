// TODO: Allow errors to describe which block, function, and module they originate from.

#[derive(thiserror::Error, Debug)]
#[non_exhaustive]
pub enum Error {
    #[error("Cannot define more than {expected} temporary registers")]
    TooManyTemporariesDefined { expected: usize },
    #[error("a value corresponding to the register {0:?} was not defined")]
    UndefinedRegister(sailar::format::indices::Register),
    #[error("the register {register:?} did not have the expected type")]
    RegisterTypeMismatch {
        register: sailar::format::indices::Register,
    },
    #[error(transparent)]
    Load(#[from] sailar_get::loader::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

//! Contains types representing errors that can occur during compilation.

pub use sailar_load::error::GenericError;

/// Represents the set of errors that can occur during compilation.
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum CompilationErrorKind {
    /// Indicates that some error occured while resolving a reference to a SAILAR module.
    #[error(transparent)]
    ModuleResolution(#[from] GenericError),
    //InvalidMainFunction(Vec<std::sync::Arc>),
    #[error(transparent)]
    Loader(#[from] sailar_load::error::LoaderError),
    #[error(transparent)]
    InvalidTarget(#[from] crate::target::Error),
}

/// The error type used when an error occurs during compilation.
#[derive(Debug, thiserror::Error)]
#[error(transparent)]
#[repr(transparent)]
pub struct CompilationError(Box<CompilationErrorKind>);

impl CompilationError {
    pub fn new<E: Into<CompilationErrorKind>>(kind: E) -> Self {
        Self(Box::new(kind.into()))
    }

    pub fn kind(&self) -> &CompilationErrorKind {
        &self.0
    }
}

impl<E: Into<CompilationErrorKind>> From<E> for CompilationError {
    fn from(kind: E) -> Self {
        Self::new(kind)
    }
}

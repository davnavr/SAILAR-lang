//! Contains types representing errors that can occur during compilation.

use std::fmt::{Display, Formatter};
use std::sync::Arc;

pub use sailar_load::error::GenericError;

/// Error type used when an entry point contains return types that are not supported.
#[derive(Debug, thiserror::Error)]
#[repr(transparent)]
pub struct EntryPointReturnTypesError(Box<[Arc<sailar_load::type_system::Signature>]>);

impl Display for EntryPointReturnTypesError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        sailar_load::type_system::display_signatures(self.0.iter(), f)?;
        f.write_str(" is not a supported return type for an entry point function")
    }
}

/// Error type used when an entry point function is invalid.
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum InvalidEntryPointError {
    #[error(transparent)]
    UnsupportedReturnTypes(#[from] EntryPointReturnTypesError),
}

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

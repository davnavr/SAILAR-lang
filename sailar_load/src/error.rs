//! Contains types representing errors encountered during loading.

use crate::module::{self, Module};
use std::fmt::{Display, Formatter};
use std::sync::Arc;

/// A boxed error type.
///
/// Workaround for <https://github.com/rust-lang/project-error-handling/issues/16>.
#[repr(transparent)]
pub struct GenericError(Box<dyn std::error::Error>);

impl GenericError {
    pub fn new<E: std::error::Error + 'static>(error: E) -> Self {
        Self(Box::from(error))
    }

    pub fn into_inner(self) -> Box<dyn std::error::Error> {
        self.0
    }
}

impl From<Box<dyn std::error::Error>> for GenericError {
    fn from(error: Box<dyn std::error::Error>) -> Self {
        Self(error)
    }
}

impl std::fmt::Debug for GenericError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
    }
}

impl Display for GenericError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl std::error::Error for GenericError {}

/// The error type used to indicate that a [`Weak`] reference to data is no longer valid since it was dropped.
///
/// In application code, this error is handled by immediately stopping execution as this error usually indicates a bug in
/// the code.
///
/// [`Weak`]: std::sync::Weak
#[derive(Clone, Debug, thiserror::Error)]
#[error("weak reference to data is no longer valid")]
pub struct DroppedError(());

impl DroppedError {
    pub(crate) fn new(x: ()) -> Self {
        Self(x)
    }
}

#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum UnresolvedReferenceKind {
    #[error("could not resolve reference to {0:?}")]
    Module(module::ModuleIdentifier),
}

/// The error type used when a reference to something could not be resolved.
#[derive(Clone, Debug, thiserror::Error)]
pub struct UnresolvedReferenceError {
    module: Arc<Module>,
    kind: UnresolvedReferenceKind,
}

impl UnresolvedReferenceError {
    pub(crate) fn new<E: Into<UnresolvedReferenceKind>>(kind: E, module: Arc<Module>) -> Self {
        Self {
            module,
            kind: kind.into(),
        }
    }

    pub fn module(&self) -> &Arc<Module> {
        &self.module
    }

    pub fn kind(&self) -> &UnresolvedReferenceKind {
        &self.kind
    }
}

impl Display for UnresolvedReferenceError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "unable to resolve reference in module {}: {}",
            &crate::module::Display::from(&self.module),
            self.kind
        )
    }
}

#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum LoaderErrorKind {
    /// Indicates that data necessary for loading was unexpectedly dropped.
    #[error(transparent)]
    Dropped(#[from] DroppedError),
    #[error(transparent)]
    UnresolvedReference(#[from] UnresolvedReferenceError),
}

/// The error type used when loading a SAILAR module fails.
///
/// In application code (interpreters, JIT compilers, AOT compilers, etc.) this error is typically handled by immediately
/// stopping execution or compilation, as no further data from the module is expected to be loaded.
#[derive(Clone, Debug, thiserror::Error)]
#[error(transparent)]
#[repr(transparent)]
pub struct LoaderError(Box<LoaderErrorKind>);

impl LoaderError {
    pub fn new<E: Into<LoaderErrorKind>>(error: E) -> Self {
        Self(Box::new(error.into()))
    }

    pub fn kind(&self) -> &LoaderErrorKind {
        &self.0
    }
}

impl<E: Into<LoaderErrorKind>> From<E> for LoaderError {
    fn from(error: E) -> Self {
        Self::new(error)
    }
}

//! Contains types representing errors encountered during loading.

use crate::module::Module;
use std::fmt::{Display, Formatter};
use std::sync::Arc;

pub use sailar::validation::Error as InvalidModuleKind;

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

#[derive(Clone)]
pub struct InvalidModuleErrorInner {
    module: Arc<Module>,
    kind: InvalidModuleKind,
}

/// The error type used when validation to check that a module's contents are valid fails.
#[derive(Clone, thiserror::Error)]
pub struct InvalidModuleError(Box<InvalidModuleErrorInner>);

impl InvalidModuleError {
    pub(crate) fn new<E: Into<InvalidModuleKind>>(kind: E, module: Arc<Module>) -> Self {
        Self(Box::new(InvalidModuleErrorInner {
            module,
            kind: kind.into(),
        }))
    }

    /// Gets the module that is invalid.
    pub fn module(&self) -> &Arc<Module> {
        &self.0.module
    }

    pub fn kind(&self) -> &InvalidModuleKind {
        &self.0.kind
    }
}

impl std::fmt::Debug for InvalidModuleError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("InvalidModuleError")
            .field("module", self.module())
            .field("kind", self.kind())
            .finish()
    }
}

impl Display for InvalidModuleError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "in module {}, {}",
            &crate::module::Display::from(self.module()),
            self.kind()
        )
    }
}

/// Indicates that a [`Weak`] reference to data is no longer valid since it was dropped.
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
pub enum LoaderErrorKind {
    /// Indicates that data necessary for loading was unexpectedly dropped.
    #[error(transparent)]
    Dropped(#[from] DroppedError),
    /// Used when the SAILAR module does not contain valid content.
    #[error(transparent)]
    Invalid(#[from] InvalidModuleError),
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

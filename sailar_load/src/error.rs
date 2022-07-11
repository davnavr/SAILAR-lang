//! Contains types representing errors encountered during loading.

use crate::module::Module;
use sailar::index;
use std::fmt::{Display, Formatter};
use std::sync::Arc;

/// A boxed error type.
///
/// Workaround for https://github.com/rust-lang/project-error-handling/issues/16
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

pub(crate) trait IndexType: Into<usize> + Copy {
    fn kind() -> &'static str;
}

macro_rules! index_type_impl {
    ($implementor:ty, $name:literal) => {
        impl IndexType for $implementor {
            fn kind() -> &'static str {
                $name
            }
        }
    };
}

index_type_impl!(index::TypeSignature, "type signature");
index_type_impl!(index::FunctionSignature, "function signature");
index_type_impl!(index::CodeBlock, "code block");
index_type_impl!(index::FunctionTemplate, "function template");
index_type_impl!(index::FunctionInstantiation, "function instantiation");
index_type_impl!(index::Register, "register");

/// The error type used when an index in a module is not valid.
#[derive(Clone, Debug, thiserror::Error)]
pub struct InvalidIndexError {
    index: usize,
    maximum_index: Option<usize>,
    kind: &'static str,
}

impl InvalidIndexError {
    pub(crate) fn new<I: IndexType>(index: I, maximum_index: Option<usize>) -> Self {
        Self {
            index: index.into(),
            maximum_index,
            kind: I::kind(),
        }
    }
}

impl Display for InvalidIndexError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} index {} is not valid", self.kind, self.index)?;
        if let Some(maximum) = self.maximum_index {
            write!(f, ", maximum valid index is {}", maximum)?;
        }
        Ok(())
    }
}

/// A list specifying the kinds of invalid content that can be encountered when validating a module.
///
/// Usually used with the [`InvalidModuleError`] type.
#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum InvalidModuleKind {
    #[error(transparent)]
    InvalidIndex(#[from] InvalidIndexError),
    #[error(transparent)]
    InvalidCode(#[from] crate::code_block::InvalidInstructionError),
}

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

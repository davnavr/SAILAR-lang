//! Module for the resolution of SAILAR modules.

use crate::module::ModuleIdentifier;
use sailar::reader::Reader;
use std::io::Read;
use std::sync::Arc;

/// Trait for retrieving SAILAR modules from module identifiers, used to resolve module imports.
pub trait Resolver {
    type Source: Read;
    type Error;

    /// Retrieves the SAILAR module corresponding to the specified module identifier.
    ///
    /// Returns `Ok(Some)` when a module is successfully retrieved, `Ok(None)` if no corresponding module was found, or `Err` if
    /// an error occured while retrieving the module.
    fn load_from_identifier(
        &mut self,
        module_identifier: &Arc<ModuleIdentifier>,
    ) -> Result<Option<Reader<Self::Source>>, Self::Error>;
}

/// A module import resolver that never successfully retrieves a module.
///
/// Obtained by calling [`empty()`].
#[derive(Clone, Copy, Debug, Default)]
#[non_exhaustive]
pub struct Unsuccessful;

/// Constructs a resolver that never succeeds.
#[inline]
#[must_use]
pub fn unsuccessful() -> Unsuccessful {
    Unsuccessful
}

impl Resolver for Unsuccessful {
    type Source = std::io::Empty;
    type Error = std::convert::Infallible;

    fn load_from_identifier(&mut self, _: &Arc<ModuleIdentifier>) -> Result<Option<Reader<Self::Source>>, Self::Error> {
        Ok(None)
    }
}

#[repr(transparent)]
struct BoxedResolverInternals<R>(R);

impl<R> Resolver for BoxedResolverInternals<R>
where
    R: Resolver + Send + 'static,
    R::Error: std::error::Error,
{
    type Source = Box<dyn std::io::Read>;
    type Error = Box<dyn std::error::Error>;

    fn load_from_identifier(
        &mut self,
        module_identifier: &Arc<ModuleIdentifier>,
    ) -> Result<Option<Reader<Self::Source>>, Self::Error> {
        match self.0.load_from_identifier(module_identifier) {
            Ok(None) => Ok(None),
            Ok(Some(reader)) => Ok(Some(reader.into_boxed_reader())),
            Err(e) => Err(Box::new(e)),
        }
    }
}

pub type BoxedResolver =
    Box<dyn (Resolver<Source = Box<dyn std::io::Read>, Error = Box<dyn std::error::Error>>) + Send + 'static>;

pub fn boxed<R>(resolver: R) -> BoxedResolver
where
    R: Resolver + Send + 'static,
    R::Error: std::error::Error,
{
    Box::new(BoxedResolverInternals(resolver))
}

/// Error type used when resolving module imports.
#[derive(Clone, Debug, thiserror::Error)]
pub enum Error<E> {
    #[error("retrieved module was expected to be {expected:?}, but got {actual:?}")]
    ModuleIdentifierMismatch {
        expected: ModuleIdentifier,
        actual: ModuleIdentifier,
    },
    /// An error occured while retrieving the module.
    #[error(transparent)]
    RetrievalError(E),
}

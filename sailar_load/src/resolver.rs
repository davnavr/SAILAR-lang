//! Module for the resolution of SAILAR modules.

use crate::error::GenericError;
use crate::module::ModuleIdentifier;
use crate::source;
use std::sync::Arc;

/// Trait for retrieving SAILAR modules from module identifiers, used to resolve module imports.
pub trait Resolver {
    type Error;
    type Source: source::Source<Error = Self::Error>;

    /// Retrieves the SAILAR module corresponding to the specified module identifier.
    ///
    /// Returns `Ok(Some)` when a module is successfully retrieved, `Ok(None)` if no corresponding module was found, or `Err` if
    /// an error occured while retrieving the module.
    fn load_from_identifier(&mut self, module_identifier: &Arc<ModuleIdentifier>) -> Result<Option<Self::Source>, Self::Error>;
}

impl<R: Resolver + ?Sized> Resolver for Box<R> {
    type Source = R::Source;
    type Error = R::Error;

    fn load_from_identifier(&mut self, module_identifier: &Arc<ModuleIdentifier>) -> Result<Option<Self::Source>, Self::Error> {
        R::load_from_identifier(self, module_identifier)
    }
}

/// A module import resolver that never successfully retrieves a module.
///
/// Obtained by calling [`unsuccessful()`].
#[derive(Clone, Copy, Debug, Default)]
#[non_exhaustive]
pub struct Unsuccessful;

/// Constructs a resolver that never succeeds.
#[must_use]
pub fn unsuccessful() -> Unsuccessful {
    Unsuccessful
}

impl Resolver for Unsuccessful {
    type Source = source::Empty;
    type Error = std::convert::Infallible;

    fn load_from_identifier(&mut self, _: &Arc<ModuleIdentifier>) -> Result<Option<source::Empty>, Self::Error> {
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
    type Source = source::BoxedSource;
    type Error = GenericError;

    fn load_from_identifier(&mut self, module_identifier: &Arc<ModuleIdentifier>) -> Result<Option<Self::Source>, Self::Error> {
        match self.0.load_from_identifier(module_identifier) {
            Ok(None) => Ok(None),
            Ok(Some(reader)) => Ok(Some(source::boxed(reader))),
            Err(e) => Err(GenericError::new(e)),
        }
    }
}

#[repr(transparent)]
pub struct BoxedResolver(Box<dyn (Resolver<Source = source::BoxedSource, Error = GenericError>) + Send>);

impl Resolver for BoxedResolver {
    type Source = source::BoxedSource;
    type Error = GenericError;

    fn load_from_identifier(&mut self, module_identifier: &Arc<ModuleIdentifier>) -> Result<Option<Self::Source>, Self::Error> {
        let resolver: &mut dyn Resolver<Source = Self::Source, Error = Self::Error> = self;
        resolver.load_from_identifier(module_identifier)
    }
}

pub fn boxed<R>(resolver: R) -> BoxedResolver
where
    R: Resolver + Send + 'static,
    R::Error: std::error::Error,
{
    BoxedResolver(Box::new(BoxedResolverInternals(resolver)))
}

/// Error type used when resolving module imports.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("retrieved module was expected to be {expected:?}, but got {actual:?}")]
    ModuleIdentifierMismatch {
        expected: ModuleIdentifier,
        actual: ModuleIdentifier,
    },
    /// An error occured while retrieving the module.
    #[error(transparent)]
    RetrievalError(GenericError),
}

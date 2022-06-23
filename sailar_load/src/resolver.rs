//! Module for the resolution of SAILAR modules.

use crate::ModuleIdentifier;
use sailar::reader::Reader;
use std::io::Read;

/// Trait for retrieving SAILAR modules from module identifiers, used to resolve module imports.
pub trait Resolver<'a> {
    type Source: Read;
    type Error;

    /// Retrieves the SAILAR module corresponding to the specified module identifier.
    ///
    /// Returns `Ok(Some)` when a module is successfully retrieved, `Ok(None)` if no corresponding module was found, or `Err` if
    /// an error occured while retrieving the module.
    fn load_from_identifier(&mut self, module_identifier: &ModuleIdentifier)
        -> Result<Option<Reader<Self::Source>>, Self::Error>;
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

impl Resolver<'_> for Unsuccessful {
    type Source = std::io::Empty;
    type Error = std::convert::Infallible;

    fn load_from_identifier(&mut self, _: &ModuleIdentifier) -> Result<Option<Reader<Self::Source>>, Self::Error> {
        Ok(None)
    }
}

#[repr(transparent)]
struct BoxedResolverInternals<R>(R);

impl<'a, R> Resolver<'a> for BoxedResolverInternals<R>
where
    R: Resolver<'a> + 'a,
    R::Error: std::error::Error + 'a,
{
    type Source = Box<dyn std::io::Read + 'a>;
    type Error = Box<dyn std::error::Error + 'a>;

    fn load_from_identifier(
        &mut self,
        module_identifier: &ModuleIdentifier,
    ) -> Result<Option<Reader<Self::Source>>, Self::Error> {
        match self.0.load_from_identifier(module_identifier) {
            Ok(None) => Ok(None),
            Ok(Some(reader)) => Ok(Some(reader.into_boxed_reader())),
            Err(e) => Err(Box::new(e)),
        }
    }
}

pub type BoxedResolver<'a> =
    Box<dyn (Resolver<'a, Source = Box<dyn std::io::Read + 'a>, Error = Box<dyn std::error::Error + 'a>>) + Send + 'a>;

pub fn boxed<'a, R>(resolver: R) -> BoxedResolver<'a>
where
    R: Resolver<'a> + Send + 'a,
    R::Error: std::error::Error + 'a,
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

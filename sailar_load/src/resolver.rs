//! Module for the resolution of SAILAR modules.

use crate::ModuleIdentifier;
use sailar::binary::reader::Reader;

/// Trait for retrieving SAILAR modules from module identifiers, used to resolve module imports.
pub trait Resolver {
    type Source: std::io::Read;
    type Error;

    /// Retrieves the SAILAR module corresponding to the specified module identifier.
    /// 
    /// Returns `Ok(Some)` when a module is successfully retrieved, `Ok(None)` if no corresponding module was found, or `Err` if
    /// an error occured while retrieving the module.
    fn load_from_identifier(&mut self, module_identifier: ModuleIdentifier) -> Result<Option<Reader<Self::Source>>, Self::Error>;
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

    fn load_from_identifier(&mut self, _: ModuleIdentifier) -> Result<Option<Reader<Self::Source>>, Self::Error> {
        Ok(None)
    }
}

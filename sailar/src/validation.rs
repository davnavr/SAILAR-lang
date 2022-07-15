//! Module to perform validation of SAILAR code.
//!
//! Validation ensures that the contents of a SAILAR module are correct, without having to resolve any imports.

use crate::helper::borrow::CowBox;
use crate::index;
use crate::record::{self, Record};
use std::borrow::Cow;

/// A list specifying the kinds of errors that can occur during SAILAR module validation.
///
/// Usually used with the [`Error`] type.
#[derive(Clone, Debug, thiserror::Error)]
#[non_exhaustive]
pub enum ErrorKind {
    /// Used when more than one entry point was specified by a metadata record.
    #[error("duplicate entry point #{duplicate}, #{defined} is already defined as the entry point function")]
    DuplicateEntryPoint {
        defined: index::FunctionInstantiation,
        duplicate: index::FunctionInstantiation,
    },
}

/// Represents an error that occured during validation of a SAILAR module.
#[derive(Clone, Debug, thiserror::Error)]
#[error(transparent)]
#[repr(transparent)]
pub struct Error(Box<ErrorKind>);

impl Error {
    pub fn from_kind<E: Into<ErrorKind>>(kind: E) -> Self {
        Self(Box::new(kind.into()))
    }
}

impl<E: Into<ErrorKind>> From<E> for Error {
    fn from(error: E) -> Self {
        Self::from_kind(error)
    }
}

/// Represents the contents of a SAILAR module.
#[derive(Clone, Debug, Default)]
#[non_exhaustive]
pub struct ModuleContents<'a> {
    pub module_identifiers: Vec<record::ModuleIdentifier<'a>>,
    pub entry_point: Option<index::FunctionInstantiation>,
    /// The list of all identifier records in the module.
    pub identifiers: Vec<Cow<'a, crate::identifier::Id>>,
}

impl<'a> ModuleContents<'a> {
    /// Indicates whether the module is anonymous.
    ///
    /// Anonymous modules do not have any module identifier, meaning that they cannot be imported by other modules.
    pub fn is_anonymous(&self) -> bool {
        self.module_identifiers.is_empty()
    }
}

/// Represents a validated SAILAR module.
#[derive(Clone, Debug)]
pub struct ValidModule<'a> {
    contents: ModuleContents<'a>,
}

impl<'a> ValidModule<'a> {
    pub fn from_records_fallible<R, E>(records: R) -> Result<Result<Self, Error>, E>
    where
        R: IntoIterator<Item = Result<Record<'a>, E>>,
    {
        let mut contents = ModuleContents::<'a>::default();
        let mut metadata_fields = Vec::new();

        for data in records.into_iter() {
            match data? {
                Record::MetadataField(metadata) => metadata_fields.push(metadata),
                Record::Identifier(identifier) => contents.identifiers.push(identifier),
                bad => todo!("validate {:?}", bad),
            }
        }

        // TODO: Perform validation here.

        for field in metadata_fields.into_iter() {
            match field {
                record::MetadataField::ModuleIdentifier(identifier) => contents.module_identifiers.push(identifier),
                record::MetadataField::EntryPoint(entry_point) => {
                    if let Some(defined) = contents.entry_point {
                        return Ok(Err(ErrorKind::DuplicateEntryPoint {
                            defined,
                            duplicate: entry_point,
                        }
                        .into()));
                    }
                    // else if entry point OOB

                    contents.entry_point = Some(entry_point);
                }
            }
        }

        Ok(Ok(Self { contents }))
    }

    pub fn from_records<R: IntoIterator<Item = Record<'a>>>(records: R) -> Result<Self, Error> {
        Self::from_records_fallible::<_, std::convert::Infallible>(records.into_iter().map(Ok)).unwrap()
    }

    pub fn contents(&self) -> &ModuleContents<'a> {
        &self.contents
    }

    pub fn into_contents(self) -> ModuleContents<'a> {
        self.contents
    }
}

impl<'a> TryFrom<Vec<Record<'a>>> for ValidModule<'a> {
    type Error = Error;

    fn try_from(records: Vec<Record<'a>>) -> Result<Self, Self::Error> {
        Self::from_records(records)
    }
}

impl<'a> TryFrom<Box<[Record<'a>]>> for ValidModule<'a> {
    type Error = Error;

    fn try_from(records: Box<[Record<'a>]>) -> Result<Self, Self::Error> {
        Self::try_from(records.into_vec())
    }
}

impl<'a, const N: usize> TryFrom<[Record<'a>; N]> for ValidModule<'a> {
    type Error = Error;

    fn try_from(records: [Record<'a>; N]) -> Result<Self, Self::Error> {
        Self::from_records(records)
    }
}

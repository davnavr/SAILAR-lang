//! Module to perform validation of SAILAR code.
//!
//! Validation ensures that the contents of a SAILAR module are correct, without having to resolve any imports.

use crate::helper::borrow::CowBox;
use crate::index;
use crate::record::{self, Record};
use crate::signature;
use std::borrow::Cow;
use std::fmt::{Display, Formatter};

/// The error type used when an index in a module is not valid.
#[derive(Clone, Debug, thiserror::Error)]
pub struct InvalidIndexError {
    index: usize,
    maximum_index: Option<usize>,
    name: &'static str,
}

impl InvalidIndexError {
    pub(crate) fn new<I: index::Index>(index: I, maximum_index: Option<usize>) -> Self {
        Self {
            index: index.into(),
            maximum_index,
            name: I::name(),
        }
    }
}

impl Display for InvalidIndexError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} index {} is not valid", self.name, self.index)?;
        if let Some(maximum) = self.maximum_index {
            write!(f, ", maximum valid index is {}", maximum)?;
        }
        Ok(())
    }
}

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
    #[error(transparent)]
    InvalidIndex(#[from] InvalidIndexError),
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
    pub type_signatures: Vec<Cow<'a, signature::Type>>,
    pub function_signatures: Vec<Cow<'a, signature::Function>>,
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
    fn validate(mut contents: ModuleContents<'a>, metadata_fields: Vec<record::MetadataField<'a>>) -> Result<Self, Error> {
        fn get_index_validator<I: index::Index>(length: usize) -> impl Fn(I) -> Result<(), Error> {
            move |index: I| {
                let index = index.into();
                if index < length {
                    Ok(())
                } else {
                    Err(InvalidIndexError {
                        index,
                        maximum_index: if length == 0 { None } else { Some(length - 1) },
                        name: I::name(),
                    })?
                }
            }
        }

        let check_type_signature_index = get_index_validator(contents.type_signatures.len());
        let check_function_signature_index = get_index_validator(contents.function_signatures.len());

        for signature in contents.type_signatures.iter() {
            match signature.as_ref() {
                signature::Type::FixedInteger(_)
                | signature::Type::F32
                | signature::Type::F64
                | signature::Type::SAddr
                | signature::Type::UAddr
                | signature::Type::RawPtr(None) => (),
                signature::Type::RawPtr(Some(pointee)) => {
                    // TODO: Check for cycle
                    check_type_signature_index(*pointee)?;
                }
                signature::Type::FuncPtr(signature) => {
                    // TODO: Check for cycle
                    check_function_signature_index(*signature)?;
                }
            }
        }

        // TODO: Perform validation here.

        for field in metadata_fields.into_iter() {
            match field {
                record::MetadataField::ModuleIdentifier(identifier) => contents.module_identifiers.push(identifier),
                record::MetadataField::EntryPoint(entry_point) => {
                    if let Some(defined) = contents.entry_point {
                        return Err(ErrorKind::DuplicateEntryPoint {
                            defined,
                            duplicate: entry_point,
                        })?;
                    }
                    // else if entry point OOB

                    contents.entry_point = Some(entry_point);
                }
            }
        }

        Ok(Self { contents })
    }

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
                Record::TypeSignature(signature) => contents.type_signatures.push(signature),
                Record::FunctionSignature(signature) => contents.function_signatures.push(signature),
                bad => todo!("validate {:?}", bad),
            }
        }

        Ok(Self::validate(contents, metadata_fields))
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

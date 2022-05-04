//! Types that represent records in a SAILAR module binary.

use crate::binary::signature;
use crate::Id;

/// Indicates what kind of content is contained in a record.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum Type {
    HeaderField = 0,
    Array = 1,
    Identifier = 2,
    TypeSignature = 3,
    FunctionSignature = 4,
    Data = 5,
    Code = 6,
    //ModuleImport = 7,
    //FunctionImport = 8,
    //StructureImport = 9,
    //GlobalImport = 10,
    //FunctionDefinition = 11,
    //StructureDefinition = 12,
    //GlobalDefinition = 13,
    //FunctionInstantiation = 14,
    //StructureInstantiation = 15,
    //Namespace = 16,
    //ExceptionClassImport = 17,
    //ExceptionClassDefinition = 18,
    //AnnotationClassImport = 19,
    //AnnotationClassDefinition = 20,
    //DebuggingInformation = 21,
}

impl From<Type> for u8 {
    fn from(value: Type) -> u8 {
        value as u8
    }
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("{value:#02X} is not a valid record type")]
pub struct InvalidTypeError {
    value: u8,
}

impl TryFrom<u8> for Type {
    type Error = InvalidTypeError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value <= Type::Code.into() {
            Ok(unsafe { std::mem::transmute::<u8, Self>(value) })
        } else {
            Err(InvalidTypeError { value })
        }
    }
}

#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum Array<'a> {
    HeaderField(Vec<HeaderField<'a>>),
    Identifier(Vec<&'a Id>),
    TypeSignature(Vec<&'a signature::Type<'a>>),
    FunctionSignature(Vec<&'a signature::Function>),
}

impl Array<'_> {
    pub fn item_type(&self) -> Type {
        match self {
            Self::HeaderField(_) => Type::HeaderField,
            Self::Identifier(_) => Type::Identifier,
            Self::TypeSignature(_) => Type::TypeSignature,
            Self::FunctionSignature(_) => Type::FunctionSignature,
        }
    }
}

#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum HeaderField<'a> {
    ModuleIdentifier { name: &'a Id, version: &'a [usize] },
}

impl HeaderField<'_> {
    pub fn field_name(&self) -> &'static Id {
        let name = match self {
            Self::ModuleIdentifier { .. } => "ModuleIdentifier",
        };

        unsafe { Id::from_str_unchecked(name) }
    }
}

// TODO: Array records only? Doesn't make sense to generate a whole entire record byte count and all just for one identifier, field, function, etc.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum Record<'a> {
    HeaderField(HeaderField<'a>),
    Identifier(&'a Id),
    TypeSignature(&'a signature::Type<'a>),
    FunctionSignature(&'a signature::Function),
    Array(Array<'a>),
}

impl Record<'_> {
    pub fn record_type(&self) -> Type {
        match self {
            Self::HeaderField(_) => Type::HeaderField,
            Self::Identifier(_) => Type::Identifier,
            Self::TypeSignature(_) => Type::TypeSignature,
            Self::FunctionSignature(_) => Type::FunctionSignature,
            Self::Array(_) => Type::Array,
        }
    }
}

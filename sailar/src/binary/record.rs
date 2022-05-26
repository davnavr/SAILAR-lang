//! Types that represent records in a SAILAR module binary.

use crate::binary::{index, instruction, signature};
use crate::helper::borrow::CowBox;
use crate::{Id, Identifier};
use std::borrow::Cow;

/// Indicates whether a definition in a module can be imported by other modules.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[repr(u8)]
pub enum Export {
    Private = 0,
    Public = 1,
}

impl Default for Export {
    #[inline]
    fn default() -> Self {
        Self::Private
    }
}

#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub struct ModuleIdentifier<'a> {
    name: Cow<'a, Id>,
    version: CowBox<'a, [usize]>,
}

impl<'a> ModuleIdentifier<'a> {
    pub fn new(name: Cow<'a, Id>, version: CowBox<'a, [usize]>) -> Self {
        Self { name, version }
    }

    #[inline]
    pub fn name(&self) -> &Id {
        &self.name
    }

    #[inline]
    pub fn version(&self) -> &[usize] {
        &self.version
    }
}

#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub enum MetadataField<'a> {
    ModuleIdentifier(ModuleIdentifier<'a>),
}

impl MetadataField<'_> {
    pub fn field_name(&self) -> &'static Id {
        let name = match self {
            Self::ModuleIdentifier(_) => "id",
        };

        // Safety: all above names are assumed to be valid.
        unsafe { Id::from_str_unchecked(name) }
    }
}

#[derive(Debug, Eq, PartialEq)]
#[repr(transparent)]
pub struct DataArray(pub [u8]);

impl DataArray {
    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }

    #[inline]
    pub fn from_bytes<'a>(bytes: &'a [u8]) -> &'a Self {
        unsafe {
            // Safety: Layout of data array is the same.
            std::mem::transmute::<&'a _, &'a _>(bytes)
        }
    }
}

impl<'a> From<&'a DataArray> for &'a [u8] {
    #[inline]
    fn from(data: &'a DataArray) -> &'a [u8] {
        data.as_bytes()
    }
}

impl<'a> From<&'a [u8]> for &'a DataArray {
    #[inline]
    fn from(bytes: &'a [u8]) -> &'a DataArray {
        DataArray::from_bytes(bytes)
    }
}

impl std::borrow::Borrow<DataArray> for Box<[u8]> {
    #[inline]
    fn borrow(&self) -> &DataArray {
        self.as_ref().into()
    }
}

impl std::borrow::ToOwned for DataArray {
    type Owned = Box<[u8]>;

    #[inline]
    fn to_owned(&self) -> Self::Owned {
        Box::from(self.as_bytes())
    }
}

impl std::cmp::PartialEq<[u8]> for DataArray {
    fn eq(&self, other: &[u8]) -> bool {
        self.as_bytes() == other
    }
}

impl<const N: usize> std::cmp::PartialEq<[u8; N]> for DataArray {
    fn eq(&self, other: &[u8; N]) -> bool {
        self.as_bytes() == other.as_slice()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CodeBlock<'a> {
    register_types: CowBox<'a, [index::TypeSignature]>,
    input_count: usize,
    result_count: usize,
    instructions: CowBox<'a, [instruction::Instruction]>,
}

impl<'a> CodeBlock<'a> {
    /// Creates a code block with the specified register types, indicating the types of the input, result, and temporary registers in that order.
    ///
    /// # Panics
    ///
    /// Panics if the number of input and result registers exceeds the total number of register types.
    pub fn from_types(
        register_types: CowBox<'a, [index::TypeSignature]>,
        input_count: usize,
        result_count: usize,
        instructions: CowBox<'a, [instruction::Instruction]>,
    ) -> Self {
        assert!(register_types.len() >= input_count + result_count);

        Self {
            register_types,
            input_count,
            result_count,
            instructions,
        }
    }

    #[inline]
    pub(crate) fn register_types(&self) -> &[index::TypeSignature] {
        std::borrow::Borrow::borrow(&self.register_types)
    }

    #[inline]
    pub fn input_count(&self) -> usize {
        self.input_count
    }

    #[inline]
    pub fn result_count(&self) -> usize {
        self.result_count
    }

    #[inline]
    pub fn temporary_count(&self) -> usize {
        self.register_types().len() - self.input_count - self.result_count
    }

    pub fn input_types(&self) -> &[index::TypeSignature] {
        &self.register_types()[0..self.input_count]
    }

    pub fn result_types(&self) -> &[index::TypeSignature] {
        &self.register_types()[self.input_count..self.input_count + self.result_count]
    }

    pub fn temporary_types(&self) -> &[index::TypeSignature] {
        &self.register_types()[self.input_count + self.result_count..]
    }

    #[inline]
    pub fn instructions(&self) -> &[instruction::Instruction] {
        &self.instructions
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum FunctionBody<'a> {
    Definition(index::CodeBlock),
    Foreign {
        library: index::Identifier,
        entry_point: Cow<'a, Id>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition<'a> {
    export: Export,
    signature: index::FunctionSignature,
    symbol: Cow<'a, Id>,
    body: FunctionBody<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionInstantiation {
    template: index::FunctionInstantiation,
}

impl FunctionInstantiation {
    pub fn from_template(template: index::FunctionInstantiation) -> Self {
        Self { template }
    }

    #[inline]
    pub fn template(&self) -> index::FunctionInstantiation {
        self.template
    }
}

impl<'a> FunctionDefinition<'a> {
    pub fn new(export: Export, signature: index::FunctionSignature, symbol: Cow<'a, Id>, body: FunctionBody<'a>) -> Self {
        Self {
            export,
            signature,
            symbol,
            body,
        }
    }

    #[inline]
    pub fn export(&self) -> Export {
        self.export
    }

    #[inline]
    pub fn signature(&self) -> index::FunctionSignature {
        self.signature
    }

    #[inline]
    pub fn symbol(&self) -> &Id {
        &self.symbol
    }

    #[inline]
    pub fn body(&self) -> &FunctionBody<'a> {
        &self.body
    }

    pub fn flag_bits(&self) -> u8 {
        let mut flags = self.export as u8;
        if let FunctionBody::Foreign { .. } = &self.body {
            flags |= 0b10;
        }
        flags
    }
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("{value:#02X} is not a valid record type")]
pub struct InvalidTypeError {
    value: u8,
}

macro_rules! record_types {
    ({
        $($(#[$case_meta:meta])* $case_name:ident$(($($case_argument_name:ident: $case_argument:ty,)*))? = $case_number:literal,)*
    }) => {
        /// Indicates what kind of content is contained in a record.
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        #[repr(u8)]
        pub enum Type {
            Array = 1,
            $($case_name = $case_number,)*
        }

        impl TryFrom<u8> for Type {
            type Error = InvalidTypeError;

            fn try_from(value: u8) -> Result<Self, Self::Error> {
                match value {
                    1 => Ok(Self::Array),
                    $(_ if value == $case_number => Ok(Self::$case_name),)*
                    _ => Err(InvalidTypeError { value })
                }
            }
        }

        #[derive(Clone, Debug, PartialEq)]
        #[non_exhaustive]
        pub enum Record<'a> {
            $($(#[$case_meta])* $case_name$(($($case_argument,)*))?,)*
        }

        impl Record<'_> {
            pub fn record_type(&self) -> Type {
                match self {
                    $(Self::$case_name$(($($case_argument_name,)*))? => Type::$case_name,)*
                }
            }
        }
    };
}

record_types!({
    MetadataField(_field: MetadataField<'a>,) = 0,
    // Array records are a special case handled by the reading and writing APIs, and so explicit construction is not allowed here.
    //Array = 1,
    Identifier(_identifier: Cow<'a, Id>,) = 2,
    TypeSignature(_signature: Cow<'a, signature::Type>,) = 3,
    FunctionSignature(_signature: Cow<'a, signature::Function>,) = 4,
    Data(_bytes: Cow<'a, DataArray>,) = 5,
    CodeBlock(_code: CowBox<'a, CodeBlock<'a>>,) = 6,
    //ModuleImport = 7,
    //FunctionImport = 8,
    //StructureImport = 9,
    //GlobalImport = 10,
    FunctionDefinition(_definition: CowBox<'a, FunctionDefinition<'a>>,) = 11,
    //StructureDefinition = 12,
    //GlobalDefinition = 13,
    FunctionInstantiation(_instantiation: CowBox<'a, FunctionInstantiation>,) = 14,
    //StructureInstantiation = 15,
    //Namespace = 16,
    //ExceptionClassImport = 17,
    //ExceptionClassDefinition = 18,
    //AnnotationClassImport = 19,
    //AnnotationClassDefinition = 20,
    //DebuggingInformation = 21,
});

impl From<Type> for u8 {
    fn from(value: Type) -> u8 {
        value as u8
    }
}

impl From<Identifier> for Record<'_> {
    #[inline]
    fn from(identifier: Identifier) -> Self {
        Self::Identifier(Cow::Owned(identifier))
    }
}

impl From<signature::Type> for Record<'_> {
    #[inline]
    fn from(signature: signature::Type) -> Self {
        Self::TypeSignature(Cow::Owned(signature))
    }
}

impl From<signature::Function> for Record<'_> {
    #[inline]
    fn from(signature: signature::Function) -> Self {
        Self::FunctionSignature(Cow::Owned(signature))
    }
}

impl<'a> From<CodeBlock<'a>> for Record<'a> {
    #[inline]
    fn from(block: CodeBlock<'a>) -> Self {
        Self::CodeBlock(CowBox::Boxed(Box::new(block)))
    }
}

impl<'a> From<FunctionDefinition<'a>> for Record<'a> {
    #[inline]
    fn from(definition: FunctionDefinition<'a>) -> Self {
        Self::FunctionDefinition(CowBox::Boxed(Box::new(definition)))
    }
}

impl From<FunctionInstantiation> for Record<'_> {
    #[inline]
    fn from(instantiation: FunctionInstantiation) -> Self {
        Self::FunctionInstantiation(CowBox::Boxed(Box::new(instantiation)))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn size_of_record_is_acceptable() {
        assert!(std::mem::size_of::<crate::binary::record::Record>() <= 72)
    }
}

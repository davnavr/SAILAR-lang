//! Types that represent records in a SAILAR module binary.

use crate::helper::borrow::CowBox;
use crate::identifier::{Id, Identifier};
use crate::index;
use crate::instruction;
use crate::num::VarU28;
use crate::signature;
use std::borrow::Cow;

/// Indicates whether a definition in a module can be imported by other modules.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[repr(u8)]
pub enum ExportKind {
    /// The definition is not exported and does not have a symbol.
    Hidden = 0,
    /// The definition has a symbol, but is not exported.
    Private = 1,
    /// The definition has a symbol and can be imported by other modules.
    Export = 2,
}

impl ExportKind {
    pub const fn bits(self) -> u8 {
        self as u8
    }
}

impl Default for ExportKind {
    #[inline]
    fn default() -> Self {
        Self::Hidden
    }
}

/// Assigns a symbol to a definition and indicates if it is exported.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Export<'a> {
    /// The definition is not exported.
    Hidden,
    /// The definition has a symbol, but is not exported.
    Private(Cow<'a, Id>),
    /// The definition is exported.
    Export(Cow<'a, Id>),
}

impl<'a> Export<'a> {
    pub fn new_export_owned(symbol: Identifier) -> Self {
        Self::Export(Cow::Owned(symbol))
    }

    pub fn new_export_borrowed(symbol: &'a Id) -> Self {
        Self::Export(Cow::Borrowed(symbol))
    }

    pub fn kind(&self) -> ExportKind {
        match self {
            Self::Hidden => ExportKind::Hidden,
            Self::Private(_) => ExportKind::Private,
            Self::Export(_) => ExportKind::Export,
        }
    }

    /// Gets the symbol of the definition.
    pub fn symbol(&self) -> Option<&Id> {
        match self {
            Self::Hidden => None,
            Self::Private(symbol) | Self::Export(symbol) => Some(std::convert::AsRef::as_ref(symbol)),
        }
    }
}

impl Default for Export<'_> {
    #[inline]
    fn default() -> Self {
        Self::Hidden
    }
}

/// Specifies the name and version of the module.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct ModuleIdentifier<'a> {
    name: Cow<'a, Id>,
    version: CowBox<'a, [VarU28]>,
}

impl<'a> ModuleIdentifier<'a> {
    pub fn new(name: Cow<'a, Id>, version: CowBox<'a, [VarU28]>) -> Self {
        Self { name, version }
    }

    pub fn new_owned<V: Into<Box<[VarU28]>>>(name: Identifier, version: V) -> Self {
        Self::new(Cow::Owned(name), CowBox::Boxed(version.into()))
    }

    pub fn new_borrowed(name: &'a Id, version: &'a [VarU28]) -> Self {
        Self::new(Cow::Borrowed(name), CowBox::Borrowed(version))
    }

    #[inline]
    pub fn name(&self) -> &Id {
        &self.name
    }

    #[inline]
    pub fn version(&self) -> &[VarU28] {
        &self.version
    }
}

/// Contains information describing the module.
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
    /// Creates a code block with the specified register types, indicating the types of the input registers, results, and
    /// temporary registers in that order.
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

    pub fn new<A, R, T, I>(input_types: A, result_types: R, temporary_types: T, instructions: I) -> Self
    where
        A: Into<CowBox<'a, [index::TypeSignature]>>,
        R: Into<CowBox<'a, [index::TypeSignature]>>,
        T: Into<CowBox<'a, [index::TypeSignature]>>,
        I: Into<CowBox<'a, [instruction::Instruction]>>
    {
        let input_types: CowBox<'a, [_]> = input_types.into();
        let result_types: CowBox<'a, [_]> = result_types.into();
        let temporary_types: CowBox<'a, [_]> = temporary_types.into();
        let input_count = input_types.len();
        let result_count = result_types.len();
        let register_types = {
            if result_types.is_empty() && temporary_types.is_empty() {
                input_types
            } else if input_types.is_empty() && temporary_types.is_empty() {
                result_types
            } else if input_types.is_empty() && result_types.is_empty() {
                temporary_types
            } else {
                CowBox::Boxed(input_types.iter().copied().chain(result_types.iter().copied()).chain(temporary_types.iter().copied()).collect())
            }
        };

        Self::from_types(register_types, input_count, result_count, instructions.into())
    }

    pub fn register_types(&self) -> &[index::TypeSignature] {
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

    /// The types of the results of this [`CodeBlock`]. These are the types of the values that are expected to be used in the
    /// block's `ret` instruction, and should be empty if the block branches to another block instead.
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

impl FunctionBody<'_> {
    pub fn is_foreign(&self) -> bool {
        match self {
            Self::Definition(_) => false,
            Self::Foreign { .. } => true,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionInstantiation {
    template: index::FunctionTemplate,
}

impl FunctionInstantiation {
    pub fn from_template(template: index::FunctionTemplate) -> Self {
        Self { template }
    }

    #[inline]
    pub fn template(&self) -> index::FunctionTemplate {
        self.template
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(transparent)]
pub struct FunctionDefinitionFlags(u16);

impl FunctionDefinitionFlags {
    pub fn try_from_bits(value: VarU28) -> Option<Self> {
        let bits = u16::try_from(value).ok()?;
        if (bits & 0xFFF0 != 0) || (bits & 0b1100 == 0b1100) || (bits & 0b10 == 0b10) {
            None
        } else {
            Some(Self(bits))
        }
    }

    pub const fn value(self) -> VarU28 {
        VarU28::from_u16(self.0)
    }

    pub const fn is_body_foreign(self) -> bool {
        self.0 & 1 == 1
    }

    pub const fn export_kind(self) -> ExportKind {
        match (self.0 >> 2) & 0b11 {
            0b00 => ExportKind::Hidden,
            0b01 => ExportKind::Private,
            0b10 => ExportKind::Export,
            _ => unreachable!(),
        }
    }
}

impl From<FunctionDefinitionFlags> for VarU28 {
    fn from(flags: FunctionDefinitionFlags) -> Self {
        flags.value()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition<'a> {
    export: Export<'a>,
    signature: index::FunctionSignature,
    body: FunctionBody<'a>,
}

impl<'a> FunctionDefinition<'a> {
    pub fn new(export: Export<'a>, signature: index::FunctionSignature, body: FunctionBody<'a>) -> Self {
        Self { export, signature, body }
    }

    #[inline]
    pub fn export(&self) -> &Export<'a> {
        &self.export
    }

    #[inline]
    pub fn signature(&self) -> index::FunctionSignature {
        self.signature
    }

    #[inline]
    pub fn body(&self) -> &FunctionBody<'a> {
        &self.body
    }

    pub fn flags(&self) -> FunctionDefinitionFlags {
        // Bit 0 indicates whether body is foreign
        let mut flags = if self.body.is_foreign() { 1u16 } else { 0 };
        // Bit 1 will be used to indicate if a generic parameter count is present
        // Bits 2 to 3 contains the export kind
        flags |= u16::from(self.export().kind().bits()) << 2u8;
        FunctionDefinitionFlags(flags)
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

impl<'a> From<MetadataField<'a>> for Record<'a> {
    fn from(metadata: MetadataField<'a>) -> Self {
        Self::MetadataField(metadata)
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
        assert!(std::mem::size_of::<crate::record::Record>() <= 72)
    }
}

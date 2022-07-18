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
pub enum ExportKind {
    /// The definition is not exported and does not have a symbol.
    Hidden,
    /// The definition has a symbol, but is not exported.
    Private,
    /// The definition has a symbol and can be imported by other modules.
    Export,
}

impl Default for ExportKind {
    #[inline]
    fn default() -> Self {
        Self::Hidden
    }
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("cannot encode symbol with length of {length} bytes")]
pub struct SymbolEncodingError {
    length: usize,
}

/// Assigns a symbol to a definition and indicates if it is exported.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Export<'data> {
    Hidden,
    PrivateBorrowed(&'data Id),
    PrivateOwned(Identifier),
    ExportBorrowed(&'data Id),
    ExportOwned(Identifier),
}

impl<'data> Export<'data> {
    pub fn new_export(symbol: Cow<'data, Id>) -> Self {
        match symbol {
            Cow::Owned(symbol) => Self::ExportOwned(symbol),
            Cow::Borrowed(symbol) => Self::ExportBorrowed(symbol),
        }
    }

    pub fn kind(&self) -> ExportKind {
        match self {
            Self::Hidden => ExportKind::Hidden,
            Self::PrivateBorrowed(_) | Self::PrivateOwned(_) => ExportKind::Private,
            Self::ExportBorrowed(_) | Self::ExportOwned(_) => ExportKind::Export,
        }
    }

    /// Gets the symbol of the definition.
    pub fn symbol(&self) -> Option<&Id> {
        match self {
            Self::Hidden => None,
            Self::PrivateBorrowed(symbol) | Self::ExportBorrowed(symbol) => Some(symbol),
            Self::PrivateOwned(symbol) | Self::ExportOwned(symbol) => Some(symbol.as_id()),
        }
    }

    /// Gets the variable-length integer indicating the length of the symbol and whether it is externally visible.
    pub fn flag_bits(&self) -> Result<VarU28, SymbolEncodingError> {
        match self.symbol() {
            None => Ok(VarU28::from_u8(0)),
            Some(symbol) => {
                let length = symbol.len();
                match VarU28::try_from(length) {
                    Ok(bits) => {
                        let flag = VarU28::from_u8(if self.kind() == ExportKind::Export { 1 } else { 0 });
                        Ok(flag | bits)
                    }
                    Err(_) => Err(SymbolEncodingError { length }),
                }
            }
        }
    }
}

impl Default for Export<'_> {
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
    /// Specifies the entry point function of the module.
    EntryPoint(index::Function),
}

impl MetadataField<'_> {
    pub fn field_name(&self) -> &'static Id {
        let name = match self {
            Self::ModuleIdentifier(_) => "id",
            Self::EntryPoint(_) => "main",
        };

        // Safety: all above names are assumed to be valid.
        unsafe { Id::from_str_unchecked(name) }
    }
}

#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub struct CodeBlock<'a> {
    /// Contains the types of all input registers, results, and temporary registers in that order.
    pub register_types: CowBox<'a, [index::TypeSignature]>,
    pub input_count: usize,
    pub result_count: usize,
    pub instructions: CowBox<'a, [instruction::Instruction]>,
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
        I: Into<CowBox<'a, [instruction::Instruction]>>,
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
                CowBox::Boxed(
                    input_types
                        .iter()
                        .copied()
                        .chain(result_types.iter().copied())
                        .chain(temporary_types.iter().copied())
                        .collect(),
                )
            }
        };

        Self::from_types(register_types, input_count, result_count, instructions.into())
    }

    pub fn temporary_count(&self) -> usize {
        self.register_types.len() - self.input_count - self.result_count
    }

    /// Gets the total number of registers defined in the block.
    pub fn register_count(&self) -> usize {
        self.register_types.len() - self.result_count
    }

    pub fn input_types(&self) -> &[index::TypeSignature] {
        &self.register_types[0..self.input_count]
    }

    /// The types of the results of this [`CodeBlock`]. These are the types of the values that are expected to be used in the
    /// block's `ret` instruction, and should be empty if the block branches to another block instead.
    pub fn result_types(&self) -> &[index::TypeSignature] {
        &self.register_types[self.input_count..self.input_count + self.result_count]
    }

    pub fn temporary_types(&self) -> &[index::TypeSignature] {
        &self.register_types[self.input_count + self.result_count..]
    }

    pub fn to_function_signature(&self) -> signature::Function {
        signature::Function::new(self.input_types(), self.result_types())
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
#[non_exhaustive]
pub struct FunctionTemplate<'data> {
    pub export: Export<'data>,
    pub signature: index::FunctionSignature,
    pub entry_block: index::CodeBlock,
}

impl<'data> FunctionTemplate<'data> {
    pub fn new(export: Export<'data>, signature: index::FunctionSignature, entry_block: index::CodeBlock) -> Self {
        Self {
            export,
            signature,
            entry_block,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub struct Function<'data> {
    pub template: index::FunctionTemplate,
    _placeholder: &'data (),
}

impl<'data> Function<'data> {
    pub fn with_template(template: index::FunctionTemplate) -> Self {
        Self {
            template,
            _placeholder: &(),
        }
    }
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("{value:#02X} is not a valid record type")]
pub struct InvalidTypeError {
    value: u8,
}

macro_rules! record_types {
    (for<$lifetimes:tt> {
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
        pub enum Record<$lifetimes> {
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

record_types!(for<'data> {
    MetadataField(_field: MetadataField<'data>,) = 0,
    // Array records are a special case handled by the reading and writing APIs, and so explicit construction is not allowed here.
    //Array = 1,
    Identifier(_identifier: Cow<'data, Id>,) = 2,
    TypeSignature(_signature: signature::Type,) = 3,
    FunctionSignature(_signature: signature::Function,) = 4,
    Data(_bytes: Cow<'data, [u8]>,) = 5,
    CodeBlock(_code: CodeBlock<'data>,) = 6,
    //ModuleImport = 7,
    //FunctionImport = 8,
    //StructureImport = 9,
    //GlobalImport = 10,
    FunctionTemplate(_template: FunctionTemplate<'data>,) = 11,
    //StructureDefinition = 12,
    //GlobalDefinition = 13,
    Function(_function: Function<'data>,) = 14,
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

impl<'data> From<MetadataField<'data>> for Record<'data> {
    fn from(metadata: MetadataField<'data>) -> Self {
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
        Self::TypeSignature(signature)
    }
}

impl From<signature::Function> for Record<'_> {
    #[inline]
    fn from(signature: signature::Function) -> Self {
        Self::FunctionSignature(signature)
    }
}

impl<'data> From<CodeBlock<'data>> for Record<'data> {
    #[inline]
    fn from(block: CodeBlock<'data>) -> Self {
        Self::CodeBlock(block)
    }
}

impl<'data> From<FunctionTemplate<'data>> for Record<'data> {
    fn from(template: FunctionTemplate<'data>) -> Self {
        Self::FunctionTemplate(template)
    }
}

impl<'data> From<Function<'data>> for Record<'data> {
    fn from(function: Function<'data>) -> Self {
        Self::Function(function)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn size_of_record_is_acceptable() {
        assert!(std::mem::size_of::<crate::record::Record>() <= 72)
    }
}

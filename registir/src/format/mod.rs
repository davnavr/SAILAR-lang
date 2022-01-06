/// Contains types that describe the properties of certain structures within the module file.
pub mod flags;
mod identifier;
/// Contains types representing indices to various structures within the module file.
pub mod indices;
pub mod instruction_set;
/// Contains types representing the integer types used within the module file.
///
/// Unless specified otherwise, all unsigned and signed integers are in little-endian order.
pub mod numeric;
/// Contains representations of common structures found within the module file.
pub mod structures;
/// Contains types representing the type system.
pub mod type_system;
pub mod versioning;

pub use identifier::Identifier;
pub use structures::{ByteLengthEncoded as LenBytes, LengthEncodedVector as LenVec};
pub use type_system::AnyType as TypeSignature;
pub use versioning::{Format as FormatVersion, Numbers as VersionNumbers};

pub type LenVecBytes<T> = LenBytes<LenVec<T>>;

/// The magic number for `binmdl` files.
pub static MAGIC: &[u8] = "binmdl\0".as_bytes();

/// Used to organize the structs, functions, and globals of a module.
/// 
/// # Structure
/// - [`Namespace::name`]
/// - [`Namespace::flags()`]
/// - [`Namespace::parent`] (if flags indicate that a parent is present)
#[derive(Debug, Eq, PartialEq)]
pub struct Namespace {
    pub name: indices::Identifier,
    /// The parent namespace that contains the current namespace.
    pub parent: Option<indices::Namespace>,
    pub structs: LenVec<indices::StructDefinition>,
    pub globals: LenVec<indices::GlobalDefinition>,
    pub functions: LenVec<indices::FunctionDefinition>,
    //pub annotations: LengthEncodedVector<>,
}

impl Namespace {
    pub fn flags(&self) -> flags::Namespace {
        if self.parent.is_some() {
            flags::Namespace::HAS_PARENT
        } else {
            flags::Namespace::NONE
        }
    }
}

/// Describes the return types and parameter types of a function.
#[derive(Debug, Default, Eq, Hash, PartialEq, PartialOrd)]
pub struct FunctionSignature {
    /// The types of the values returned by the function.
    pub return_types: LenVec<indices::TypeSignature>,
    pub parameter_types: LenVec<indices::TypeSignature>,
}

#[derive(Debug)]
pub struct CodeExceptionHandler {
    /// Indicates the block that control will transfer to when an exception is thrown, relative to the current block.
    pub catch_block: instruction_set::JumpTarget,
    /// Specifies the input register of the [`catch_block`] that the exception object is stored into when an exception is thrown.
    /// If omitted, the exception object is ignored.
    pub exception_register: Option<indices::InputRegister>,
}

/// # Structure
/// - [`CodeBlock::flags()`]
/// - [`CodeBlock::input_register_count`]
/// - [`CodeBlock::exception_handler`]
/// - [`CodeBlock::instructions`]
#[derive(Debug)]
pub struct CodeBlock {
    /// A variable-length integer placed after the flags indicating the number of input registers for this block.
    ///
    /// For the entry block's count, this should match the number of arguments of the method.
    pub input_register_count: numeric::UInteger,
    /// Specifies the block that control should be transferred to if an exception is thrown inside this block.
    pub exception_handler: Option<CodeExceptionHandler>,
    /// The instructions of the block.
    ///
    /// Both the byte length and the actual number of instructions are included to simplify parsing.
    pub instructions: LenVecBytes<instruction_set::Instruction>,
}

impl CodeBlock {
    /// Byte at the beginning of the block describing how it handles exceptions.
    pub fn flags(&self) -> flags::CodeBlock {
        match self.exception_handler {
            None => flags::CodeBlock::NO_EXCEPTION_HANDLING,
            Some(CodeExceptionHandler {
                exception_register: Some(_),
                ..
            }) => flags::CodeBlock::EXCEPTION_HANDLER_IGNORES_EXCEPTION,
            Some(CodeExceptionHandler {
                exception_register: None,
                ..
            }) => flags::CodeBlock::EXCEPTION_HANDLER_STORES_EXCEPTION,
        }
    }
}

#[derive(Debug)]
pub struct Code {
    /// The block that will be executed when the function is called, corresponds to block index `0`.
    pub entry_block: CodeBlock,
    pub blocks: LenVec<CodeBlock>,
}

#[derive(Clone, Debug, Default)]
#[repr(transparent)]
pub struct DataArray(pub LenVec<u8>);

impl std::ops::Deref for DataArray {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        &self.0
    }
}

#[derive(Debug)]
pub struct StructImport {
    pub module: indices::Module,
    pub symbol: indices::Identifier,
    //pub type_parameters: (),
}


#[derive(Debug)]
pub struct GlobalImport {
    pub module: indices::Module,
    pub symbol: indices::Identifier,
    pub signature: indices::TypeSignature,
}

#[derive(Debug)]
pub struct FieldImport {
    pub owner: indices::Struct,
    pub symbol: indices::Identifier,
    pub signature: indices::TypeSignature,
}

#[derive(Debug)]
pub struct FunctionImport {
    pub module: indices::Module,
    pub symbol: indices::Identifier,
    pub signature: indices::MethodSignature,
    //pub type_parameters: (),
}

/// Contains the structs, fields, and methods imported by a module.
#[derive(Debug)]
pub struct ModuleImports {
    pub imported_modules: LenVecBytes<ModuleIdentifier>,
    pub imported_structs: LenVecBytes<StructImport>,
    pub imported_globals: LenVecBytes<GlobalImport>,
    pub imported_fields: LenVecBytes<FieldImport>,
    pub imported_functions: LenVecBytes<FunctionImport>,
}

pub type Symbol = Option<indices::Identifier>;

/// Represents a collection of fields which form a type.
/// 
/// # Structure
/// - [`name`]
/// - [`Struct::flags()`]
/// - [`symbol`] (if [`flags::Struct::IS_EXPORT`] is set)
/// - [`fields`]
#[derive(Debug)]
pub struct Struct {
    pub name: indices::Identifier,
    pub symbol: Symbol,
    pub layout: indices::TypeLayout, // TODO: Could merge field vector and layout struct, to ensure that for explicit layouts, the index of a field is right next to its offset.
    /// The list of fields that make up this struct, the [`Field::owner`] must point to the current struct.
    pub fields: LenVec<indices::FieldDefinition>,
    //pub annotations: LengthEncodedVector<>,
    //pub type_parameters: (),
}

impl Struct {
    pub fn flags(&self) -> flags::Struct {
        if self.symbol.is_some() {
            flags::Struct::IS_EXPORT
        } else {
            flags::Struct::NONE
        }
    }
}

macro_rules! field_flags {
    ($field_record_type: ty) => {
        impl $field_record_type {
            pub fn flags(&self) -> flags::Field {
                let mut flags = if self.symbol.is_some() {
                    flags::Field::IS_EXPORT
                } else {
                    flags::Field::NONE
                };
                if self.is_mutable {
                    flags |= flags::Field::MUTABLE;
                }
                flags
            }
        }
    };
}

/// Represents a global variable.
/// 
/// # Structure
/// - [`name`]
/// - [`Global::flags()`]
/// - [`symbol`] (if [`flags::Field::IS_EXPORT`] is set)
/// - [`signature`]
#[derive(Debug)]
pub struct Global {
    pub name: indices::Identifier,
    pub is_mutable: bool,
    pub symbol: Symbol,
    pub signature: indices::TypeSignature,
    //pub annotations: LengthEncodedVector<>,
}

field_flags!(Global);

/// Represents a field in a [`Struct`].
/// 
/// # Structure
/// - [`owner`]
/// - [`name`]
/// - [`Field::flags()`]
/// - [`symbol`] (if [`flags::Field::IS_EXPORT`] is set)
/// - [`signature`]
#[derive(Debug)]
pub struct Field {
    pub owner: indices::StructDefinition,
    pub name: indices::Identifier,
    pub is_mutable: bool,
    pub symbol: Symbol,
    pub signature: indices::TypeSignature,
    //pub annotations: LengthEncodedVector<>,
}

field_flags!(Field);

#[derive(Debug)]
pub enum FunctionBody {
    /// Defined in the current module with the specified method body.
    Defined(indices::Code),
    //Abstract,
    /// Defined elsewhere, used by the foreign function interface or to call methods defined in the runtime.
    External {
        library: indices::Identifier,
        entry_point_name: indices::Identifier,
    },
}

/// Represents a method, constructor, or initializer.
///
/// # Structure
/// - [`name`]
/// - [`signature`]
/// - [`symbol`] (if [`flags::Function::IS_EXPORT`] is set)
/// - [`body`]
#[derive(Debug)]
pub struct Function {
    pub name: indices::Identifier,
    pub signature: indices::MethodSignature,
    pub symbol: Symbol,
    pub body: FunctionBody,
    //pub annotations: LengthEncodedVector<>,
    //pub type_parameters: (),
}

impl Function {
    pub fn flags(&self) -> flags::Function {
        let mut flags = if self.symbol.is_some() { flags::Function::IS_EXPORT } else { flags::Function::NONE };
        if let FunctionBody::External { .. } = self.body {
            flags |= flags::Function::IS_EXTERNAL;
        }
        flags
    }
}

/// Contains the types, fields, and methods defined in the module.
///
/// Each type contains a list indices refering to the fields and methods that it defines, and each field or method contains the
/// index of the type that defines it. These indices must exactly match in order for the module to be valid.
#[derive(Debug)]
pub struct ModuleDefinitions {
    pub defined_structs: LenVecBytes<Struct>,
    pub defined_globals: LenVecBytes<Global>,
    pub defined_fields: LenVecBytes<Field>,
    pub defined_functions: LenVecBytes<Function>,
}

#[derive(Debug)]
pub struct FieldOffset {
    pub field: indices::Field,
    pub offset: numeric::UInteger,
}

/// Describes how the fields are a type are layout, starting with a flag byte indicating whether or not the type has an explicit layout.
///
/// # Structure
/// - [`TypeLayout::flags()`]
/// - Size (optional)
/// - Field Offsets (optional)
#[derive(Debug)]
pub enum TypeLayout {
    Unspecified,
    Sequential(Option<numeric::UInteger>),
    Explicit {
        size: numeric::UInteger,
        field_offsets: Vec<FieldOffset>, // TODO: Use a hash map for field offsets?
    },
}

impl TypeLayout {
    /// Flags at the beginning of the structure indicating the kind of layout used by a type's instances.
    pub fn flags(&self) -> flags::TypeLayout {
        match self {
            Self::Unspecified => flags::TypeLayout::Unspecified,
            Self::Sequential(None) => flags::TypeLayout::Sequential,
            Self::Explicit { .. } => flags::TypeLayout::ExplicitOffsets,
            Self::Sequential(Some(_)) => flags::TypeLayout::ExplicitSize,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ModuleIdentifier {
    pub name: Identifier,
    pub version: VersionNumbers,
}

pub static MIN_HEADER_FIELD_COUNT: numeric::UInteger = numeric::UInteger(1);
pub static MAX_HEADER_FIELD_COUNT: numeric::UInteger = MIN_HEADER_FIELD_COUNT;

/// # Structure
/// - [`ModuleHeader::field_count()`]
/// - [`ModuleHeader::identifier`]
#[derive(Debug)]
pub struct ModuleHeader {
    pub identifier: ModuleIdentifier,
}

impl ModuleHeader {
    /// Variable-length unsigned integer placed at the start of the header indicating the number of fields present.
    pub fn field_count(&self) -> numeric::UInteger {
        MAX_HEADER_FIELD_COUNT
    }
}

pub static MIN_MODULE_DATA_COUNT: numeric::UInteger = numeric::UInteger(1);
pub static MAX_MODULE_DATA_COUNT: numeric::UInteger = numeric::UInteger(11);

// TODO: Figure out rules for Symbols, maybe disallow ANY duplicate symbols in a module (e.g. a struct cannot have the same symbol as a global).
// Note that since fields "belong" to a struct, fields with different owners could have duplicate names or even share names with other exports (e.g. a field and a struct could both have the same name, but a two fields belonging to the same struct cannot have the same name)

/// Represents the contents of a `binmdl` file following the [`MAGIC`] number.
///
/// # Structure
/// - [`Module::integer_size`]
/// - [`Module::format_version`]
/// - data_count ([`numeric::UInteger`])
/// - [`Module::header`]
/// - [`Module::identifiers`]
/// - [`Module::namespaces`]
/// - [`Module::type_signatures`]
/// - [`Module::function_signatures`]
/// - [`Module::function_bodies`]
/// - [`Module::data`]
/// - [`Module::imports`]
/// - [`Module::definitions`]
/// - [`Module::entry_point`]
/// - [`Module::type_layouts`]
#[derive(Debug)]
pub struct Module {
    pub integer_size: numeric::IntegerSize,
    pub format_version: FormatVersion,
    /// The header, which identifies and describes the module.
    pub header: LenBytes<ModuleHeader>,
    /// An array containing the names of the structs, namespaces, fields, and functions.
    pub identifiers: LenVecBytes<Identifier>,
    /// An array of the namespaces containing the imported and defined structs, fields, and functions.
    pub namespaces: LenVecBytes<Namespace>,
    pub type_signatures: LenVecBytes<type_system::AnyType>,
    pub function_signatures: LenVecBytes<FunctionSignature>,
    /// An array containing the function bodies of the module.
    pub function_bodies: LenVecBytes<Code>,
    pub data: LenVecBytes<DataArray>,
    pub imports: LenBytes<ModuleImports>,
    pub definitions: LenBytes<ModuleDefinitions>,
    pub type_layouts: LenVecBytes<TypeLayout>,
    /// An optional index specifying the entry point method of the application. It is up to additional constraints made by the
    /// compiler or runtime to determine if the signature of the entry point method is valid.
    ///
    /// When the module does not have an entry point, the byte length is set to zero.
    pub entry_point: LenBytes<Option<indices::FunctionDefinition>>,
    ///// An optional index specifying a function to run once the module is loaded.
    //pub initializer: LenBytes<Option<indices::FunctionDefinition>>,
    //pub debugging_information: LenBytes<Option<>>
}

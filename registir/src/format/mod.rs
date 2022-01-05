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
pub use versioning::Numbers as VersionNumbers;

pub type LenVecBytes<T> = LenBytes<LenVec<T>>;

/// The magic number for `binmdl` files.
pub static MAGIC: &[u8] = "binmdl\0".as_bytes();

/// Describes the return types and parameter types of a method.
#[derive(Debug, Default, Eq, Hash, PartialEq, PartialOrd)]
pub struct MethodSignature {
    /// The types of the values returned by the method.
    pub return_types: LenVec<indices::TypeSignature>,
    pub parameter_types: LenVec<indices::TypeSignature>,
}

#[derive(Debug)]
pub struct CodeExceptionHandler {
    /// Indicates the block that control will transfer to when an exception is thrown, relative to the current block.
    pub catch_block: instruction_set::JumpTarget,
    /// Specifies the input register of the `[catch_block]` that the exception object is stored into when an exception is thrown.
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
    /// The block that will be executed when the method is called, corresponds to block index `0`.
    pub entry_block: CodeBlock,
    pub blocks: LenVec<CodeBlock>,
}

#[derive(Clone, Debug, Default)]
pub struct DataArray(pub LenVec<u8>);

impl std::ops::Deref for DataArray {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        &self.0
    }
}

#[derive(Debug)]
pub struct TypeImport {
    /// Indicates the module that the type was imported from.
    pub module: indices::Module,
    pub name: indices::Identifier,
    pub namespace: indices::Namespace,
    //pub type_parameters: (),
}

#[derive(Debug)]
pub struct FieldImport {
    pub owner: indices::Type,
    pub name: indices::Identifier,
    //pub flags: FieldFlags,
    pub signature: indices::TypeSignature,
}

#[derive(Debug)]
pub struct MethodImport {
    pub owner: indices::Type,
    pub name: indices::Identifier, // TODO: How to handle importing constructors, use flags?
    //pub flags: MethodFlags,
    pub signature: indices::MethodSignature,
    //pub type_parameters: (),
}

/// Contains the types, fields, and methods imported by a module.
///
/// Note that both field and method imports allow their owner to be a defined type instead of an imported type, to allow usage
/// of generics in the future.
#[derive(Debug)]
pub struct ModuleImports {
    pub imported_modules: LenVecBytes<ModuleIdentifier>,
    pub imported_types: LenVecBytes<TypeImport>,
    pub imported_fields: LenVecBytes<FieldImport>,
    pub imported_methods: LenVecBytes<MethodImport>,
}

#[derive(Debug)] // TODO: Custom equality comparison to prevent overriding of method twice?
pub struct MethodOverride {
    /// Specifies the method to override.
    pub declaration: indices::Method,
    /// Specifies the new implementation of the method, the method must be defined in the current type.
    pub implementation: indices::MethodDefinition,
}

// TODO: Since some structs have "flags" that vary depending on the value of other fields, expose new() functions instead of exposing all fields as public.

#[derive(Debug)]
pub struct Type {
    pub name: indices::Identifier,
    pub namespace: indices::Namespace,
    pub visibility: flags::Visibility,
    pub flags: flags::Type,
    pub layout: indices::TypeLayout,
    pub inherited_types: LenVec<indices::Type>,
    pub fields: LenVec<indices::FieldDefinition>,
    pub methods: LenVec<indices::MethodDefinition>,
    pub vtable: LenVec<MethodOverride>,
    //pub annotations: LengthEncodedVector<>,
    //pub type_parameters: (),
}

#[derive(Debug)]
pub struct Field {
    pub owner: indices::TypeDefinition, // TODO: Make this an option, None means its a global and a corresponding flag should be set.
    pub name: indices::Identifier,
    pub visibility: flags::Visibility,
    pub flags: flags::Field,
    pub signature: indices::TypeSignature,
    //pub annotations: LengthEncodedVector<>,
}

#[derive(Debug)]
pub enum MethodBody {
    /// Defined in the current module with the specified method body.
    Defined(indices::Code),
    /// Not defined in the current type, but in a derived type.
    Abstract,
    /// Defined elsewhere, used by the foreign function interface or to call methods defined in the runtime.
    External {
        library: indices::Identifier,
        entry_point_name: indices::Identifier,
    },
}

impl Default for MethodBody {
    fn default() -> Self {
        Self::Abstract
    }
}

impl MethodBody {
    pub fn flags(&self) -> flags::MethodBody {
        match self {
            Self::Defined(_) => flags::MethodBody::DEFINED,
            Self::Abstract => flags::MethodBody::NONE,
            Self::External { .. } => flags::MethodBody::EXTERNAL,
        }
    }
}

/// Represents a method, constructor, or initializer.
///
/// Valid constructors must have the [`MethodFlags::CONSTRUCTOR`] flags set, must have no type parameters, and must
/// not have any return values.
///
/// Valid initializers must have the [`MethodFlags::INITIALIZER`] flag set, and must also have no parameters in addition to the
/// restrictions regarding valid constructors.
///
/// # Structure
/// - [`Method::owner`]
/// - [`Method::name`]
/// - [`Method::visibility`]
/// - [`Method::flags`]
/// - [`Method::implementation_flags()`]
/// - [`Method::signature`]
/// - [`Method::body`]
#[derive(Debug)]
pub struct Method {
    pub owner: indices::TypeDefinition,
    pub name: indices::Identifier,
    pub visibility: flags::Visibility,
    pub flags: flags::Method,
    pub signature: indices::MethodSignature,
    /// The method body, in the binary format, this is where the structure for external methods would go.
    pub body: MethodBody,
    //pub annotations: LengthEncodedVector<>,
    //pub type_parameters: (),
}

impl Method {
    /// Flags that describe how the method is implemented, placed after the [`Method::flags`] field.
    pub fn implementation_flags(&self) -> flags::MethodBody {
        self.body.flags()
    }
}

/// Contains the types, fields, and methods defined in the module.
///
/// Each type contains a list indices refering to the fields and methods that it defines, and each field or method contains the
/// index of the type that defines it. These indices must exactly match in order for the module to be valid.
#[derive(Debug)]
pub struct ModuleDefinitions {
    pub defined_types: LenVecBytes<Type>,
    pub defined_fields: LenVecBytes<Field>,
    pub defined_methods: LenVecBytes<Method>,
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

/// Specifies what version of the module format is being used, placed after the module's integer size field.
#[derive(Debug, Default, Eq, PartialEq, PartialOrd)]
pub struct FormatVersion {
    pub major: numeric::UInteger,
    pub minor: numeric::UInteger,
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

pub type Namespace = LenVec<indices::Identifier>;

/// Represents the contents of a `binmdl` file following the [`MAGIC`] number.
///
/// # Structure
/// - [`Module::integer_size`]
/// - [`Module::format_version`]
/// - [`Module::data_count()`]
/// - [`Module::header`]
/// - [`Module::identifiers`]
/// - [`Module::namespaces`]
/// - [`Module::type_signatures`]
/// - [`Module::method_signatures`]
/// - [`Module::method_bodies`]
/// - [`Module::data_arrays`]
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
    /// An array containing the names of the types, namespaces, fields, and methods.
    pub identifiers: LenVecBytes<Identifier>,
    /// An array of the namespaces containing the imported and defined types.
    pub namespaces: LenVecBytes<Namespace>,
    pub type_signatures: LenVecBytes<type_system::AnyType>,
    pub method_signatures: LenVecBytes<MethodSignature>,
    /// An array containing the method bodies of the module.
    pub method_bodies: LenVecBytes<Code>,
    pub data_arrays: LenVecBytes<DataArray>,
    pub imports: LenBytes<ModuleImports>,
    pub definitions: LenBytes<ModuleDefinitions>,
    /// An optional index specifying the entry point method of the application. It is up to additional constraints made by the
    /// compiler or runtime to determine if the signature of the entry point method is valid.
    pub entry_point: LenBytes<Option<indices::MethodDefinition>>,
    pub type_layouts: LenVecBytes<TypeLayout>,
    //pub debugging_information: LenBytes<Option<>>
}

impl Module {
    /// Variable-length unsigned integer following the format version indicating the number of length encoded things to follow.
    pub fn data_count(&self) -> numeric::UInteger {
        MAX_MODULE_DATA_COUNT
    }
}

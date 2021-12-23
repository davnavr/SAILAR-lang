use bitflags::bitflags;

/// Contains types representing indices to various structures within the module file.
pub mod indices;
/// Contains type representing the integer types used within the module file.
///
/// Unless specified otherwise, all unsigned and signed integers are in little-endian order.
pub mod numeric;
/// Contains representations of common structures found within the module file.
pub mod structures;
/// Contains types representing the type system.
pub mod type_system;

/// The magic number for `binmdl` files.
pub static MAGIC: &[u8] = "binmdl\0".as_bytes();

/// A length-encoded array of variable-length unsigned integers used to indicate a version.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct VersionNumbers(pub structures::LengthEncodedVector<numeric::UInteger>);

/// Represents a length-encoded UTF-8 string that cannot be empty.
#[derive(Clone, Debug, Default, Eq, std::hash::Hash, PartialEq, PartialOrd)]
pub struct Identifier(String);

/// Describes the return types and parameter types of a method.
#[derive(Debug, Default, Eq, Hash, PartialEq, PartialOrd)]
pub struct MethodSignature {
    /// The types of the values returned by the method.
    pub return_types: structures::LengthEncodedVector<indices::TypeSignature>,
    pub parameter_types: structures::LengthEncodedVector<indices::TypeSignature>,
}

pub mod instruction_set;

#[derive(Debug)]
pub struct CodeExceptionHandler {
    /// Indicates the block that control will transfer to when an exception is thrown, relative to the current block.
    pub catch_block: instruction_set::BlockOffset,
    /// Specifies the input register of the `[catch_block]` that the exception object is stored into when an exception is thrown.
    /// If omitted, the exception object is ignored.
    pub exception_register: Option<indices::InputRegister>,
}

bitflags! {
    #[repr(transparent)]
    pub struct CodeBlockFlags: u8 {
        const NO_EXCEPTION_HANDLING = 0;
        const EXCEPTION_HANDLER_IGNORES_EXCEPTION = 0b0000_0001;
        const EXCEPTION_HANDLER_STORES_EXCEPTION = 0b0000_0010;
    }
}

#[derive(Debug)]
pub struct CodeBlock {
    //pub flags: (),
    /// A variable-length integer preceding the flags indicating the number of input registers for this block.
    ///
    /// For the entry block's count, this should match the number of arguments of the method.
    pub input_register_count: numeric::UInteger,
    /// Specifies the block that control should be transferred to if an exception is thrown inside this block.
    pub exception_handler: Option<CodeExceptionHandler>,
    /// The instructions of the block.
    ///
    /// Both the byte length and the actual number of instructions are included to simplify parsing.
    pub instructions: structures::DoubleLengthEncodedVector<instruction_set::Instruction>,
}

impl CodeBlock {
    /// Byte at the beginning of the block describing how it handles exceptions.
    pub fn flags(&self) -> CodeBlockFlags {
        match self.exception_handler {
            None => CodeBlockFlags::NO_EXCEPTION_HANDLING,
            Some(CodeExceptionHandler {
                exception_register: Some(_),
                ..
            }) => CodeBlockFlags::EXCEPTION_HANDLER_IGNORES_EXCEPTION,
            Some(CodeExceptionHandler {
                exception_register: None,
                ..
            }) => CodeBlockFlags::EXCEPTION_HANDLER_STORES_EXCEPTION,
        }
    }
}

#[derive(Debug)]
pub struct Code {
    /// The block that will be executed when the method is called, corresponds to block index `0`.
    pub entry_block: CodeBlock,
    pub blocks: structures::LengthEncodedVector<CodeBlock>,
}

#[derive(Clone, Debug, Default)]
pub struct DataArray(pub structures::LengthEncodedVector<u8>);

/// Indicates whether or not a type, field, or method can be imported by another module.
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum Visibility {
    /// Compiler decides whether or not it can be used.
    Unspecified = 0,
    /// Can be used as an import by another module.
    Public = 1,
    /// Can only be used within the current module.
    Private = 2,
}

impl Default for Visibility {
    fn default() -> Self {
        Visibility::Unspecified
    }
}

bitflags! {
    #[repr(transparent)]
    pub struct FieldFlags: u8 {
        const READ_ONLY = 0;
        const MUTABLE = 0b0000_0001;
        const STATIC = 0b0000_0010;
        const VALID_MASK = 0b0000_0011;
    }
}

bitflags! {
    #[derive(Default)]
    #[repr(transparent)]
    pub struct MethodFlags: u8 {
        const FINAL = 0;
        const INSTANCE = 0b0000_0001;
        const CONSTRUCTOR_OR_INITIALIZER = 0b0000_0010;
        const CONSTRUCTOR = Self::CONSTRUCTOR_OR_INITIALIZER.bits | Self::INSTANCE.bits;
        const INITIALIZER = Self::CONSTRUCTOR_OR_INITIALIZER.bits;
        const VIRTUAL = 0b0000_0100;
    }
}

bitflags! {
    #[derive(Default)]
    #[repr(transparent)]
    pub struct TypeDefinitionFlags: u8 {
        const FINAL = 0;
        /// The type can be inherited from.
        const NOT_FINAL = 0b0000_0001;
        /// Instances of this type cannot be created.
        const ABSTRACT = 0b0000_0010;
    }
}

macro_rules! flags_helpers {
    ($name: ident) => {
        impl $name {
            pub fn is_valid(self) -> bool {
                $name::all().contains(self)
            }
        }
    };
}

flags_helpers!(FieldFlags);
flags_helpers!(MethodFlags);
flags_helpers!(TypeDefinitionFlags);

#[derive(Debug)]
pub struct TypeDefinitionImport {
    /// Indicates the module that the type was imported from.
    pub module: indices::Module,
    pub name: indices::Identifier,
    pub namespace: indices::Namespace,
    //pub type_parameters: (),
}

#[derive(Debug)]
pub struct FieldImport {
    pub owner: indices::TypeDefinition,
    pub name: indices::Identifier,
    //pub flags: FieldFlags,
    pub signature: indices::TypeSignature,
}

#[derive(Debug)]
pub struct MethodImport {
    pub owner: indices::TypeDefinition,
    pub name: indices::Identifier, // TODO: How to handle importing constructors, use flags?
    //pub flags: MethodFlags,
    pub signature: indices::MethodSignature,
    //pub type_parameters: (),
}

/// Contains the types, fields, and methods imported by a module.
#[derive(Debug)]
pub struct ModuleImports {
    pub imported_modules: structures::LengthEncodedVector<ModuleIdentifier>, // TODO: Could also add byte length for module imports.
    pub imported_types: structures::DoubleLengthEncodedVector<TypeDefinitionImport>,
    pub imported_fields: structures::DoubleLengthEncodedVector<FieldImport>,
    pub imported_methods: structures::DoubleLengthEncodedVector<MethodImport>,
}

#[derive(Debug)] // TODO: Custom equality comparison to prevent overriding of method twice?
pub struct MethodOverride {
    /// Specifies the method to override.
    pub declaration: indices::Method,
    /// Specifies the new implementation of the method, the method must be defined in the current type.
    pub implementation: indices::Method, // TODO: Could optimize implementation index by just having 0 be current type's first method since the method vector is just before the vtable field.
}

#[derive(Debug)]
pub struct TypeDefinition {
    pub name: indices::Identifier,
    pub namespace: indices::Namespace,
    pub visibility: Visibility,
    pub flags: TypeDefinitionFlags,
    pub layout: indices::TypeLayout,
    pub inherited_types: structures::LengthEncodedVector<indices::TypeDefinition>,
    pub fields: structures::LengthEncodedVector<indices::Field>,
    pub methods: structures::LengthEncodedVector<indices::Method>,
    pub vtable: structures::LengthEncodedVector<MethodOverride>,
    //pub annotations: LengthEncodedVector<>,
    //pub type_parameters: (),
}

#[derive(Debug)]
pub struct Field {
    pub owner: indices::TypeDefinition,
    pub name: indices::Identifier,
    pub visibility: Visibility,
    pub flags: FieldFlags,
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

bitflags! {
    #[repr(transparent)]
    pub struct MethodImplementationFlags: u8 {
        const DEFINED = 0;
        /// The method body is not defined.
        const NONE = 0b0000_0001;
        const EXTERNAL = 0b0000_0010;
    }
}

flags_helpers!(MethodImplementationFlags);

impl MethodBody {
    pub fn flags(&self) -> MethodImplementationFlags {
        match self {
            Self::Defined(_) => MethodImplementationFlags::DEFINED,
            Self::Abstract => MethodImplementationFlags::NONE,
            Self::External { .. } => MethodImplementationFlags::EXTERNAL,
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
    pub visibility: Visibility,
    pub flags: MethodFlags,
    pub signature: indices::MethodSignature,
    /// The method body, in the binary format, this is where the structure for external methods would go.
    pub body: MethodBody,
    //pub annotations: LengthEncodedVector<>,
    //pub type_parameters: (),
}

impl Method {
    /// Flags that describe how the method is implemented, placed after the [`Method::flags`] field.
    pub fn implementation_flags(&self) -> MethodImplementationFlags {
        self.body.flags()
    }
}

/// Contains the types, fields, and methods defined in the module.
///
/// Each type contains a list indices refering to the fields and methods that it defines, and each field or method contains the
/// index of the type that defines it. These indices must exactly match in order for the module to be valid.
#[derive(Debug)]
pub struct ModuleDefinitions {
    pub defined_types: structures::DoubleLengthEncodedVector<TypeDefinition>,
    pub defined_fields: structures::DoubleLengthEncodedVector<Field>,
    pub defined_methods: structures::DoubleLengthEncodedVector<Method>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum TypeLayoutFlags {
    /// The runtime or compiler is free to decide how the fields of the type are laid out.
    Unspecified = 0,
    /// The fields of the type are laid out sequentially, and the size of the type is calculated automatically.
    Sequential = 1,
    /// The size and offset of fields is specified manually.
    ExplicitOffsets = 2,
    /// The fields of the type are laid out sequentially, but the size of the type is specified manually.
    ExplicitSize = 3,
}

#[derive(Debug)]
pub struct FieldOffset {
    pub field: indices::Field,
    pub offset: numeric::UInteger,
}

/// Describes how the fields are a type are layout, starting with a flag byte indicating whether or not the type has an explicit layout.
#[derive(Debug)]
pub enum TypeDefinitionLayout {
    Unspecified,
    Sequential(Option<numeric::UInteger>),
    Explicit {
        size: numeric::UInteger,
        field_offsets: Vec<FieldOffset>, // TODO: Use a hash map for field offsets?
    },
}

impl TypeDefinitionLayout {
    /// Flags at the beginning of the structure indicating the kind of layout used by a type's instances.
    pub fn flags(&self) -> TypeLayoutFlags {
        match self {
            Self::Unspecified => TypeLayoutFlags::Unspecified,
            Self::Sequential(None) => TypeLayoutFlags::Sequential,
            Self::Explicit { .. } => TypeLayoutFlags::ExplicitOffsets,
            Self::Sequential(Some(_)) => TypeLayoutFlags::ExplicitSize,
        }
    }
}

/// Specifies what version of the module format is being used, placed after the module's integer size field.
#[derive(Debug, Default, Eq, PartialEq, PartialOrd)]
pub struct FormatVersion {
    pub major: numeric::UInteger,
    pub minor: numeric::UInteger,
}

#[derive(Debug)]
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

pub type Namespace = structures::LengthEncodedVector<indices::Identifier>;

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
    pub header: structures::ByteLengthEncoded<ModuleHeader>,
    /// An array containing the names of the types, namespaces, fields, and methods.
    pub identifiers: structures::DoubleLengthEncodedVector<Identifier>,
    /// An array of the namespaces containing the imported and defined types.
    pub namespaces: structures::DoubleLengthEncodedVector<Namespace>,
    pub type_signatures: structures::DoubleLengthEncodedVector<type_system::AnyType>,
    pub method_signatures: structures::DoubleLengthEncodedVector<MethodSignature>,
    /// An array containing the method bodies of the module.
    pub method_bodies: structures::DoubleLengthEncodedVector<Code>,
    pub data_arrays: structures::DoubleLengthEncodedVector<DataArray>,
    pub imports: structures::ByteLengthEncoded<ModuleImports>,
    pub definitions: structures::ByteLengthEncoded<ModuleDefinitions>,
    /// An optional index specifying the entry point method of the application. It is up to additional constraints made by the
    /// compiler or runtime to determine if the signature of the entry point method is valid.
    pub entry_point: structures::ByteLengthEncoded<Option<indices::Method>>,
    pub type_layouts: structures::DoubleLengthEncodedVector<TypeDefinitionLayout>,
    //pub debugging_information: ByteLengthEncoded<>
}

impl Identifier {
    pub fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl TryFrom<&Vec<char>> for Identifier {
    type Error = ();

    fn try_from(chars: &Vec<char>) -> Result<Self, Self::Error> {
        if chars.is_empty() {
            Err(())
        } else {
            Ok(Self(chars.iter().collect::<String>()))
        }
    }
}

impl TryFrom<&str> for Identifier {
    type Error = ();

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        if s.is_empty() {
            Err(())
        } else {
            Ok(Self(String::from(s)))
        }
    }
}

impl<T: Into<numeric::UInteger>> std::iter::FromIterator<T> for VersionNumbers {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut numbers = Vec::new();
        for i in iter {
            numbers.push(i.into())
        }
        Self(structures::LengthEncodedVector(numbers))
    }
}

impl Module {
    /// Variable-length unsigned integer following the format version indicating the number of length encoded things to follow.
    pub fn data_count(&self) -> numeric::UInteger {
        MAX_MODULE_DATA_COUNT
    }
}

/// The magic number for `binmdl` files.
pub static MAGIC: &'static [u8] = "reg\0".as_bytes();

/// Represents a variable-length unsigned integer.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, PartialOrd)]
#[allow(non_camel_case_types)]
pub struct uvarint(pub u64);

/// Represents a variable-length signed integer.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, PartialOrd)]
#[allow(non_camel_case_types)]
pub struct varint(pub u128);

pub(crate) trait Index: Copy {
    fn index(self) -> uvarint;
}

macro_rules! index_type {
    ($name: ident, $description: literal) => {
        #[derive(Clone, Copy, Debug, Default, Eq, PartialEq, PartialOrd)]
        #[doc = $description]
        pub struct $name(pub uvarint);

        impl Index for $name {
            fn index(self) -> uvarint {
                let $name(index) = self;
                index
            }
        }
    };
}

index_type!(IdentifierIndex, "An index into the module's identifiers, the index of the first identifier is `0`.");
index_type!(NamespaceIndex, "An index into the namespaces of the types defined in this module, starting at `0`.");
index_type!(TypeSignatureIndex, "An index into the module's type signatures, starting at `0`.");
index_type!(MethodSignatureIndex, "An index into the module's method signatures, starting at `0`.");
index_type!(CodeIndex, "An index into the module's method bodies, starting at `0`.");
index_type!(DataIndex, "An index into the module's data arrays, starting at `0`.");
index_type!(ModuleIndex, "`0` refers to the current module, while the remaining indices refer to the module imports.");
index_type!(TypeDefinitionIndex, "An index into the module's imported types then defined types, with the index of the first defined type equal to the number of imported types.");
index_type!(FieldIndex, "An index into the module's field imports then defined fields, with the index of the first field definition equal to the number of imported fields.");
index_type!(MethodIndex, "An index into the module's method imports then defined methods, with the index of the first method definition equal to the number of imported methods.");

/// Represents data that is preceded by a variable-length unsigned integer indicating the byte length of the following data.
#[derive(Debug, Default, Eq, PartialEq, PartialOrd)]
pub struct ByteLengthEncoded<T>(pub T);

/// Represents an array preceded by an variable-length unsigned integer indicating the number of items.
#[derive(Debug, Default, Eq, PartialEq, PartialOrd)]
pub struct LengthEncodedVector<T>(pub Vec<T>);

/// A length-encoded array of variable-length unsigned integers used to indicate a version.
#[derive(Debug, Default)]
pub struct VersionNumbers(pub LengthEncodedVector<uvarint>);

/// Represents a length-encoded UTF-8 string that cannot be empty.
#[derive(Debug, Default, Eq, PartialEq, PartialOrd)]
pub struct Identifier(LengthEncodedVector<u8>);

#[derive(Debug, Eq, PartialEq, PartialOrd)]
pub enum PrimitiveType {
    U8 = 0,
    S8,
    U16,
    S16,
    U32,
    S32,
    U64,
    S64,
    UNative,
    SNative,
    F32,
    F64
}

/// A value type or native pointer type.
#[derive(Debug, Eq, PartialEq, PartialOrd)]
pub enum SimpleType {
    Primitive(PrimitiveType),
    Defined(TypeDefinitionIndex),
    NativePointer(Box<SimpleType>),
}

/// Union of all types in the type system that do not represent pointers to the stack that are tracked by the garbage collector.
#[derive(Debug, Eq, PartialEq, PartialOrd)]
pub enum HeapType {
    /// An untyped object reference, similar to `System.Object` or `java.lang.Object`.
    AnyRef,
    Val(SimpleType),
    /// An object reference to an instance of a class or a boxed primitive type.
    ObjRef(SimpleType),
    /// An object reference to an array whose elements are of the specified type.
    ArrayRef(Box<HeapType>),
    /// A pointer to a field or array element that is tracked by the garbage collector.
    HeapPointer(Box<HeapType>),
}

/// Represents the type of a parameter or a method return type.
#[derive(Debug, Eq, PartialEq, PartialOrd)]
pub enum AnyType {
    Val(SimpleType),
    /// A pointer to a field, array element, or the stack that is tracked by the garbage collector.
    GargbageCollectedPointer(Box<AnyType>),
    /// An object reference or a pointer to a field or array element that is tracked by the garbage collector.
    Heap(HeapType),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum TypeTag {
    /// The type of things that have no value, currently unused.
    Unit = 0,
    U8 = 1,
    S8 = 2,
    U16 = 3,
    S16 = 4,
    U32 = 5,
    S32 = 6,
    U64 = 7,
    S64 = 8,
    UNative = 9,
    SNative = 0x0A,
    RefAny = 0xB0,
    /// An object reference to an array containing elements of the following type.
    RefArray = 0xBA,
    /// An object reference to a boxed instance of the following primitive or native pointer type.
    RefBoxed = 0xBB,
    /// An object reference to a type specified by a type definition index
    RefDefined = 0xBE,
    /// A native pointer type containing instances of the following type.
    NativePointer = 0xCC,
    /// A pointer type containing instances of the following type on the heap.
    HeapPointer = 0xCB,
    /// A pointer type containing instances of the following type on the heap or stack.
    GarbageCollectedPointer = 0xCE,
    /// A type specified by a type definition index passed by value.
    Struct = 0xDE,
    F32 = 0xF4,
    F64 = 0xF8
}

pub(crate) trait TypeTagged {
    fn tag(&self) -> TypeTag;
}

impl TypeTagged for PrimitiveType {
    fn tag(&self) -> TypeTag {
        match self {
            PrimitiveType::U8 => TypeTag::U8,
            PrimitiveType::S8 => TypeTag::S8,
            PrimitiveType::U16 => TypeTag::U16,
            PrimitiveType::S16 => TypeTag::S16,
            PrimitiveType::U32 => TypeTag::U32,
            PrimitiveType::S32 => TypeTag::S32,
            PrimitiveType::U64 => TypeTag::U64,
            PrimitiveType::S64 => TypeTag::S64,
            PrimitiveType::UNative => TypeTag::UNative,
            PrimitiveType::SNative => TypeTag::SNative,
            PrimitiveType::F32 => TypeTag::F32,
            PrimitiveType::F64 => TypeTag::F64,
        }
    }
}

impl TypeTagged for SimpleType {
    fn tag(&self) -> TypeTag {
        match self {
            SimpleType::Primitive(primitive_type) => primitive_type.tag(),
            SimpleType::Defined(_) => TypeTag::Struct,
            SimpleType::NativePointer(_) => TypeTag::NativePointer,
        }
    }
}

impl TypeTagged for HeapType {
    fn tag(&self) -> TypeTag {
        match self {
            HeapType::Val(value_type) => value_type.tag(),
            HeapType::ObjRef(SimpleType::Defined(_)) => TypeTag::RefDefined,
            HeapType::ArrayRef(_) => TypeTag::RefArray,
            HeapType::ObjRef(_) => TypeTag::RefBoxed,
            HeapType::AnyRef => TypeTag::RefAny,
            HeapType::HeapPointer(_) => TypeTag::HeapPointer,
        }
    }
}

/// Describes the return types and parameter types of a method.
#[derive(Debug, Eq, PartialEq, PartialOrd)]
pub struct MethodSignature {
    /// The types of the values returned by the method.
    pub return_types: LengthEncodedVector<TypeSignatureIndex>,
    pub parameter_types: LengthEncodedVector<TypeSignatureIndex>
}

pub mod instruction_set;

index_type!(CodeBlockIndex, "An index corresponding to the input registers of a code block");
index_type!(InputRegisterIndex, "An index corresponding to the input registers of a code block");
index_type!(TemporaryRegisterIndex, "An index corresponding to the temporary registers of a code block");

#[derive(Debug)]
pub struct CodeExceptionHandler {
    /// Indicates the block that control will transfer to when an exception is thrown, relative to the current block.
    pub catch_block: instruction_set::BlockOffset,
    /// Specifies the input register of the `[catch_block]` that the exception object is stored into when an exception is thrown.
    /// If omitted, the exception object is ignored.
    pub exception_register: Option<InputRegisterIndex>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum CodeBlockFlags {
    NoExceptionHandling = 0,
    ExceptionHandlerIgnoresException = 1,
    ExceptionHandlerStoresException = 2,
}

#[derive(Debug)]
pub struct CodeBlock {
    //pub flags: (),
    /// A variable-length integer preceding the flags indicating the number of input registers for this block.
    /// 
    /// For the entry block's count, this should match the number of arguments of the method.
    pub input_register_count: uvarint,
    /// Specifies the block that control should be transferred to if an exception is thrown inside this block.
    pub exception_handler: Option<CodeExceptionHandler>,
    /// The instructions of the block.
    /// 
    /// Both the byte length and the actual number of instructions are included to simplify parsing.
    pub instructions: ByteLengthEncoded<LengthEncodedVector<instruction_set::Instruction>>,
}

impl CodeBlock {
    /// Byte at the beginning of the block describing how it handles exceptions.
    pub fn flags(&self) -> CodeBlockFlags {
        match self.exception_handler {
            None => CodeBlockFlags::NoExceptionHandling,
            Some(CodeExceptionHandler { exception_register: Some(_), .. }) => CodeBlockFlags::ExceptionHandlerIgnoresException,
            Some(CodeExceptionHandler { exception_register: None, .. }) => CodeBlockFlags::ExceptionHandlerStoresException,
        }
    }
}

#[derive(Debug)]
pub struct Code {
    /// The block that will be executed when the method is called, corresponds to block index `0`.
    pub entry_block: CodeBlock,
    pub blocks: LengthEncodedVector<CodeBlock>
}

#[derive(Clone, Debug, Default)]
pub struct DataArray(pub Vec<u8>);

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum MethodFlags {
    Instance = 0b0000_0001,
    ConstructorOrInitializer = 0b0000_0010,
    Constructor = 0b0000_0011,
    //Initializer = 0b0000_0010,
}

#[derive(Debug)]
pub struct TypeDefinitionImport {

}

#[derive(Debug)]
pub struct FieldImport {

}

#[derive(Debug)]
pub struct MethodImport {

}

/// Contains the types, fields, and methods imported by a module.
#[derive(Debug)]
pub struct ModuleImports {
    pub imported_modules: LengthEncodedVector<ModuleIdentifier>,
    pub imported_types: ByteLengthEncoded<LengthEncodedVector<TypeDefinitionImport>>,
    pub imported_fields: ByteLengthEncoded<LengthEncodedVector<FieldImport>>,
    pub imported_methods: ByteLengthEncoded<LengthEncodedVector<MethodImport>>,
}

#[derive(Debug)]
pub struct TypeDefinition {

}

#[derive(Debug)]
pub struct Field {

}

#[derive(Debug)]
pub struct Method {

}

/// Contains the types, fields, and methods defined in the module.
/// 
/// Each type contains a list indices refering to the fields and methods that it defines, and each field or method contains the
/// index of the type that defines it. These indices must exactly match in order for the module to be valid.
#[derive(Debug)]
pub struct ModuleDefinitions {
    pub defined_types: ByteLengthEncoded<LengthEncodedVector<TypeDefinition>>,
    pub defined_fields: ByteLengthEncoded<LengthEncodedVector<Field>>,
    pub defined_methods: ByteLengthEncoded<LengthEncodedVector<Method>>,
}

/// Describes the features that a module makes use of.
#[derive(Debug, Default, Eq, PartialEq, PartialOrd)]
pub struct FormatVersion {
    pub major: uvarint,
    pub minor: uvarint,
}

#[derive(Debug)]
pub struct ModuleIdentifier {
    pub name: Identifier,
    pub version: VersionNumbers,
}

pub static MAX_HEADER_FIELD_COUNT: uvarint = uvarint(1);

#[derive(Debug)]
pub struct ModuleHeader {
    //pub field_count: (),
    pub identifier: ModuleIdentifier,
}

impl ModuleHeader {
    /// Variable-length unsigned integer placed at the start of the header indicating the number of fields present.
    pub fn field_count(&self) -> uvarint { MAX_HEADER_FIELD_COUNT }
}

pub static MIN_MODULE_DATA_COUNT: uvarint = uvarint(1);
pub static MAX_MODULE_DATA_COUNT: uvarint = uvarint(9);

/// Represents the contents of a `binmdl` file following the [`MAGIC`] number.
#[derive(Debug)]
pub struct Module {
    pub format_version: FormatVersion,
    //pub data_count: (),
    /// The header, which identifies and describes the module.
    pub header: ByteLengthEncoded<ModuleHeader>,
    /// An array containing the names of the types, namespaces, fields, and methods.
    pub identifiers: ByteLengthEncoded<LengthEncodedVector<Identifier>>,
    /// An array of the namespaces containing the imported and defined types.
    pub namespaces: ByteLengthEncoded<LengthEncodedVector<LengthEncodedVector<IdentifierIndex>>>,
    pub type_signatures: ByteLengthEncoded<LengthEncodedVector<AnyType>>,
    pub method_signatures: ByteLengthEncoded<LengthEncodedVector<MethodSignature>>,
    /// An array containing the method bodies of the module.
    pub method_bodies: ByteLengthEncoded<LengthEncodedVector<Code>>,
    pub data_arrays: ByteLengthEncoded<DataArray>,
    pub imports: ByteLengthEncoded<ModuleImports>,
    pub definitions: ByteLengthEncoded<ModuleDefinitions>,
}

impl Identifier {
    pub fn with_bytes(bytes: Vec<u8>) -> Identifier {
        Identifier(LengthEncodedVector(bytes))
    }

    pub fn bytes(&self) -> &Vec<u8> {
        let Identifier(LengthEncodedVector(bytes)) = self;
        bytes
    }
}

impl<T> ByteLengthEncoded<T> {
    pub fn data(&self) -> &T {
        let ByteLengthEncoded(data) = self;
        data
    }
}

impl<T> LengthEncodedVector<T> {
    pub fn len(&self) -> usize {
        let LengthEncodedVector(items) = self;
        items.len()
    }
}

impl Module {
    /// Variable-length unsigned integer following the format version indicating the number of length encoded things to follow.
    pub fn data_count(&self) -> uvarint { MAX_MODULE_DATA_COUNT }
}

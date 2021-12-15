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
pub static MAX_MODULE_DATA_COUNT: uvarint = uvarint(5);

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
    pub method_signatures: ByteLengthEncoded<LengthEncodedVector<MethodSignature>>
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

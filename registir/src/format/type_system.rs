use crate::format::indices;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
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
    F64,
}

/// A value type or native pointer type.
#[derive(Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum SimpleType {
    Primitive(PrimitiveType),
    Defined(indices::Type),
    NativePointer(Box<SimpleType>),
}

/// Union of all types in the type system that do not represent pointers to the stack that are tracked by the garbage collector.
#[derive(Debug, Eq, Hash, PartialEq, PartialOrd)]
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
#[derive(Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum AnyType {
    /// A pointer to a field, array element, or the stack that is tracked by the garbage collector.
    GargbageCollectedPointer(Box<AnyType>),
    /// An value type, native pointer type, object reference, or a pointer to a field or array element that is tracked by the
    /// garbage collector.
    Heap(HeapType),
}

impl AnyType {
    pub fn primitive(primitive_type: PrimitiveType) -> Self {
        Self::Heap(HeapType::Val(SimpleType::Primitive(primitive_type)))
    }
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
    F64 = 0xF8,
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

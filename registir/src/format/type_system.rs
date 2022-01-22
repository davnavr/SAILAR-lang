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

impl std::fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(match self {
            Self::U8 => "u8",
            Self::S8 => "s8",
            Self::U16 => "u16",
            Self::S16 => "s16",
            Self::U32 => "u32",
            Self::S32 => "s32",
            Self::U64 => "u64",
            Self::S64 => "s64",
            Self::UNative => "unative",
            Self::SNative => "snative",
            Self::F32 => "F32",
            Self::F64 => "F64",
        })
    }
}

/// Represents the type of a parameter or a method return type.
#[derive(Debug, Eq, Hash, PartialEq, PartialOrd)]
pub enum AnyType {
    Primitive(PrimitiveType),
    Struct(indices::Struct),
    NativePointer(Box<AnyType>),
    //FixedArray { element_type: Box<AnyType>, length: IntegerConstant },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum TypeTag {
    /// The type of things that have no value, currently unused.
    Unit = 0,
    U8 = 1,
    U16 = 2,
    U32 = 4,
    U64 = 8,
    UNative = 0xA,
    S8 = 0x11,
    S16 = 0x12,
    S32 = 0x14,
    S64 = 0x18,
    SNative = 0x1A,
    /// A native pointer type pointing to instances of the following type.
    NativePointer = 0xCC,
    /// A type specified by a type definition index passed by value.
    Struct = 0xDE,
    F32 = 0xF4,
    F64 = 0xF8,
    //FixedArray = 0xFA,
}

impl PrimitiveType {
    pub fn tag(self) -> TypeTag {
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

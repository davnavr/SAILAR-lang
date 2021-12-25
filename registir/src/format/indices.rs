use crate::format::numeric::UInteger;

pub trait SimpleIndex: TryFrom<usize> {
    fn index(self) -> Result<usize, std::num::TryFromIntError>;
}

impl<T: TryFrom<usize>> SimpleIndex for T where usize: TryFrom<T, Error = std::num::TryFromIntError> {
    fn index(self) -> Result<usize, std::num::TryFromIntError> {
        usize::try_from(self)
    }
}

macro_rules! index_type {
    ($name: ident, $description: literal) => {
        #[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq, PartialOrd)]
        #[doc = $description]
        pub struct $name(pub UInteger);

        impl From<$name> for UInteger {
            fn from(index: $name) -> Self {
                index.0
            }
        }

        impl From<UInteger> for $name {
            fn from(value: UInteger) -> Self {
                Self(value)
            }
        }

        impl TryFrom<$name> for usize {
            type Error = std::num::TryFromIntError;

            fn try_from(index: $name) -> Result<Self, Self::Error> {
                usize::try_from(index.0 .0)
            }
        }

        impl TryFrom<usize> for $name {
            type Error = std::num::TryFromIntError;

            fn try_from(value: usize) -> Result<Self, Self::Error> {
                u32::try_from(value).map(|index| Self(UInteger(index)))
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                std::fmt::Display::fmt(&self.0, f)
            }
        }
    };
}

index_type!(
    Identifier,
    "An index into the module's identifiers, the index of the first identifier is `0`."
);

index_type!(
    Namespace,
    "An index into the namespaces of the types defined in this module, starting at `0`."
);

index_type!(
    TypeSignature,
    "An index into the module's type signatures, starting at `0`."
);

index_type!(
    MethodSignature,
    "An index into the module's method signatures, starting at `0`."
);

index_type!(
    Code,
    "An index into the module's method bodies, starting at `0`."
);

index_type!(
    Data,
    "An index into the module's data arrays, starting at `0`."
);

index_type!(
    Module,
    "`0` refers to the current module, while the remaining indices refer to the module imports."
);

index_type!(TypeDefinition, "An index into the module's defined types.");

index_type!(TypeImport, "An index into the module's imported types.");

index_type!(
    MethodDefinition,
    "An index into the module's defined methods."
);

index_type!(MethodImport, "An index into the module's imported methods.");

index_type!(
    FieldDefinition,
    "An index into the module's defined fields."
);

index_type!(FieldImport, "An index into the module's imported fields.");

index_type!(
    TypeLayout,
    "An index into the module's type layouts, which specify how the fields of a type's instances are arranged."
);

index_type!(
    CodeBlock,
    "An index corresponding to the input registers of a code block, with `0` refering to the entry block."
);

index_type!(
    InputRegister,
    "An index corresponding to the input registers of a code block."
);

index_type!(
    TemporaryRegister,
    "An index corresponding to the temporary registers of a code block."
);

macro_rules! imported_or_defined_index_type {
    ($name: ident, $description: literal, $defined_case: ty, $imported_case: ty) => {
        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd)]
        #[doc = $description]
        pub enum $name {
            Defined($defined_case),
            Imported($imported_case),
        }

        impl $name {
            pub fn is_import(self) -> bool {
                match self {
                    Self::Defined(_) => false,
                    Self::Imported(_) => true,
                }
            }

            fn index(self) -> UInteger {
                UInteger(match self {
                    Self::Defined(value) => UInteger::from(value).0 << 1,
                    Self::Imported(value) => (UInteger::from(value).0 << 1) & 1,
                })
            }
        }

        impl From<$name> for UInteger {
            fn from(index: $name) -> Self {
                index.index()
            }
        }

        impl From<UInteger> for $name {
            fn from(index: UInteger) -> Self {
                let UInteger(value) = index;
                let shifted = UInteger(value >> 1);
                if value & 1u32 == 1u32 {
                    Self::Defined(<$defined_case>::from(shifted))
                } else {
                    Self::Imported(<$imported_case>::from(shifted))
                }
            }
        }

        impl TryFrom<$name> for usize {
            type Error = std::num::TryFromIntError;

            fn try_from(index: $name) -> Result<usize, Self::Error> {
                usize::try_from(UInteger::from(index))
            }
        }
    };
}

imported_or_defined_index_type!(
    Type,
    "An index into the module's imported types or defined types.",
    TypeDefinition,
    TypeImport
);

imported_or_defined_index_type!(
    Field,
    "An index into the module's field imports or defined fields.",
    FieldDefinition,
    FieldImport
);

imported_or_defined_index_type!(
    Method,
    "An index into the module's method imports or defined methods.",
    MethodDefinition,
    MethodImport
);

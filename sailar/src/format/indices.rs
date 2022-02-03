use crate::format::numeric::UInteger;
use std::fmt::{Display, Formatter};

pub trait SimpleIndex: TryFrom<usize> {
    fn index(self) -> Result<usize, std::num::TryFromIntError>;
}

impl<T: TryFrom<usize>> SimpleIndex for T
where
    usize: TryFrom<T, Error = std::num::TryFromIntError>,
{
    fn index(self) -> Result<usize, std::num::TryFromIntError> {
        usize::try_from(self)
    }
}

macro_rules! index_type {
    ($name: ident, $description: literal) => {
        #[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
        #[doc = $description]
        pub struct $name(pub UInteger);

        impl From<u32> for $name {
            fn from(value: u32) -> Self {
                Self(UInteger(value))
            }
        }

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

        impl Display for $name {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                Display::fmt(&self.0, f)
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
    FunctionSignature,
    "An index into the module's method signatures, starting at `0`."
);

index_type!(
    Code,
    "An index into the module's method bodies, starting at `0`."
);

index_type!(
    CodeBlock,
    "An index into a method body's other blocks, with `0` refering to the entry block."
);

index_type!(
    Data,
    "An index into the module's data arrays, starting at `0`."
);

index_type!(
    Module,
    "`0` refers to the current module, while the remaining indices refer to the module imports."
);

index_type!(
    StructDefinition,
    "An index into the module's defined structs."
);

index_type!(StructImport, "An index into the module's imported structs.");

index_type!(
    FunctionDefinition,
    "An index into the module's defined functions."
);

index_type!(
    FunctionImport,
    "An index into the module's imported functions."
);

index_type!(
    FieldDefinition,
    "An index into the module's defined fields."
);

index_type!(FieldImport, "An index into the module's imported fields.");

index_type!(
    GlobalDefinition,
    "An index into the module's defined globals."
);

index_type!(GlobalImport, "An index into the module's imported globals.");

index_type!(
    StructLayout,
    "An index into the module's type layouts, which specify how the fields of a type's instances are arranged."
);

index_type!(
    InputRegister,
    "An index corresponding to the input registers of a code block."
);

index_type!(
    TemporaryRegister,
    "An index corresponding to the temporary registers of a code block."
);

// NOTE: Usage of bit to indicate what kind of index is being used may mean excessively large modules are not valid.
macro_rules! double_index_type {
    ($name: ident, $description: literal, $case_name_1: ident, $case_value_1: ty, $case_name_2: ident, $case_value_2: ty) => {
        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd)]
        #[doc = $description]
        pub enum $name {
            $case_name_1($case_value_1),
            $case_name_2($case_value_2),
        }

        impl $name {
            pub fn index(self) -> UInteger {
                UInteger(match self {
                    Self::$case_name_1(value) => UInteger::from(value).0 << 1,
                    Self::$case_name_2(value) => (UInteger::from(value).0 << 1) | 1,
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
                    Self::$case_name_2(<$case_value_2>::from(shifted))
                } else {
                    Self::$case_name_1(<$case_value_1>::from(shifted))
                }
            }
        }

        impl TryFrom<$name> for usize {
            type Error = std::num::TryFromIntError;

            fn try_from(index: $name) -> Result<usize, Self::Error> {
                usize::try_from(UInteger::from(index))
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
                let index = match self {
                    Self::$case_name_1(value) => {
                        f.write_str(stringify!($case_name_1))?;
                        UInteger::from(*value)
                    }
                    Self::$case_name_2(value) => {
                        f.write_str(stringify!($case_name_2))?;
                        UInteger::from(*value)
                    }
                };
                write!(f, " {}", index)
            }
        }
    };
}

double_index_type!(
    Register,
    "An index into a code block's input registers or temporary registers.",
    Temporary,
    TemporaryRegister,
    Input,
    InputRegister
);

macro_rules! imported_or_defined_index_type {
    ($name: ident, $description: literal, $defined_case: ty, $imported_case: ty) => {
        double_index_type!(
            $name,
            $description,
            Defined,
            $defined_case,
            Imported,
            $imported_case
        );

        impl $name {
            pub fn is_import(self) -> bool {
                match self {
                    Self::Defined(_) => false,
                    Self::Imported(_) => true,
                }
            }
        }
    };
}

imported_or_defined_index_type!(
    Struct,
    "An index into the module's imported structs or defined structs.",
    StructDefinition,
    StructImport
);

imported_or_defined_index_type!(
    Global,
    "An index into the module's global imports or defined globals.",
    GlobalDefinition,
    GlobalImport
);

imported_or_defined_index_type!(
    Field,
    "An index into the module's field imports or defined fields.",
    FieldDefinition,
    FieldImport
);

imported_or_defined_index_type!(
    Function,
    "An index into the module's function imports or defined functions.",
    FunctionDefinition,
    FunctionImport
);

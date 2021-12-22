use crate::format::numeric::UInteger;

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

        impl TryFrom<$name> for usize {
            type Error = std::num::TryFromIntError;

            fn try_from(index: $name) -> Result<Self, Self::Error> {
                usize::try_from(index.0.0)
            }
        }

        impl TryFrom<usize> for $name {
            type Error = std::num::TryFromIntError;

            fn try_from(value: usize) -> Result<Self, Self::Error> {
                u32::try_from(value).map(|index| Self(UInteger(index)))
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

// TODO: Make this an enum.
index_type!(
    TypeDefinition,
    "An index into the module's imported types then defined types, with the index of the first defined type equal to the number of imported types."
);

index_type!(
    Field,
    "An index into the module's field imports then defined fields, with the index of the first field definition equal to the number of imported fields."
);

index_type!(
    Method,
    "An index into the module's method imports then defined methods, with the index of the first method definition equal to the number of imported methods."
);

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

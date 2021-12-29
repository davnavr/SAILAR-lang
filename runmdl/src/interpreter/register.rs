use crate::interpreter;

pub use interpreter::{instruction_set::RegisterType, IntegerConstant, PrimitiveType};

#[derive(Clone, Copy)]
pub(crate) union RegisterValue {
    s_byte: i8,
    u_byte: u8,
    s_short: i16,
    u_short: u16,
    s_int: i32,
    u_int: u32,
    s_long: i64,
    u_long: u64,
    f_single: f32,
    f_double: f64,
    s_native: isize,
    u_native: usize,
}

impl Default for RegisterValue {
    fn default() -> Self {
        Self { u_long: 0 }
    }
}

pub struct Register {
    pub(crate) value: RegisterValue,
    value_type: interpreter::RegisterType,
}

impl Register {
    pub(crate) fn initialize(value_type: &interpreter::type_system::AnyType) -> Self {
        use interpreter::type_system::{AnyType, HeapType, SimpleType};

        Self {
            value: RegisterValue::default(),
            value_type: match value_type {
                AnyType::Heap(HeapType::Val(SimpleType::Primitive(primitive_type))) => {
                    RegisterType::Primitive(*primitive_type)
                }
                _ => todo!("Unsupported register type"),
            },
        }
    }

    pub(crate) fn initialize_many<
        'l,
        T: std::iter::IntoIterator<Item = &'l interpreter::type_system::AnyType>,
    >(
        types: T,
    ) -> Vec<Register> {
        types.into_iter().map(Self::initialize).collect()
    }

    pub(crate) fn copy_raw(source: &Self, destination: &mut Self) {
        destination.value = source.value;
    }

    /// Copies the register values from a `source` to a `destination`.
    ///
    /// # Panics
    ///
    /// Panics if the `source` and `destination` lengths are not equal.
    pub(crate) fn copy_many_raw(source: &[&Self], destination: &mut [Self]) {
        assert_eq!(source.len(), destination.len());
        for (index, register) in source.iter().enumerate() {
            Self::copy_raw(register, &mut destination[index]);
        }
    }
}

macro_rules! register_conversion_from {
    ($source_type: ty, $value_field: ident, $register_type: expr) => {
        impl From<$source_type> for Register {
            fn from(value: $source_type) -> Self {
                Self {
                    value: RegisterValue {
                        $value_field: value,
                    },
                    value_type: $register_type,
                }
            }
        }
    };
}

register_conversion_from!(i8, s_byte, RegisterType::Primitive(PrimitiveType::S8));
register_conversion_from!(u8, u_byte, RegisterType::Primitive(PrimitiveType::U8));
register_conversion_from!(i16, s_short, RegisterType::Primitive(PrimitiveType::S16));
register_conversion_from!(u16, u_short, RegisterType::Primitive(PrimitiveType::U16));
register_conversion_from!(i32, s_int, RegisterType::Primitive(PrimitiveType::S32));
register_conversion_from!(u32, u_int, RegisterType::Primitive(PrimitiveType::U32));
register_conversion_from!(i64, s_long, RegisterType::Primitive(PrimitiveType::S64));
register_conversion_from!(u64, u_long, RegisterType::Primitive(PrimitiveType::U64));
register_conversion_from!(
    isize,
    s_native,
    RegisterType::Primitive(PrimitiveType::SNative)
);
register_conversion_from!(
    usize,
    u_native,
    RegisterType::Primitive(PrimitiveType::UNative)
);
register_conversion_from!(f32, f_single, RegisterType::Primitive(PrimitiveType::F32));
register_conversion_from!(f64, f_double, RegisterType::Primitive(PrimitiveType::F64));

impl From<IntegerConstant> for Register {
    fn from(value: IntegerConstant) -> Self {
        match value {
            IntegerConstant::U8(value) => Self::from(value),
            IntegerConstant::S8(value) => Self::from(value),
            IntegerConstant::U16(value) => Self::from(value),
            IntegerConstant::S16(value) => Self::from(value),
            IntegerConstant::U32(value) => Self::from(value),
            IntegerConstant::S32(value) => Self::from(value),
            IntegerConstant::U64(value) => Self::from(value),
            IntegerConstant::S64(value) => Self::from(value),
        }
    }
}

/// The error type returned when a conversion from a register value fails.
#[derive(Debug, Clone)]
pub struct TryFromRegisterValueError {
    expected: RegisterType,
    actual: RegisterType,
}

impl std::fmt::Display for TryFromRegisterValueError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "expected register to contain a value of type {:?} but got {:?}",
            self.expected, self.actual
        )
    }
}

impl std::error::Error for TryFromRegisterValueError {}

macro_rules! register_conversion_to {
    ($destination_type: ty, $source_field: ident, $register_type: pat, $expected_type: expr) => {
        impl TryFrom<&Register> for $destination_type {
            type Error = TryFromRegisterValueError;

            fn try_from(source: &Register) -> std::result::Result<Self, Self::Error> {
                match source.value_type {
                    $register_type => unsafe { Ok(source.value.$source_field) },
                    actual => Err(Self::Error {
                        expected: $expected_type,
                        actual: actual,
                    }),
                }
            }
        }
    };
}

register_conversion_to!(
    i8,
    s_byte,
    RegisterType::Primitive(PrimitiveType::S8),
    RegisterType::Primitive(PrimitiveType::S8)
);
register_conversion_to!(
    u8,
    u_byte,
    RegisterType::Primitive(PrimitiveType::U8),
    RegisterType::Primitive(PrimitiveType::U8)
);
register_conversion_to!(
    i16,
    s_short,
    RegisterType::Primitive(PrimitiveType::S16),
    RegisterType::Primitive(PrimitiveType::S16)
);
register_conversion_to!(
    u16,
    u_short,
    RegisterType::Primitive(PrimitiveType::U16),
    RegisterType::Primitive(PrimitiveType::U16)
);
register_conversion_to!(
    i32,
    s_int,
    RegisterType::Primitive(PrimitiveType::S32),
    RegisterType::Primitive(PrimitiveType::S32)
);
register_conversion_to!(
    u32,
    u_int,
    RegisterType::Primitive(PrimitiveType::U32),
    RegisterType::Primitive(PrimitiveType::U32)
);
register_conversion_to!(
    i64,
    s_long,
    RegisterType::Primitive(PrimitiveType::S64),
    RegisterType::Primitive(PrimitiveType::S64)
);
register_conversion_to!(
    u64,
    u_long,
    RegisterType::Primitive(PrimitiveType::U64),
    RegisterType::Primitive(PrimitiveType::U64)
);
register_conversion_to!(
    isize,
    s_native,
    RegisterType::Primitive(PrimitiveType::SNative),
    RegisterType::Primitive(PrimitiveType::SNative)
);
register_conversion_to!(
    usize,
    u_native,
    RegisterType::Primitive(PrimitiveType::UNative),
    RegisterType::Primitive(PrimitiveType::UNative)
);
register_conversion_to!(
    f32,
    f_single,
    RegisterType::Primitive(PrimitiveType::F32),
    RegisterType::Primitive(PrimitiveType::F32)
);
register_conversion_to!(
    f64,
    f_double,
    RegisterType::Primitive(PrimitiveType::F64),
    RegisterType::Primitive(PrimitiveType::F64)
);

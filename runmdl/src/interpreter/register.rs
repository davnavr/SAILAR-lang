use crate::interpreter;

pub use interpreter::{instruction_set::RegisterType, IntegerConstant, PrimitiveType};

#[derive(Clone, Copy)]
pub union RegisterValue {
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

trait InterpretRegister {
    fn interpret_register(register: &Register) -> Self;
}

macro_rules! register_interpretation_to {
    ($destination_type: ty) => {
        impl InterpretRegister for $destination_type {
            fn interpret_register(register: &Register) -> Self {
                match register.value_type {
                    RegisterType::Primitive(PrimitiveType::U8) => unsafe {
                        register.value.u_byte as $destination_type
                    },
                    RegisterType::Primitive(PrimitiveType::S8) => unsafe {
                        register.value.s_byte as $destination_type
                    },
                    RegisterType::Primitive(PrimitiveType::U16) => unsafe {
                        register.value.u_short as $destination_type
                    },
                    RegisterType::Primitive(PrimitiveType::S16) => unsafe {
                        register.value.s_short as $destination_type
                    },
                    RegisterType::Primitive(PrimitiveType::U32) => unsafe {
                        register.value.u_int as $destination_type
                    },
                    RegisterType::Primitive(PrimitiveType::S32) => unsafe {
                        register.value.s_int as $destination_type
                    },
                    RegisterType::Primitive(PrimitiveType::U64) => unsafe {
                        register.value.u_long as $destination_type
                    },
                    RegisterType::Primitive(PrimitiveType::S64) => unsafe {
                        register.value.s_long as $destination_type
                    },
                    RegisterType::Primitive(PrimitiveType::UNative) => unsafe {
                        register.value.u_native as $destination_type
                    },
                    RegisterType::Primitive(PrimitiveType::SNative) => unsafe {
                        register.value.s_native as $destination_type
                    },
                    RegisterType::Primitive(PrimitiveType::F32) => unsafe {
                        register.value.f_single as $destination_type
                    },
                    RegisterType::Primitive(PrimitiveType::F64) => unsafe {
                        register.value.f_double as $destination_type
                    },
                }
            }
        }
    };
}

register_interpretation_to!(u8);
register_interpretation_to!(i8);
register_interpretation_to!(u16);
register_interpretation_to!(i16);
register_interpretation_to!(u32);
register_interpretation_to!(i32);
register_interpretation_to!(u64);
register_interpretation_to!(i64);
register_interpretation_to!(usize);
register_interpretation_to!(isize);
register_interpretation_to!(f32);
register_interpretation_to!(f64);

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

impl From<bool> for Register {
    fn from(value: bool) -> Self {
        Register::from(if value { 1u8 } else { 0 })
    }
}

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

macro_rules! register_conversion_to_integer {
    ($destination_type: ty, $source_field: ident, $register_type: pat, $expected_type: expr) => {
        impl TryFrom<&Register> for $destination_type {
            type Error = TryFromRegisterValueError;

            #[allow(unreachable_patterns)]
            fn try_from(source: &Register) -> std::result::Result<Self, Self::Error> {
                let error = || TryFromRegisterValueError {
                    expected: RegisterType::Primitive(PrimitiveType::S8),
                    actual: source.value_type,
                };

                match source.value_type {
                    $register_type => unsafe { Ok(source.value.$source_field) },
                    RegisterType::Primitive(PrimitiveType::U8) => {
                        <$destination_type>::try_from(unsafe { source.value.u_byte })
                            .map_err(|_| error())
                    }
                    RegisterType::Primitive(PrimitiveType::S8) => {
                        <$destination_type>::try_from(unsafe { source.value.s_byte })
                            .map_err(|_| error())
                    }
                    RegisterType::Primitive(PrimitiveType::U16) => {
                        <$destination_type>::try_from(unsafe { source.value.u_short })
                            .map_err(|_| error())
                    }
                    RegisterType::Primitive(PrimitiveType::S16) => {
                        <$destination_type>::try_from(unsafe { source.value.s_short })
                            .map_err(|_| error())
                    }
                    RegisterType::Primitive(PrimitiveType::U32) => {
                        <$destination_type>::try_from(unsafe { source.value.u_int })
                            .map_err(|_| error())
                    }
                    RegisterType::Primitive(PrimitiveType::S32) => {
                        <$destination_type>::try_from(unsafe { source.value.s_int })
                            .map_err(|_| error())
                    }
                    RegisterType::Primitive(PrimitiveType::U64) => {
                        <$destination_type>::try_from(unsafe { source.value.u_long })
                            .map_err(|_| error())
                    }
                    RegisterType::Primitive(PrimitiveType::S64) => {
                        <$destination_type>::try_from(unsafe { source.value.s_long })
                            .map_err(|_| error())
                    }
                    RegisterType::Primitive(PrimitiveType::UNative) => {
                        <$destination_type>::try_from(unsafe { source.value.u_native })
                            .map_err(|_| error())
                    }
                    RegisterType::Primitive(PrimitiveType::SNative) => {
                        <$destination_type>::try_from(unsafe { source.value.s_native })
                            .map_err(|_| error())
                    }
                    RegisterType::Primitive(PrimitiveType::F32 | PrimitiveType::F64) => todo!("Conversions from floating point register values to integers are not yet supported")
                }
            }
        }
    };
}

register_conversion_to_integer!(
    i8,
    s_byte,
    RegisterType::Primitive(PrimitiveType::S8),
    RegisterType::Primitive(PrimitiveType::S8)
);
register_conversion_to_integer!(
    u8,
    u_byte,
    RegisterType::Primitive(PrimitiveType::U8),
    RegisterType::Primitive(PrimitiveType::U8)
);
register_conversion_to_integer!(
    i16,
    s_short,
    RegisterType::Primitive(PrimitiveType::S16),
    RegisterType::Primitive(PrimitiveType::S16)
);
register_conversion_to_integer!(
    u16,
    u_short,
    RegisterType::Primitive(PrimitiveType::U16),
    RegisterType::Primitive(PrimitiveType::U16)
);
register_conversion_to_integer!(
    i32,
    s_int,
    RegisterType::Primitive(PrimitiveType::S32),
    RegisterType::Primitive(PrimitiveType::S32)
);
register_conversion_to_integer!(
    u32,
    u_int,
    RegisterType::Primitive(PrimitiveType::U32),
    RegisterType::Primitive(PrimitiveType::U32)
);
register_conversion_to_integer!(
    i64,
    s_long,
    RegisterType::Primitive(PrimitiveType::S64),
    RegisterType::Primitive(PrimitiveType::S64)
);
register_conversion_to_integer!(
    u64,
    u_long,
    RegisterType::Primitive(PrimitiveType::U64),
    RegisterType::Primitive(PrimitiveType::U64)
);
register_conversion_to_integer!(
    isize,
    s_native,
    RegisterType::Primitive(PrimitiveType::SNative),
    RegisterType::Primitive(PrimitiveType::SNative)
);
register_conversion_to_integer!(
    usize,
    u_native,
    RegisterType::Primitive(PrimitiveType::UNative),
    RegisterType::Primitive(PrimitiveType::UNative)
);

//register_conversion_to_float

macro_rules! basic_arithmetic_operation {
    ($operation_name: ident) => {
        impl Register {
            pub(crate) fn $operation_name(
                result_type: RegisterType,
                lhs: &Register,
                rhs: &Register,
            ) -> (Register, bool) {
                macro_rules! integer_operation {
                    ($integer_type: ty) => {
                        match <$integer_type>::$operation_name(
                            <$integer_type>::interpret_register(lhs),
                            <$integer_type>::interpret_register(rhs),
                        ) {
                            (value, overflowed) => (Register::from(value), overflowed), // TODO: Set overflowed flag to true if lhs or rhs converted to the integer_type would result in overflow
                        }
                    };
                }

                match result_type { // TODO: If one of the arguments is a NaN or Infinity, what should be done?
                    RegisterType::Primitive(PrimitiveType::S8) => integer_operation!(i8),
                    RegisterType::Primitive(PrimitiveType::U8) => integer_operation!(u8),
                    RegisterType::Primitive(PrimitiveType::S16) => integer_operation!(i16),
                    RegisterType::Primitive(PrimitiveType::U16) => integer_operation!(u16),
                    RegisterType::Primitive(PrimitiveType::S32) => integer_operation!(i32),
                    RegisterType::Primitive(PrimitiveType::U32) => integer_operation!(u32),
                    RegisterType::Primitive(PrimitiveType::S64) => integer_operation!(i64),
                    RegisterType::Primitive(PrimitiveType::U64) => integer_operation!(u64),
                    RegisterType::Primitive(PrimitiveType::SNative) => integer_operation!(i64),
                    RegisterType::Primitive(PrimitiveType::UNative) => integer_operation!(u64),
                    RegisterType::Primitive(PrimitiveType::F32 | PrimitiveType::F64) => todo!("Basic arithmetic operations are not yet supported for floating-point numbers"),
                }
            }
        }
    };
}

basic_arithmetic_operation!(overflowing_add);
basic_arithmetic_operation!(overflowing_sub);
basic_arithmetic_operation!(overflowing_mul);
// NOTE: Need to handle overflows of division, since MAXIMUM / -1 overflows.

#[cfg(test)]
mod tests {
    use crate::interpreter::register::RegisterValue;

    #[test]
    fn register_value_size_is_correct() {
        use std::mem::size_of;
        assert!(size_of::<RegisterValue>() <= size_of::<u64>());
    }
}

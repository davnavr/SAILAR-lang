use crate::interpreter;
use std::ops::{BitAnd, BitOr, BitXor, Shl, Shr};

pub use interpreter::{
    instruction_set::{NumericType, RegisterType},
    IntegerConstant, PrimitiveType,
};

#[derive(Clone, Copy)]
pub union RegisterValue {
    // NOTE: Currently, some code assumes that groups of fields (e.g. s_int, u_int, and f_single) start at the same offset.
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

#[derive(Clone)]
pub struct Register {
    pub(super) value: RegisterValue,
    pub(super) value_type: RegisterType,
}

impl Register {
    pub(crate) fn uninitialized() -> Self {
        Self {
            value: RegisterValue::default(),
            value_type: RegisterType::Primitive(PrimitiveType::U8),
        }
    }

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

    pub fn is_truthy(&self) -> bool {
        match self.value_type {
            RegisterType::Primitive(PrimitiveType::U8 | PrimitiveType::S8) => unsafe {
                self.value.u_byte != 0
            },
            RegisterType::Primitive(PrimitiveType::U16 | PrimitiveType::S16) => unsafe {
                self.value.u_long != 0
            },
            RegisterType::Primitive(
                PrimitiveType::U32 | PrimitiveType::S32 | PrimitiveType::F32,
            ) => unsafe { self.value.u_int != 0 },
            RegisterType::Primitive(
                PrimitiveType::U64 | PrimitiveType::S64 | PrimitiveType::F64,
            ) => unsafe { self.value.u_long != 0 },
            RegisterType::Primitive(PrimitiveType::UNative | PrimitiveType::SNative) => unsafe {
                self.value.u_native != 0usize
            },
        }
    }
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value_type {
            RegisterType::Primitive(PrimitiveType::U8) => unsafe { self.value.u_byte.fmt(f) },
            RegisterType::Primitive(PrimitiveType::S8) => unsafe { self.value.s_byte.fmt(f) },
            RegisterType::Primitive(PrimitiveType::U16) => unsafe { self.value.u_short.fmt(f) },
            RegisterType::Primitive(PrimitiveType::S16) => unsafe { self.value.s_short.fmt(f) },
            RegisterType::Primitive(PrimitiveType::U32) => unsafe { self.value.u_int.fmt(f) },
            RegisterType::Primitive(PrimitiveType::S32) => unsafe { self.value.s_int.fmt(f) },
            RegisterType::Primitive(PrimitiveType::U64) => unsafe { self.value.u_long.fmt(f) },
            RegisterType::Primitive(PrimitiveType::S64) => unsafe { self.value.s_long.fmt(f) },
            RegisterType::Primitive(PrimitiveType::UNative) => unsafe {
                self.value.u_native.fmt(f)
            },
            RegisterType::Primitive(PrimitiveType::SNative) => unsafe {
                self.value.s_native.fmt(f)
            },
            RegisterType::Primitive(PrimitiveType::F32) => unsafe { self.value.f_single.fmt(f) },
            RegisterType::Primitive(PrimitiveType::F64) => unsafe { self.value.f_double.fmt(f) },
        }
    }
}

impl std::fmt::Debug for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Register")
            .field("value_type", &self.value_type)
            .field("value", &self.to_string())
            .finish()
    }
}

impl std::fmt::UpperHex for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value_type {
            RegisterType::Primitive(PrimitiveType::U8) => unsafe { self.value.u_byte.fmt(f) },
            RegisterType::Primitive(PrimitiveType::S8) => unsafe { self.value.s_byte.fmt(f) },
            RegisterType::Primitive(PrimitiveType::U16) => unsafe { self.value.u_short.fmt(f) },
            RegisterType::Primitive(PrimitiveType::S16) => unsafe { self.value.s_short.fmt(f) },
            RegisterType::Primitive(PrimitiveType::U32 | PrimitiveType::F32) => unsafe {
                self.value.u_int.fmt(f)
            },
            RegisterType::Primitive(PrimitiveType::S32) => unsafe { self.value.s_int.fmt(f) },
            RegisterType::Primitive(PrimitiveType::U64 | PrimitiveType::F64) => unsafe {
                self.value.u_long.fmt(f)
            },
            RegisterType::Primitive(PrimitiveType::S64) => unsafe { self.value.s_long.fmt(f) },
            RegisterType::Primitive(PrimitiveType::UNative) => unsafe {
                self.value.u_native.fmt(f)
            },
            RegisterType::Primitive(PrimitiveType::SNative) => unsafe {
                self.value.s_native.fmt(f)
            },
        }
    }
}

trait InterpretRegister {
    /// Interprets the value of the register, performing any necessary conversions.
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

macro_rules! typed_integer_operation {
    ($operation_name: ident, $integer_type: ty, $x: ident, $y: ident) => {{
        let (value, overflowed) = <$integer_type>::$operation_name(
            <$integer_type>::interpret_register($x),
            <$integer_type>::interpret_register($y),
        );
        (Register::from(value), overflowed)
    }};
}

macro_rules! basic_arithmetic_operation {
    ($operation_name: ident) => {
        impl Register {
            pub fn $operation_name(
                result_type: RegisterType,
                lhs: &Register,
                rhs: &Register,
            ) -> (Register, bool) {
                macro_rules! integer_operation {
                    ($integer_type: ty) => {
                        typed_integer_operation!($operation_name, $integer_type, lhs, rhs)
                        // TODO: Set overflowed flag to true if lhs or rhs converted to the integer_type would result in overflow
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
                    RegisterType::Primitive(PrimitiveType::SNative) => integer_operation!(isize),
                    RegisterType::Primitive(PrimitiveType::UNative) => integer_operation!(usize),
                    RegisterType::Primitive(PrimitiveType::F32 | PrimitiveType::F64) => todo!("Basic arithmetic operations are not yet supported for floating-point numbers"),
                }
            }
        }
    };
}

basic_arithmetic_operation!(overflowing_add);
basic_arithmetic_operation!(overflowing_sub);
basic_arithmetic_operation!(overflowing_mul);

macro_rules! basic_division_operation {
    ($operation_name: ident) => {
        impl Register {
            pub fn $operation_name(
                result_type: RegisterType,
                numerator: &Register,
                denominator: &Register,
            ) -> Option<(Register, bool)> {
                macro_rules! division_operation {
                    ($integer_type: ty) => {
                        {
                            let actual_numerator = <$integer_type>::interpret_register(numerator);
                            let actual_denominator = <$integer_type>::interpret_register(denominator);
                            if actual_denominator == 0 {
                                None
                            }
                            else {
                                let (value, overflowed) = <$integer_type>::$operation_name(actual_numerator, actual_denominator);
                                Some((Register::from(value), overflowed))
                            }
                        }
                    };
                }

                match result_type {
                    RegisterType::Primitive(PrimitiveType::S8) => division_operation!(i8),
                    RegisterType::Primitive(PrimitiveType::U8) => division_operation!(u8),
                    RegisterType::Primitive(PrimitiveType::S16) => division_operation!(i16),
                    RegisterType::Primitive(PrimitiveType::U16) => division_operation!(u16),
                    RegisterType::Primitive(PrimitiveType::S32) => division_operation!(i32),
                    RegisterType::Primitive(PrimitiveType::U32) => division_operation!(u32),
                    RegisterType::Primitive(PrimitiveType::S64) => division_operation!(i64),
                    RegisterType::Primitive(PrimitiveType::U64) => division_operation!(u64),
                    RegisterType::Primitive(PrimitiveType::SNative) => division_operation!(isize),
                    RegisterType::Primitive(PrimitiveType::UNative) => division_operation!(usize),
                    RegisterType::Primitive(PrimitiveType::F32 | PrimitiveType::F64) => todo!("Basic division operations are not yet supported for floating-point numbers"),
                }
            }
        }
    };
}

basic_division_operation!(overflowing_div);

macro_rules! basic_bitwise_operation {
    ($function_name: ident, $operation_name: ident) => {
        impl Register {
            pub fn $function_name(
                result_type: NumericType,
                x: &Register,
                y: &Register,
            ) -> Register {
                macro_rules! bitwise_operation {
                    ($integer_type: ty) => {
                        Register::from(<$integer_type>::$operation_name(
                            <$integer_type>::interpret_register(x),
                            <$integer_type>::interpret_register(y),
                        ))
                    };
                }

                match result_type {
                    NumericType::Primitive(PrimitiveType::S8 | PrimitiveType::U8) => {
                        bitwise_operation!(u8)
                    }
                    NumericType::Primitive(PrimitiveType::S16 | PrimitiveType::U16) => {
                        bitwise_operation!(u16)
                    }
                    NumericType::Primitive(
                        PrimitiveType::S32 | PrimitiveType::U32 | PrimitiveType::F32,
                    ) => bitwise_operation!(u32),
                    NumericType::Primitive(
                        PrimitiveType::S64 | PrimitiveType::U64 | PrimitiveType::F64,
                    ) => bitwise_operation!(u64),
                    NumericType::Primitive(PrimitiveType::SNative | PrimitiveType::UNative) => {
                        bitwise_operation!(usize)
                    }
                }
            }
        }
    };
}

basic_bitwise_operation!(bitwise_add, bitand);
basic_bitwise_operation!(bitwise_or, bitor);
basic_bitwise_operation!(bitwise_xor, bitxor);

impl Register {
    pub fn bitwise_not(result_type: NumericType, value: &Register) -> Register {
        macro_rules! bitwise_not_operation {
            ($integer_type: ty) => {
                Register::from(<$integer_type as std::ops::Not>::not(
                    <$integer_type>::interpret_register(value),
                ))
            };
        }

        match result_type {
            NumericType::Primitive(PrimitiveType::S8 | PrimitiveType::U8) => {
                bitwise_not_operation!(u8)
            }
            NumericType::Primitive(PrimitiveType::S16 | PrimitiveType::U16) => {
                bitwise_not_operation!(u16)
            }
            NumericType::Primitive(
                PrimitiveType::S32 | PrimitiveType::U32 | PrimitiveType::F32,
            ) => bitwise_not_operation!(u32),
            NumericType::Primitive(
                PrimitiveType::S64 | PrimitiveType::U64 | PrimitiveType::F64,
            ) => bitwise_not_operation!(u64),
            NumericType::Primitive(PrimitiveType::SNative | PrimitiveType::UNative) => {
                bitwise_not_operation!(usize)
            }
        }
    }
}

macro_rules! bitwise_shift_operation {
    ($operation_name: ident) => {
        impl Register {
            pub fn $operation_name(
                result_type: NumericType,
                value: &Register,
                amount: &Register,
            ) -> Register {
                macro_rules! shift {
                    ($value_type: ty) => {{
                        macro_rules! perform_shift {
                            ($amount_type: ty) => {
                                Register::from(<$value_type>::$operation_name(<$value_type>::interpret_register(value), <$amount_type>::interpret_register(amount)))
                            };
                        }

                        match amount.value_type {
                            RegisterType::Primitive(PrimitiveType::S8) => perform_shift!(i8),
                            RegisterType::Primitive(PrimitiveType::U8) => perform_shift!(u8),
                            RegisterType::Primitive(PrimitiveType::S16) => perform_shift!(i16),
                            RegisterType::Primitive(PrimitiveType::U16) => perform_shift!(u16),
                            RegisterType::Primitive(PrimitiveType::S32) => perform_shift!(i32),
                            RegisterType::Primitive(PrimitiveType::U32) => perform_shift!(u32),
                            RegisterType::Primitive(PrimitiveType::S64) => perform_shift!(i64),
                            RegisterType::Primitive(PrimitiveType::U64) => perform_shift!(u64),
                            RegisterType::Primitive(PrimitiveType::SNative) => perform_shift!(isize),
                            RegisterType::Primitive(PrimitiveType::UNative) => perform_shift!(usize),
                            RegisterType::Primitive(PrimitiveType::F32 | PrimitiveType::F64) => todo!("semantics regarding floating point amounts in bitwise operations have not yet been decided"),
                        }
                    }};
                }

                match result_type {
                    NumericType::Primitive(PrimitiveType::S8) => shift!(i8),
                    NumericType::Primitive(PrimitiveType::U8) => shift!(u8),
                    NumericType::Primitive(PrimitiveType::S16) => shift!(i16),
                    NumericType::Primitive(PrimitiveType::U16) => shift!(u16),
                    NumericType::Primitive(PrimitiveType::S32) => shift!(i32),
                    NumericType::Primitive(PrimitiveType::U32 | PrimitiveType::F32) => shift!(u32),
                    NumericType::Primitive(PrimitiveType::S64) => shift!(i64),
                    NumericType::Primitive(PrimitiveType::U64 | PrimitiveType::F64) => shift!(u64),
                    NumericType::Primitive(PrimitiveType::SNative) => shift!(isize),
                    NumericType::Primitive(PrimitiveType::UNative) => shift!(usize),
                }
            }
        }
    };
}

bitwise_shift_operation!(shl);
bitwise_shift_operation!(shr);

macro_rules! bitwise_rotate_operation {
    ($operation_name: ident) => {
        impl Register {
            pub fn $operation_name(
                result_type: NumericType,
                value: &Register,
                amount: &Register,
            ) -> Register {
                macro_rules! rotate {
                    ($integer_type: ty) => {
                        Register::from(<$integer_type>::$operation_name(
                            <$integer_type>::interpret_register(value),
                            u32::interpret_register(amount),
                        ))
                    };
                }

                match result_type {
                    NumericType::Primitive(PrimitiveType::U8 | PrimitiveType::S8) => rotate!(u8),
                    NumericType::Primitive(PrimitiveType::U16 | PrimitiveType::S16) => rotate!(i16),
                    NumericType::Primitive(
                        PrimitiveType::U32 | PrimitiveType::S32 | PrimitiveType::F32,
                    ) => rotate!(u32),
                    NumericType::Primitive(
                        PrimitiveType::U64 | PrimitiveType::S64 | PrimitiveType::F64,
                    ) => rotate!(u64),
                    NumericType::Primitive(PrimitiveType::UNative | PrimitiveType::SNative) => {
                        rotate!(usize)
                    }
                }
            }
        }
    };
}

bitwise_rotate_operation!(rotate_left);
bitwise_rotate_operation!(rotate_right);

#[cfg(test)]
mod tests {
    use crate::interpreter::register::RegisterValue;

    #[test]
    fn register_value_size_is_correct() {
        use std::mem::size_of;
        assert!(size_of::<RegisterValue>() <= size_of::<u64>());
    }
}

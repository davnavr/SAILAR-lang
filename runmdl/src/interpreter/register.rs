use crate::interpreter;

pub use interpreter::{
    instruction_set::{NumericType, RegisterType},
    IntegerConstant, PrimitiveType,
};

/*
#[derive(Clone, Copy, Eq, PartialEq)]
pub union RegisterValue {
    S8(i8),
    U8(u8),

    S32(i32),

    Pointer(*mut u8),
    Struct(*mut u8),
}
*/

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
    pointer: *mut u8,
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
    pub(super) fn new(value_type: RegisterType) -> Self {
        Self {
            value: RegisterValue::default(),
            value_type,
        }
    }

    pub(super) fn with_type(value_type: &interpreter::type_system::AnyType) -> Self {
        use interpreter::type_system::AnyType;

        Self::new(match value_type {
            AnyType::Primitive(primitive_type) => RegisterType::Primitive(*primitive_type),
            _ => todo!("Unsupported register type"),
        })
    }

    pub(super) fn many_with_type<
        'l,
        T: std::iter::IntoIterator<Item = &'l interpreter::type_system::AnyType>,
    >(
        types: T,
    ) -> Vec<Register> {
        types.into_iter().map(Self::with_type).collect()
    }

    pub(super) fn from_pointer(pointer: *mut u8, pointee_size: u32) -> Self {
        Self {
            value: RegisterValue { pointer },
            value_type: RegisterType::Pointer(pointee_size),
        }
    }

    /// Returns a value indicating whether the integer value contained in the register is not zero.
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
            RegisterType::Primitive(PrimitiveType::UNative | PrimitiveType::SNative)
            | RegisterType::Pointer(_) => unsafe { self.value.u_native != 0usize },
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
            RegisterType::Pointer(_) => std::fmt::UpperHex::fmt(unsafe { &self.value.u_native }, f),
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
        use std::fmt::UpperHex;

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
            RegisterType::Primitive(PrimitiveType::UNative | PrimitiveType::SNative)
            | RegisterType::Pointer(_) => UpperHex::fmt(unsafe { &self.value.u_native }, f),
        }
    }
}

impl std::cmp::PartialEq for Register {
    fn eq(&self, other: &Register) -> bool {
        macro_rules! value_equals {
            ($value_field: ident) => {
                unsafe { self.value.$value_field == other.value.$value_field }
            };
        }

        match self.value_type {
            // TODO: Should registers that point to a type of different size be considered unequal?
            self_type if self_type != other.value_type => false,
            RegisterType::Primitive(PrimitiveType::U8) => value_equals!(u_byte),
            RegisterType::Primitive(PrimitiveType::S8) => value_equals!(s_byte),
            RegisterType::Primitive(PrimitiveType::U16) => value_equals!(u_short),
            RegisterType::Primitive(PrimitiveType::S16) => value_equals!(s_short),
            RegisterType::Primitive(PrimitiveType::U32) => value_equals!(u_int),
            RegisterType::Primitive(PrimitiveType::S32) => value_equals!(s_int),
            RegisterType::Primitive(PrimitiveType::U64) => value_equals!(u_long),
            RegisterType::Primitive(PrimitiveType::S64) => value_equals!(s_long),
            RegisterType::Primitive(PrimitiveType::UNative) => value_equals!(u_native),
            RegisterType::Primitive(PrimitiveType::SNative) => value_equals!(s_native),
            RegisterType::Primitive(PrimitiveType::F32) => value_equals!(f_single),
            RegisterType::Primitive(PrimitiveType::F64) => value_equals!(f_double),
            RegisterType::Pointer(_) => value_equals!(pointer),
        }
    }
}

impl std::cmp::Eq for Register {}

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

impl TryFrom<&Register> for IntegerConstant {
    type Error = ();

    fn try_from(register: &Register) -> Result<Self, Self::Error> {
        macro_rules! extract_value {
            ($constant_case: ident, $register_field: ident) => {
                Ok(Self::$constant_case(unsafe {
                    register.value.$register_field
                }))
            };
        }

        match register.value_type {
            RegisterType::Primitive(PrimitiveType::S8) => extract_value!(S8, s_byte),
            RegisterType::Primitive(PrimitiveType::U8) => extract_value!(U8, u_byte),
            RegisterType::Primitive(PrimitiveType::S16) => extract_value!(S16, s_short),
            RegisterType::Primitive(PrimitiveType::U16) => extract_value!(U16, u_short),
            RegisterType::Primitive(PrimitiveType::S32) => extract_value!(S32, s_int),
            RegisterType::Primitive(PrimitiveType::U32) => extract_value!(U32, u_int),
            RegisterType::Primitive(PrimitiveType::S64) => extract_value!(S64, s_long),
            RegisterType::Primitive(PrimitiveType::U64) => extract_value!(U64, u_long),
            _ => Err(()),
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
                    RegisterType::Primitive(PrimitiveType::UNative) | RegisterType::Pointer(_) => {
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
            pub fn $operation_name(
                self: &Self,
                other: &Self,
            ) -> Result<(Self, bool), RegisterType> {
                macro_rules! integer_operation {
                    ($value_field: ident) => {{
                        let (value, overflowed) = (unsafe { self.value.$value_field })
                            .$operation_name(unsafe { other.value.$value_field });
                        Ok((Register::from(value), overflowed))
                    }};
                }

                match (self.value_type, other.value_type) {
                    (
                        RegisterType::Primitive(PrimitiveType::S8),
                        RegisterType::Primitive(PrimitiveType::S8),
                    ) => integer_operation!(s_byte),
                    (
                        RegisterType::Primitive(PrimitiveType::U8),
                        RegisterType::Primitive(PrimitiveType::U8),
                    ) => integer_operation!(u_byte),
                    (
                        RegisterType::Primitive(PrimitiveType::S16),
                        RegisterType::Primitive(PrimitiveType::S16),
                    ) => integer_operation!(s_long),
                    (
                        RegisterType::Primitive(PrimitiveType::U16),
                        RegisterType::Primitive(PrimitiveType::U16),
                    ) => integer_operation!(u_long),
                    (
                        RegisterType::Primitive(PrimitiveType::S32),
                        RegisterType::Primitive(PrimitiveType::S32),
                    ) => integer_operation!(s_long),
                    (
                        RegisterType::Primitive(PrimitiveType::U32),
                        RegisterType::Primitive(PrimitiveType::U32),
                    ) => integer_operation!(u_long),
                    (
                        RegisterType::Primitive(PrimitiveType::S64),
                        RegisterType::Primitive(PrimitiveType::S64),
                    ) => integer_operation!(s_long),
                    (
                        RegisterType::Primitive(PrimitiveType::U64),
                        RegisterType::Primitive(PrimitiveType::U64),
                    ) => integer_operation!(u_long),
                    (
                        RegisterType::Primitive(PrimitiveType::SNative),
                        RegisterType::Primitive(PrimitiveType::SNative),
                    ) => integer_operation!(s_native),
                    (
                        RegisterType::Primitive(PrimitiveType::UNative),
                        RegisterType::Primitive(PrimitiveType::UNative),
                    ) => integer_operation!(u_native),
                    (
                        RegisterType::Primitive(PrimitiveType::F32),
                        RegisterType::Primitive(PrimitiveType::F32),
                    ) => Ok((
                        Register::from(unsafe { self.value.f_single + other.value.f_single }),
                        false,
                    )),
                    (
                        RegisterType::Primitive(PrimitiveType::F64),
                        RegisterType::Primitive(PrimitiveType::F64),
                    ) => Ok((
                        Register::from(unsafe { self.value.f_double + other.value.f_double }),
                        false,
                    )),
                    // TODO: Add case for pointer arithmetic
                    //(RegisterType::Pointer(pointee_size), _)
                    (_, _) => Err(other.value_type), // TODO: Always allow adding a 32-bit integer to a native integer?
                }
            }
        }
    };
}

basic_arithmetic_operation!(overflowing_add);
basic_arithmetic_operation!(overflowing_sub);
basic_arithmetic_operation!(overflowing_mul);

#[cfg(test)]
mod tests {
    use crate::interpreter::register::RegisterValue;

    #[test]
    fn register_value_size_is_correct() {
        use std::mem::size_of;
        assert!(size_of::<RegisterValue>() <= size_of::<u64>());
    }
}

use registir::format::{instruction_set::IntegerConstant, type_system};
use std::fmt::{Display, Formatter};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Type {
    Primitive(type_system::Primitive),
    Pointer(usize),
}

impl From<type_system::Int> for Type {
    fn from(integer_type: type_system::Int) -> Self {
        Self::Primitive(type_system::Primitive::Int(integer_type))
    }
}

impl From<type_system::FixedInt> for Type {
    fn from(integer_type: type_system::FixedInt) -> Self {
        Self::from(type_system::Int::Fixed(integer_type))
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum IntVal {
    S8(i8),
    U8(u8),
    S16(i16),
    U16(u16),
    S32(i32),
    U32(u32),
    S64(i64),
    U64(u64),
    SNative(isize),
    UNative(usize),
}

impl IntVal {
    pub fn value_type(&self) -> type_system::Int {
        use type_system::{FixedInt, Int};
        match self {
            Self::S8(_) => Int::Fixed(FixedInt::S8),
            Self::U8(_) => Int::Fixed(FixedInt::U8),
            Self::S16(_) => Int::Fixed(FixedInt::S16),
            Self::U16(_) => Int::Fixed(FixedInt::U16),
            Self::S32(_) => Int::Fixed(FixedInt::S32),
            Self::U32(_) => Int::Fixed(FixedInt::U32),
            Self::S64(_) => Int::Fixed(FixedInt::S64),
            Self::U64(_) => Int::Fixed(FixedInt::U64),
            Self::SNative(_) => Int::SNative,
            Self::UNative(_) => Int::UNative,
        }
    }

    pub fn from_integer_type(source_type: type_system::Int) -> Self {
        use type_system::{FixedInt, Int};
        match source_type {
            Int::Fixed(FixedInt::U8) => Self::U8(0),
            Int::Fixed(FixedInt::S8) => Self::S8(0),
            Int::Fixed(FixedInt::U16) => Self::U16(0),
            Int::Fixed(FixedInt::S16) => Self::S16(0),
            Int::Fixed(FixedInt::U32) => Self::U32(0),
            Int::Fixed(FixedInt::S32) => Self::S32(0),
            Int::Fixed(FixedInt::U64) => Self::U64(0),
            Int::Fixed(FixedInt::S64) => Self::S64(0),
            Int::UNative => Self::UNative(0),
            Int::SNative => Self::SNative(0),
        }
    }
}

macro_rules! int_val_display_impl {
    ($trait_name: ident) => {
        impl std::fmt::$trait_name for IntVal {
            fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
                match self {
                    Self::S8(value) => std::fmt::$trait_name::fmt(value, f),
                    Self::U8(value) => std::fmt::$trait_name::fmt(value, f),
                    Self::S16(value) => std::fmt::$trait_name::fmt(value, f),
                    Self::U16(value) => std::fmt::$trait_name::fmt(value, f),
                    Self::S32(value) => std::fmt::$trait_name::fmt(value, f),
                    Self::U32(value) => std::fmt::$trait_name::fmt(value, f),
                    Self::S64(value) => std::fmt::$trait_name::fmt(value, f),
                    Self::U64(value) => std::fmt::$trait_name::fmt(value, f),
                    Self::SNative(value) => std::fmt::$trait_name::fmt(value, f),
                    Self::UNative(value) => std::fmt::$trait_name::fmt(value, f),
                }
            }
        }
    };
}

int_val_display_impl!(Display);
int_val_display_impl!(UpperHex);
int_val_display_impl!(LowerHex);

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PointerVal {
    pointee_size: usize,
    address: *mut u8,
}

impl PointerVal {
    pub(super) fn new(address: *mut u8, pointee_size: usize) -> Self {
        Self {
            address,
            pointee_size,
        }
    }

    pub fn pointee_size(&self) -> usize {
        self.pointee_size
    }

    pub fn address(&self) -> *const u8 {
        self.address
    }

    pub fn address_mut(&self) -> *mut u8 {
        self.address
    }
}

impl Display for PointerVal {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        std::fmt::UpperHex::fmt(&(self.address as usize), f)
    }
}

impl std::fmt::UpperHex for PointerVal {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl From<&PointerVal> for usize {
    fn from(pointer: &PointerVal) -> Self {
        pointer.address as usize
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[non_exhaustive]
pub enum Register {
    Int(IntVal),
    //Real(FloatVal),
    Pointer(PointerVal),
}

impl Register {
    pub fn value_type(&self) -> Type {
        match self {
            Self::Int(value) => Type::Primitive(type_system::Primitive::Int(value.value_type())),
            Self::Pointer(value) => Type::Pointer(value.pointee_size()),
        }
    }

    /// Returns `true` if the value contained in the register is not zero.
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Int(
                IntVal::S8(0)
                | IntVal::U8(0)
                | IntVal::S16(0)
                | IntVal::U16(0)
                | IntVal::S32(0)
                | IntVal::U32(0)
                | IntVal::S64(0)
                | IntVal::U64(0)
                | IntVal::SNative(0)
                | IntVal::UNative(0),
            ) => false,
            Self::Pointer(value) => !value.address.is_null(),
            Self::Int(_) => true,
        }
    }

    pub(super) fn from_type<'a>(source_type: &'a getmdl::loader::TypeSignature<'a>) -> Self {
        use type_system::{Any, FixedInt, Int, Primitive};
        match source_type.as_raw() {
            Any::Primitive(Primitive::Int(integer_type)) => {
                Self::Int(IntVal::from_integer_type(*integer_type))
            }
            _ => todo!(),
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::Int(value) => Display::fmt(value, f),
            Register::Pointer(value) => Display::fmt(value, f),
        }
    }
}

impl std::fmt::UpperHex for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::Int(value) => std::fmt::UpperHex::fmt(value, f),
            Register::Pointer(value) => std::fmt::UpperHex::fmt(value, f),
        }
    }
}

macro_rules! register_conversion_from_integer {
    ($source_type: ty, $value_field: ident) => {
        impl From<$source_type> for Register {
            fn from(value: $source_type) -> Self {
                Self::Int(IntVal::$value_field(value))
            }
        }
    };
}

register_conversion_from_integer!(i8, S8);
register_conversion_from_integer!(u8, U8);
register_conversion_from_integer!(i16, S16);
register_conversion_from_integer!(u16, U16);
register_conversion_from_integer!(i32, S32);
register_conversion_from_integer!(u32, U32);
register_conversion_from_integer!(i64, S64);
register_conversion_from_integer!(u64, U64);
register_conversion_from_integer!(isize, SNative);
register_conversion_from_integer!(usize, UNative);

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
        if let Register::Int(integer_value) = register {
            match integer_value {
                IntVal::S8(value) => Ok(Self::S8(*value)),
                IntVal::U8(value) => Ok(Self::U8(*value)),
                IntVal::S16(value) => Ok(Self::S16(*value)),
                IntVal::U16(value) => Ok(Self::U16(*value)),
                IntVal::S32(value) => Ok(Self::S32(*value)),
                IntVal::U32(value) => Ok(Self::U32(*value)),
                IntVal::S64(value) => Ok(Self::S64(*value)),
                IntVal::U64(value) => Ok(Self::U64(*value)),
                _ => Err(()),
            }
        } else {
            Err(())
        }
    }
}

/// The error type returned when a conversion from a register value fails.
#[derive(Debug, Clone, thiserror::Error)]
#[error("expected register to contain a value of type {expected:?} but got {actual:?}")]
pub struct TryFromRegisterValueError {
    expected: Type,
    actual: Type,
}

macro_rules! register_conversion_to_integer {
    ($destination_type: ty, $conversion_error_type: expr) => {
        impl TryFrom<&Register> for $destination_type {
            type Error = TryFromRegisterValueError;

            fn try_from(register: &Register) -> Result<Self, Self::Error> {
                macro_rules! integer_conversion {
                    ($value: expr) => {
                        <$destination_type>::try_from($value).map_err(|_| {
                            TryFromRegisterValueError {
                                expected: $conversion_error_type,
                                actual: register.value_type(),
                            }
                        })
                    };
                }

                match register {
                    Register::Int(integer_value) => match integer_value {
                        IntVal::S8(value) => integer_conversion!(*value),
                        IntVal::U8(value) => integer_conversion!(*value),
                        IntVal::S16(value) => integer_conversion!(*value),
                        IntVal::U16(value) => integer_conversion!(*value),
                        IntVal::S32(value) => integer_conversion!(*value),
                        IntVal::U32(value) => integer_conversion!(*value),
                        IntVal::S64(value) => integer_conversion!(*value),
                        IntVal::U64(value) => integer_conversion!(*value),
                        IntVal::SNative(value) => integer_conversion!(*value),
                        IntVal::UNative(value) => integer_conversion!(*value),
                    },
                    Register::Pointer(pointer_value) => {
                        integer_conversion!(usize::from(pointer_value))
                    }
                }
            }
        }
    };
}

register_conversion_to_integer!(i8, Type::from(type_system::FixedInt::S8));
register_conversion_to_integer!(u8, Type::from(type_system::FixedInt::U8));
register_conversion_to_integer!(i16, Type::from(type_system::FixedInt::S16));
register_conversion_to_integer!(u16, Type::from(type_system::FixedInt::U16));
register_conversion_to_integer!(i32, Type::from(type_system::FixedInt::S32));
register_conversion_to_integer!(u32, Type::from(type_system::FixedInt::U32));
register_conversion_to_integer!(usize, Type::from(type_system::Int::UNative));

macro_rules! basic_arithmetic_operation {
    ($operation_name: ident) => {
        impl Register {
            pub fn $operation_name(self: &Self, other: &Self) -> Result<(Self, bool), Type> {
                macro_rules! integer_operation {
                    ($x: ident, $y: ident) => {{
                        let (value, overflowed) = $x.$operation_name(*$y);
                        Ok((Register::from(value), overflowed))
                    }};
                }

                match (self, other) {
                    (Self::Int(IntVal::S8(x)), Self::Int(IntVal::S8(y))) => {
                        integer_operation!(x, y)
                    }
                    (Self::Int(IntVal::U8(x)), Self::Int(IntVal::U8(y))) => {
                        integer_operation!(x, y)
                    }
                    (Self::Int(IntVal::S16(x)), Self::Int(IntVal::S16(y))) => {
                        integer_operation!(x, y)
                    }
                    (Self::Int(IntVal::U16(x)), Self::Int(IntVal::U16(y))) => {
                        integer_operation!(x, y)
                    }
                    (Self::Int(IntVal::S32(x)), Self::Int(IntVal::S32(y))) => {
                        integer_operation!(x, y)
                    }
                    (Self::Int(IntVal::U32(x)), Self::Int(IntVal::U32(y))) => {
                        integer_operation!(x, y)
                    }
                    (Self::Int(IntVal::S64(x)), Self::Int(IntVal::S64(y))) => {
                        integer_operation!(x, y)
                    }
                    (Self::Int(IntVal::U64(x)), Self::Int(IntVal::U64(y))) => {
                        integer_operation!(x, y)
                    }
                    (Self::Int(IntVal::SNative(x)), Self::Int(IntVal::SNative(y))) => {
                        integer_operation!(x, y)
                    }
                    (Self::Int(IntVal::UNative(x)), Self::Int(IntVal::UNative(y))) => {
                        integer_operation!(x, y)
                    }
                    // TODO: Add case for pointer arithmetic
                    //(RegisterType::Pointer(pointee_size), _)
                    (_, _) => Err(other.value_type()), // TODO: Always allow adding a 32-bit integer to a native integer?
                }
            }
        }
    };
}

basic_arithmetic_operation!(overflowing_add);
basic_arithmetic_operation!(overflowing_sub);
basic_arithmetic_operation!(overflowing_mul);

//! Model of the SAILAR instruction set.

use crate::binary::index;
use std::fmt::{Debug, Formatter};

/// Represents a constant integer value stored in little-endian order. Whether or not the value is signed is inferred from
/// context.
#[derive(Clone, Eq, Hash, PartialEq)]
pub enum ConstantInteger {
    I8(u8),
    I16([u8; 2]),
    I32([u8; 4]),
    I64([u8; 8]),
}

impl Debug for ConstantInteger {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::I8(value) => write!(f, "I8({:#02X})", value),
            Self::I16(value) => write!(f, "I16({:#04X})", u16::from_le_bytes(*value)),
            Self::I32(value) => write!(f, "I32({:#08X})", u32::from_le_bytes(*value)),
            Self::I64(value) => write!(f, "I64({:#016X})", u64::from_le_bytes(*value)),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Constant {
    Integer(ConstantInteger),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Value {
    Constant(Constant),
    IndexedRegister(usize),
}

macro_rules! integer_conversion_impls {
    ($constant_case_name: ident, $integer_type: ty) => {
        impl From<$integer_type> for ConstantInteger {
            #[inline]
            fn from(value: $integer_type) -> Self {
                Self::$constant_case_name(value.to_le_bytes())
            }
        }

        impl From<$integer_type> for Constant {
            #[inline]
            fn from(value: $integer_type) -> Self {
                Self::Integer(ConstantInteger::from(value))
            }
        }

        impl From<$integer_type> for Value {
            #[inline]
            fn from(value: $integer_type) -> Self {
                Self::Constant(Constant::from(value))
            }
        }
    };
}

impl From<u8> for Value {
    #[inline]
    fn from(value: u8) -> Self {
        Self::Constant(Constant::Integer(ConstantInteger::I8(value)))
    }
}

integer_conversion_impls!(I16, u16);
integer_conversion_impls!(I16, i16);
integer_conversion_impls!(I32, u32);
integer_conversion_impls!(I32, i32);
integer_conversion_impls!(I64, u64);
integer_conversion_impls!(I64, i64);

bitflags::bitflags! {
    #[repr(transparent)]
    pub struct ValueFlags: u8 {
        const REGISTER = 0;
        const CONSTANT = 0b0000_0001;
        const INTEGER = 0b0000_0010;
        const INTEGER_SIZE_MASK = 0b0000_1100;
        const INTEGER_SIZE_1 = 0;
        const INTEGER_SIZE_2 = 0b0000_0100;
        const INTEGER_SIZE_4 = 0b0000_1000;
        const INTEGER_SIZE_8 = 0b0000_1100;
        const INTEGER_EMBEDDED = 0b0001_0000;
        const INTEGER_EMBEDDED_ONE = 0b0010_0000;
    }
}

impl Value {
    pub fn flags(&self) -> ValueFlags {
        match self {
            Self::IndexedRegister(_) => ValueFlags::REGISTER,
            Self::Constant(Constant::Integer(integer)) => {
                let mut integer_flags = ValueFlags::CONSTANT | ValueFlags::INTEGER;

                macro_rules! unsigned_integer_flag {
                    ($value: expr, $size: ident) => {{
                        integer_flags |= ValueFlags::$size;

                        match $value {
                            0 => integer_flags |= ValueFlags::INTEGER_EMBEDDED,
                            1 => integer_flags |= ValueFlags::INTEGER_EMBEDDED | ValueFlags::INTEGER_EMBEDDED_ONE,
                            _ => (),
                        }
                    }};
                }

                match integer {
                    ConstantInteger::I8(value) => unsigned_integer_flag!(value, INTEGER_SIZE_1),
                    ConstantInteger::I16(value) => unsigned_integer_flag!(u16::from_le_bytes(*value), INTEGER_SIZE_2),
                    ConstantInteger::I32(value) => unsigned_integer_flag!(u32::from_le_bytes(*value), INTEGER_SIZE_4),
                    ConstantInteger::I64(value) => unsigned_integer_flag!(u64::from_le_bytes(*value), INTEGER_SIZE_8),
                };

                integer_flags
            }
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[repr(u8)]
#[non_exhaustive]
pub enum OverflowBehavior {
    /// Allows silent overflow or underflow of the value.
    Ignore = 0,
    /// Indicates that an extra temporary register should be introduced after the result register containing a boolean value
    /// indicating if an overflow or underflow occured.
    Flag = 1,
    /// Keeps the value at the maximum if an overflow would occur, or at the minimum if an underflow would occur.
    Saturate = 2,
}

impl From<OverflowBehavior> for u8 {
    #[inline]
    fn from(behavior_value: OverflowBehavior) -> u8 {
        behavior_value as u8
    }
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("{value:#02X} is not a valid overflow behavior value")]
pub struct InvalidOverflowBehaviorError {
    value: u8,
}

impl TryFrom<u8> for OverflowBehavior {
    type Error = InvalidOverflowBehaviorError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Ignore),
            1 => Ok(Self::Flag),
            2 => Ok(Self::Saturate),
            _ => Err(InvalidOverflowBehaviorError { value }),
        }
    }
}

/// Describes a basic arithmetic operation on integers.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct IntegerArithmetic {
    overflow_behavior: OverflowBehavior,
    x: Value,
    y: Value,
}

impl IntegerArithmetic {
    pub fn new(overflow_behavior: OverflowBehavior, x: Value, y: Value) -> Self {
        Self { overflow_behavior, x, y }
    }

    #[inline]
    pub fn overflow_behavior(&self) -> OverflowBehavior {
        self.overflow_behavior
    }

    #[inline]
    pub fn x_value(&self) -> &Value {
        &self.x
    }

    #[inline]
    pub fn y_value(&self) -> &Value {
        &self.y
    }
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("{value:#02X} is not a valid opcode")]
pub struct InvalidOpcodeError {
    value: u8,
}

macro_rules! instruction_set {
    ({
        $($(#[$instruction_meta:meta])* $instruction_name:ident$(($($instruction_argument_name:ident: $instruction_argument:ty,)*))? = $instruction_code:literal,)*
    }) => {
        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
        #[repr(u8)]
        pub enum Opcode {
            $($instruction_name = $instruction_code,)*
        }

        impl TryFrom<u8> for Opcode {
            type Error = InvalidOpcodeError;

            fn try_from(value: u8) -> Result<Self, Self::Error> {
                match value {
                    $(_ if value == $instruction_code => Ok(Self::$instruction_name),)*
                    _ => Err(InvalidOpcodeError { value }),
                }
            }
        }

        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        #[non_exhaustive]
        pub enum Instruction {
            $($(#[$instruction_meta])* $instruction_name$(($($instruction_argument,)*))?,)*
        }

        impl Instruction {
            pub fn opcode(&self) -> Opcode {
                match self {
                    $(Self::$instruction_name$(($($instruction_argument_name,)*))? => Opcode::$instruction_name,)*
                }
            }
        }
    };
}

instruction_set! {{
    /// ```text
    /// nop
    /// ```
    /// Does absolutely nothing.
    Nop = 0,
    /// ```text
    /// break
    /// ```
    /// On supported platforms, indicates a debugger breakpoint has been hit. Behaves like a `nop` instruction otherwise.
    Break = 1,
    /// ```text
    /// ret <value0>, <value1>, ... ; Return multiple values
    /// ret ; Return no values
    /// ```
    /// Transfers control flow back to the calling function, providing the specified return value(s).
    Ret(_values: Box<[Value]>,) = 2,
    // Select = 3,
    // Switch = 4,
    // Br = 5,
    // BrIf = 6,
    /// ```text
    /// <result0>, <result1>, ... = call <function> (<argument0>, <argument1>, ...) ; Call function with return values
    /// call <function> (<argument0>, <argument1>, ...) ; Call function with no return values
    /// ```
    /// Transfers control flow to the specified `function`, providing the specified values as arguments.
    Call(_callee: u32, _arguments: Box<[Value]>,) = 7,
    //CallIndr = 8,
    //CallRet = 9,
    /// ```text
    /// <sum> = addi <x> <y>
    /// <sum> = addi sat <x> <y>
    /// <sum>, <overflowed> = addi ovf <x> <y>
    /// ```
    /// Calculates the sum of two integer values.
    AddI(_op: Box<IntegerArithmetic>,) = 0xA,
    /// ```text
    /// <sum> = subi <x> <y> ; Calculates x - y
    /// <sum> = subi sat <x> <y>
    /// <sum>, <overflowed> = subi ovf <x> <y>
    /// ```
    /// Calculates the integer result of subtracting `y` from `x`.
    SubI(_op: Box<IntegerArithmetic>,) = 0xB,
    // TODO: Could introduce muli overflow variant that returns the HIGH overflowing bits instead of just a single I overflow bool.
    //MulI(_op: Box<IntegerArithmetic>,) = 0xC,
    //DivI = 0xD,
    //RemI,
    //ModI,
    //DivRemI,
    //AddF,
    //SubF,
    //MulF,
    //DivF,
    //RemF,
    //NegF,
    //Not
    //And,
    //Or,
    //Xor,
    //Rotate,
    //Cmp,
    //BitCount,
    //Reverse,
}}

impl From<Opcode> for u8 {
    #[inline]
    fn from(opcode: Opcode) -> u8 {
        opcode as u8
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn instruction_size_is_acceptable() {
        assert!(std::mem::size_of::<Instruction>() <= 24);
    }

    #[test]
    fn integer_value_flags_are_correct() {
        macro_rules! value_flags {
            ($flag: ident) => (ValueFlags::$flag);
            ($flag: ident, $($remaining: ident),+) => (ValueFlags::$flag | value_flags!($($remaining),+))
        }

        macro_rules! assert_flags_eq {
            ($value: expr, $($flags: ident),+) => (
                assert_eq!(Value::from($value).flags(), value_flags!($($flags),+))
            )
        }

        assert_flags_eq!(
            1i32,
            CONSTANT,
            INTEGER,
            INTEGER_SIZE_4,
            INTEGER_EMBEDDED,
            INTEGER_EMBEDDED_ONE
        );

        assert_flags_eq!(0i16, CONSTANT, INTEGER, INTEGER_SIZE_2, INTEGER_EMBEDDED);
        assert_flags_eq!(10u32, CONSTANT, INTEGER, INTEGER_SIZE_4);
        assert_flags_eq!(42u64, CONSTANT, INTEGER, INTEGER_SIZE_8);

        assert_flags_eq!(
            1u64,
            CONSTANT,
            INTEGER,
            INTEGER_SIZE_8,
            INTEGER_EMBEDDED,
            INTEGER_EMBEDDED_ONE
        );
    }
}
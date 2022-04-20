//! Model of the SAILAR instruction set.

/// Represents a constant integer value stored in little-endian order. Whether or not the value is signed is inferred from
/// context.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ConstantInteger {
    I8(u8),
    I16([u8; 2]),
    I32([u8; 4]),
    I64([u8; 8]),
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

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[repr(u8)]
pub enum Opcode {
    Nop = 0,
    Break = 1,
    Ret = 2,
    // Select = 3,
    // Switch = 4,
    // Br = 5,
    // BrIf = 6,
    // Call = 7,
    AddI = 8,
    SubI = 9,
    MulI = 0xA,
}

impl From<Opcode> for u8 {
    #[inline]
    fn from(opcode: Opcode) -> u8 {
        opcode as u8
    }
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("{value:#02X} is not a valid opcode")]
pub struct InvalidOpcodeError {
    value: u8,
}

impl TryFrom<u8> for Opcode {
    type Error = InvalidOpcodeError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        macro_rules! success {
            ($opcode: ident) => {
                Ok(Self::$opcode)
            };
        }

        match value {
            0 => success!(Nop),
            1 => success!(Break),
            2 => success!(Ret),
            8 => success!(AddI),
            9 => success!(SubI),
            0xA => success!(MulI),
            _ => Err(InvalidOpcodeError { value }),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Instruction {
    Nop,
    Break,
    Ret(Box<[Value]>),
    //Select,
    //Switch,
    //Br,
    //BrIf,
    //Call,
    //CallIndr,
    //CallRet,
    AddI(Box<IntegerArithmetic>),
    SubI(Box<IntegerArithmetic>),
    MulI(Box<IntegerArithmetic>),
    //DivI,
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
    //
}

impl Instruction {
    pub fn opcode(&self) -> Opcode {
        match self {
            Self::Nop => Opcode::Nop,
            Self::Break => Opcode::Break,
            Self::Ret(_) => Opcode::Ret,
            Self::AddI(_) => Opcode::AddI,
            Self::SubI(_) => Opcode::SubI,
            Self::MulI(_) => Opcode::MulI,
        }
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

//! Model of the SAILAR instruction set.

use crate::type_system;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ConstantInteger {
    U8(u8),
    S8(i8),
    U16(u16),
    S16(i16),
    U32(u32),
    S32(i32),
    U64(u64),
    S64(i64),
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

pub trait TypedValue {
    fn value_type(&self) -> type_system::Any; // Cow<'_, type_system::Any>
}

impl TypedValue for ConstantInteger {
    fn value_type(&self) -> type_system::Any {
        match self {
            Self::U8(_) => type_system::FixedInt::U8.into(),
            Self::S8(_) => type_system::FixedInt::S8.into(),
            Self::U16(_) => type_system::FixedInt::U16.into(),
            Self::S16(_) => type_system::FixedInt::S16.into(),
            Self::U32(_) => type_system::FixedInt::U32.into(),
            Self::S32(_) => type_system::FixedInt::S32.into(),
            Self::U64(_) => type_system::FixedInt::U64.into(),
            Self::S64(_) => type_system::FixedInt::S64.into(),
        }
    }
}

impl TypedValue for Constant {
    fn value_type(&self) -> type_system::Any {
        match self {
            Self::Integer(integer) => integer.value_type(),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[repr(u8)]
#[non_exhaustive]
pub enum OverflowBehavior {
    Ignore,
    /// Introduces an extra temporary register after the result register containing a boolean value indicating if an overflow
    /// occured.
    Flag,
    Saturate,
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

macro_rules! integer_conversion_impl {
    ($constant_case_name: ident, $integer_type: ty) => {
        crate::enum_case_from_impl!(ConstantInteger, $constant_case_name, $integer_type);

        impl From<$integer_type> for Constant {
            #[inline]
            fn from(value: $integer_type) -> Self {
                Constant::Integer(ConstantInteger::$constant_case_name(value))
            }
        }

        impl From<$integer_type> for Value {
            #[inline]
            fn from(value: $integer_type) -> Self {
                Value::Constant(Constant::from(value))
            }
        }
    };
}

integer_conversion_impl!(U8, u8);
integer_conversion_impl!(S8, i8);
integer_conversion_impl!(U16, u16);
integer_conversion_impl!(S16, i16);
integer_conversion_impl!(U32, u32);
integer_conversion_impl!(S32, i32);
integer_conversion_impl!(U64, u64);
integer_conversion_impl!(S64, i64);

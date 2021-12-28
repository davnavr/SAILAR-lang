use crate::format::{indices, numeric, structures::LengthEncodedVector, type_system};
use bitflags::bitflags;

pub use indices::Register as RegisterIndex;
pub use type_system::PrimitiveType;

/// Specifies the target of a branch instruction, pointing to the block containing the instructions that will be executed next
/// if the target branch is taken, with `0` refering to the current block.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, PartialOrd)]
pub struct BlockOffset(pub numeric::SInteger);

#[derive(Debug, Clone, Copy)]
pub enum RegisterType {
    Primitive(PrimitiveType),
    //Pointer(usize),
    //Object
}

#[derive(Debug, Clone, Copy)]
pub enum NumericType {
    Primitive(PrimitiveType),
    //Pointer(usize)
}

impl From<NumericType> for RegisterType {
    fn from(t: NumericType) -> Self {
        match t {
            NumericType::Primitive(primitive_type) => Self::Primitive(primitive_type),
        }
    }
}

/// Represents an integer constant, whose value is stored in little-endian order.
///
/// # Structure
/// - [`Opcode`]
/// - [`IntegerConstant::integer_type()`]
/// - [`IntegerConstant::value()`]
#[derive(Clone, Copy, Debug, Eq)]
pub enum IntegerConstant {
    U8(u8),
    S8(i8),
    U16(u16),
    S16(i16),
    U32(u32),
    S32(i32),
    U64(u64),
    S64(i64),
}

impl IntegerConstant {
    pub fn integer_type(self) -> PrimitiveType {
        match self {
            Self::U8(_) => PrimitiveType::U8,
            Self::S8(_) => PrimitiveType::S8,
            Self::U16(_) => PrimitiveType::U16,
            Self::S16(_) => PrimitiveType::S16,
            Self::U32(_) => PrimitiveType::U32,
            Self::S32(_) => PrimitiveType::S32,
            Self::U64(_) => PrimitiveType::U64,
            Self::S64(_) => PrimitiveType::S64,
        }
    }

    pub fn value(self) -> i128 {
        match self {
            Self::U8(value) => value.into(),
            Self::S8(value) => value.into(),
            Self::U16(value) => value.into(),
            Self::S16(value) => value.into(),
            Self::U32(value) => value.into(),
            Self::S32(value) => value.into(),
            Self::U64(value) => value.into(),
            Self::S64(value) => value.into(),
        }
    }
}

impl std::cmp::PartialEq for IntegerConstant {
    fn eq(&self, other: &Self) -> bool {
        self.value() == other.value()
    }
}

// See https://github.com/davnavr/ubyte/blob/c-like-language/src/UByte.Format/Model.fsi#L180
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd)]
#[repr(u32)]
pub enum Opcode {
    Nop = 0,
    Ret,
    Phi,
    Select,
    //Switch = 4,
    Br = 5,
    BrIf,
    Call,
    CallVirt,
    //CallIndr,
    Add = 10,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Not,
    Xor,
    Rem,
    //StoresBothDivisionResultAndRemainder,
    //ShiftIntegerBitsLeft,
    //ShiftIntegerBitsRight,
    Rotl = 22,
    Rotr,
    ConstI,
    ConstS,
    ConstF,
    /// Not an instruction, indicates that there are more opcode bytes to follow.
    Continuation = 0xFF,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum OverflowBehavior {
    Ignore = 0,
    /// Introduces an extra temporary register containing a boolean value indicating if an overflow occured.
    Flag,
    /// Indicates that program execution should immediately halt if an overflow occured.
    Halt,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DivideByZeroBehavior {
    /// Indicates that the value contained in the specified register should be returned if a division by zero occured.
    Return(RegisterIndex),
    Halt,
}

/// Represents an instruction consisting of an opcode and one or more operands.
///
/// For instructions that take a vector of registers, such as `ret` or `call`, the length of the vector is
/// included as usual to simplify parsing.
///
/// For instructions that call another method, such as `call` or `call.virt`, the number of registers used as
/// arguments must exactly match the number of arguments specified by the signature of the method. Additionally, the number
/// of temporary registers introduced is equal to the number of return values in the method's signature.
#[derive(Debug)]
pub enum Instruction {
    /// ```txt
    /// nop;
    /// ```
    /// Does absolutely nothing.
    Nop,
    /// ```txt
    /// ret <value1>, <value2>, ...;
    /// ```
    /// Returns the values in the specified registers and transfers control back to the calling method.
    ///
    /// Should be the last instruction in a block.
    Ret(LengthEncodedVector<RegisterIndex>),

    /// ```txt
    /// <result> = add <numeric type> <x> and <y>;
    /// <result> = add ovf.exit <numeric type> <x> and <y>;
    /// <result>, <overflowed> = add ovf.flag <numeric type> <x> and <y>;
    /// ```
    /// Returns the sum of the values in the `x` and `y` registers converted to the specified type.
    Add {
        overflow: OverflowBehavior,
        return_type: NumericType,
        x: RegisterIndex,
        y: RegisterIndex,
    },
    /// ```txt
    /// <result> = sub <numeric type> <x> from <y>;
    /// <result> = sub ovf.exit <numeric type> <x> from <y>;
    /// <result>, <overflowed> = sub ovf.flag <numeric type> <x> from <y>;
    /// ```
    /// Subtracts the value in the `x` register from the value in the `y` register converted to the specified type, and returns
    /// the difference.
    Sub {
        overflow: OverflowBehavior,
        return_type: NumericType,
        x: RegisterIndex,
        y: RegisterIndex,
    },
    /// ```txt
    /// <result> = mul <numeric type> <x> by <y>;
    /// <result> = mul ovf.exit <numeric type> <x> by <y>;
    /// <result>, <overflowed> = mul ovf.flag <numeric type> <x> by <y>;
    /// ```
    /// Returns the product of the values in the `x` and `y` registers converted to the specified type.
    Mul {
        overflow: OverflowBehavior,
        return_type: NumericType,
        x: RegisterIndex,
        y: RegisterIndex,
    },
    /// ```txt
    /// <result> = div <numeric type> <numerator> over <denominator> or <nan>;
    /// <result> = div zeroed.exit <numeric type> <numerator> over <denominator>;
    /// ```
    /// Returns the result of dividing the values in the `numerator` and `denominator` registers converted to the specified type.
    Div {
        divide_by_zero: DivideByZeroBehavior,
        return_type: NumericType,
        numerator: RegisterIndex,
        denominator: RegisterIndex,
    },

    /// ```txt
    /// <result> = const.i <integer type> <value>;
    /// ```
    /// Returns an integer of the specified type.
    ConstI(IntegerConstant), // TODO: Allow indicating if integer constant is of a pointer type?
}

impl Instruction {
    pub fn opcode(&self) -> Opcode {
        match self {
            Instruction::Nop => Opcode::Nop,
            Instruction::Ret(_) => Opcode::Ret,
            Instruction::Add { .. } => Opcode::Add,
            Instruction::Sub { .. } => Opcode::Sub,
            Instruction::Mul { .. } => Opcode::Mul,
            Instruction::Div { .. } => Opcode::Div,
            Instruction::ConstI(_) => Opcode::ConstI,
        }
    }

    /// The number of temporary registers introduced after execution of the instruction.
    pub fn return_count(&self) -> u8 {
        match self {
            Instruction::Nop | Instruction::Ret(_) => 0,
            Instruction::Add { overflow, .. }
            | Instruction::Sub { overflow, .. }
            | Instruction::Mul { overflow, .. } => match overflow {
                OverflowBehavior::Ignore | OverflowBehavior::Halt => 1,
                OverflowBehavior::Flag => 2,
            },
            Instruction::Div { .. } | Instruction::ConstI(_) => 1,
        }
    }
}

impl TryFrom<u32> for Opcode {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        if value < Self::ConstF as u32 {
            Ok(unsafe { std::mem::transmute(value) })
        } else {
            Err(())
        }
    }
}

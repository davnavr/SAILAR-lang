use crate::format::{indices, numeric, structures::LengthEncodedVector};

pub use crate::format::type_system::PrimitiveType;
pub use indices::Register as RegisterIndex;

/// Specifies the target of a branch instruction, pointing to the block containing the instructions that will be executed next
/// if the target branch is taken, with `0` refering to the current block.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, PartialOrd)]
pub struct BlockOffset(pub numeric::SInteger);

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
    /// <result> = const.i <integer type> <value>;
    /// ```
    /// Returns an integer of the specified type.
    ConstI(IntegerConstant),
}

impl Instruction {
    pub fn opcode(&self) -> Opcode {
        match self {
            Instruction::Nop => Opcode::Nop,
            Instruction::Ret(_) => Opcode::Ret,
            Instruction::ConstI(_) => Opcode::ConstI,
        }
    }

    /// The number of temporary registers introduced after execution of the instruction.
    pub fn return_count(&self) -> u8 {
        match self {
            Instruction::Nop | Instruction::Ret(_) => 0,
            Instruction::ConstI(_) => 1,
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

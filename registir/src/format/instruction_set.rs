use crate::format::{indices, type_system, LenVec};
use bitflags::bitflags;

pub use indices::{Function as FunctionIndex, Register as RegisterIndex};
pub use type_system::PrimitiveType;

/// Specifies the target of a branch instruction, pointing to the block containing the instructions that will be executed next
/// if the target branch is taken.
///
/// Note that branch instructions and exception handlers cannot transfer control to an entry block.
pub type JumpTarget = indices::CodeBlock;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RegisterType {
    Primitive(PrimitiveType),
    //Pointer(u32),
    //Object
}

impl std::fmt::Display for RegisterType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Primitive(primitive_type) => primitive_type.fmt(f),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum NumericType {
    Primitive(PrimitiveType),
    //Pointer(u32)
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
    Switch,
    Br,
    BrIf,
    Call,
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Not,
    Xor,
    Rem,
    Mod,
    StoresBothDivisionResultAndRemainder,
    StoresBothModuloAndRemainder,
    ShL,
    ShR,
    RotL,
    RotR,
    ConstI,
    ConstF,
    Break = 254,
    /// Not an instruction, indicates that there are more opcode bytes to follow.
    Continuation = 0xFF,
}

bitflags! {
    #[repr(transparent)]
    pub struct ArithmeticFlags: u8 {
        const NONE = 0;
        const HALT_ON_OVERFLOW = 0b0000_0001;
        const FLAG_ON_OVERFLOW = 0b0000_0010;
        const RETURN_VALUE_ON_DIVIDE_BY_ZERO = 0b0000_0100;
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum OverflowBehavior {
    Ignore,
    /// Indicates that program execution should immediately halt if an overflow occured.
    Halt,
    /// Introduces an extra temporary register containing a boolean value indicating if an overflow occured.
    Flag,
}

impl OverflowBehavior {
    pub fn flags(self) -> ArithmeticFlags {
        match self {
            Self::Ignore => ArithmeticFlags::NONE,
            Self::Halt => ArithmeticFlags::HALT_ON_OVERFLOW,
            Self::Flag => ArithmeticFlags::FLAG_ON_OVERFLOW,
        }
    }
}

impl From<ArithmeticFlags> for OverflowBehavior {
    fn from(flags: ArithmeticFlags) -> Self {
        if flags.contains(ArithmeticFlags::HALT_ON_OVERFLOW) {
            Self::Halt
        } else if flags.contains(ArithmeticFlags::FLAG_ON_OVERFLOW) {
            Self::Flag
        } else {
            Self::Ignore
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DivideByZeroBehavior {
    /// Indicates that the value contained in the specified register should be returned if a division by zero occured.
    Return(RegisterIndex),
    Halt,
}

impl DivideByZeroBehavior {
    pub fn flags(self) -> ArithmeticFlags {
        match self {
            Self::Return(_) => ArithmeticFlags::RETURN_VALUE_ON_DIVIDE_BY_ZERO,
            Self::Halt => ArithmeticFlags::NONE,
        }
    }
}

#[derive(Debug)]
pub struct BasicArithmeticOperation {
    pub overflow: OverflowBehavior,
    pub return_type: NumericType,
    pub x: RegisterIndex,
    pub y: RegisterIndex,
}

impl BasicArithmeticOperation {
    pub fn flags(&self) -> ArithmeticFlags {
        self.overflow.flags()
    }
}

/// # Structure
/// - [`DivisionOperation::flags()`]
/// - nan index
/// - [`return_type`]
/// - [`numerator`]
/// - [`denominator`]
#[derive(Debug)]
pub struct DivisionOperation {
    pub overflow: OverflowBehavior,
    pub divide_by_zero: DivideByZeroBehavior,
    pub return_type: NumericType,
    pub numerator: RegisterIndex,
    pub denominator: RegisterIndex,
}

impl DivisionOperation {
    pub fn flags(&self) -> ArithmeticFlags {
        self.overflow.flags().union(self.divide_by_zero.flags())
    }
}

#[derive(Debug)]
pub struct BitwiseOperation {
    pub result_type: NumericType,
    pub x: RegisterIndex,
    pub y: RegisterIndex,
}

/// Describes a bitwise operation that results in the shifting of a value.
/// # Structure
/// - [`BitwiseShiftOperation::result_type()`]
/// - [`BitwiseShiftOperation::value()`]
/// - [`BitwiseShiftOperation::amount()`]
#[derive(Debug)]
#[repr(transparent)]
pub struct BitwiseShiftOperation(pub BitwiseOperation);

impl BitwiseShiftOperation {
    pub fn new(result_type: NumericType, value: RegisterIndex, amount: RegisterIndex) -> Self {
        Self(BitwiseOperation {
            result_type,
            x: value,
            y: amount,
        })
    }

    pub fn result_type(&self) -> &NumericType {
        &self.0.result_type
    }

    pub fn value(&self) -> &RegisterIndex {
        &self.0.x
    }

    pub fn amount(&self) -> &RegisterIndex {
        &self.0.y
    }
}

bitflags! {
    #[repr(transparent)]
    pub struct CallFlags: u8 {
        const NONE = 0;
        const TAIL_CALL_REQUIRED = 0b0000_0001;
        const TAIL_CALL_PROHIBITED = 0b0000_0010;
        const TAIL_CALL_MASK = 0b0000_0011;
        /// Applicable to the `call.virt` instruction, halts execution if the `this` argument is `null`.
        const HALT_ON_NULL_THIS = 0b0000_0100;
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TailCall {
    Allowed,
    Required,
    Prohibited,
}

impl TailCall {
    pub fn flags(self) -> CallFlags {
        match self {
            Self::Allowed => CallFlags::NONE,
            Self::Required => CallFlags::TAIL_CALL_REQUIRED,
            Self::Prohibited => CallFlags::TAIL_CALL_PROHIBITED,
        }
    }
}

/// # Structure
/// - [`CallInstruction::flags()`]
/// - [`function`]
/// - [`arguments`]
#[derive(Debug)]
pub struct CallInstruction {
    pub tail_call: TailCall,
    pub function: FunctionIndex,
    pub arguments: LenVec<RegisterIndex>,
}

impl CallInstruction {
    pub fn flags(&self) -> CallFlags {
        self.tail_call.flags()
    }
}

/// Represents an instruction consisting of an opcode and one or more operands.
///
/// For instructions that take a vector of registers, such as `ret` or `call`, the length of the vector is
/// included as usual to simplify parsing.
///
/// Floating-point numbers used in bitwise instructions such as [`And`] or [`Xor`] are simply reinterpreted as values of their
/// corresponding unsigned integer counterparts, (e.g. an `f32` is reinpterpreted as a `u32`).
///
/// TODO: Have not about conversions, maybe round towards 0 for float to signed int conversions?
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
    /// Returns the values in the specified registers and transfers control back to the calling function.
    ///
    /// Should be the last instruction in a block.
    Ret(LenVec<RegisterIndex>),
    /// ```txt
    /// br <target>;
    /// br <target> with <input1>, <input2>, ...;
    /// ```
    /// Unconditionally transfers control flow to the `target` block, with the specified `input` values.
    Br(JumpTarget, LenVec<RegisterIndex>),
    /// ```txt
    /// br.if <condition> then <true> else <false>;
    /// br.if <condition> then <true> else <false> with <input1>, <input2>, ...;
    /// ```
    /// If the value in the `condition` register is truthy (not equal to zero), transfers control flow to the `true` block;
    /// otherwise, control flow is transferred to the `false` block.
    BrIf {
        condition: RegisterIndex,
        true_branch: JumpTarget,
        false_branch: JumpTarget,
        input_registers: LenVec<RegisterIndex>,
    },
    /// ```txt
    /// <result0>, <result1>, ... = call [tail.prohibited | tail.required] <function> <argument0>, <argument1>, ...;
    /// ```
    /// Calls the specified `function`, supplying the values in the arguments registers as inputs to its entry block.
    ///
    /// The number of registers used as arguments must exactly match the number of arguments specified by the signature of the
    /// function. Additionally, the number of temporary registers introduced is equal to the number of return values in the
    /// function's signature.
    Call(CallInstruction),

    /// ```txt
    /// <sum> = add <numeric type> <x> and <y>;
    /// <sum> = add <numeric type> <x> and <y> ovf.halt;
    /// <sum>, <overflowed> = add <numeric type> <x> and <y> ovf.flag;
    /// ```
    /// Returns the sum of the values in the `x` and `y` registers converted to the specified type.
    Add(BasicArithmeticOperation),
    /// ```txt
    /// <result> = sub <numeric type> <x> from <y>;
    /// <result> = sub <numeric type> <x> from <y> ovf.halt;
    /// <result>, <overflowed> = sub <numeric type> <x> from <y> ovf.flag;
    /// ```
    /// Subtracts the value in the `x` register from the value in the `y` register converted to the specified type, and returns
    /// the difference.
    Sub(BasicArithmeticOperation),
    /// ```txt
    /// <product> = mul <numeric type> <x> by <y>;
    /// <product> = mul <numeric type> <x> by <y> ovf.halt;
    /// <product>, <overflowed> = mul <numeric type> <x> by <y> ovf.flag;
    /// ```
    /// Returns the product of the values in the `x` and `y` registers converted to the specified type.
    Mul(BasicArithmeticOperation),
    /// ```txt
    /// <quotient> = div <numeric type> <numerator> over <denominator> or <nan>;
    /// <quotient> = div <numeric type> <numerator> over <denominator> or <nan> ovf.halt;
    /// <quotient>, <overflowed> = div <numeric type> <numerator> over <denominator> or <nan> ovf.flag;
    /// <quotient> = div <numeric type> <numerator> over <denominator> zeroed.halt;
    /// <quotient> = div <numeric type> <numerator> over <denominator> zeroed.halt ovf.halt;
    /// <quotient>, <overflowed> = div <numeric type> <numerator> over <denominator> zeroed.halt ovf.flag;
    /// ```
    /// Returns the result of dividing the values in the `numerator` and `denominator` registers converted to the specified type.
    Div(DivisionOperation),
    /// ```txt
    /// <result> = and <numeric type> <x> <y>;
    /// ```
    /// Returns the bitwise `AND` of the values in the `x` and `y` registers converted to the specified numeric type.
    And(BitwiseOperation),
    /// ```txt
    /// <result> = or <numeric type> <x> <y>;
    /// ```
    /// Returns the bitwise `OR` of the values in the `x` and `y` registers converted to the specified numeric type.
    Or(BitwiseOperation),
    /// ```txt
    /// <result> = not <numeric type> <value>;
    /// ```
    /// Returns the bitwise `NOT` of the value in the specified register converted to the specified numeric type.
    Not(NumericType, RegisterIndex),
    /// ```txt
    /// <result> = xor <numeric type> <x> <y>;
    /// ```
    /// Returns the bitwise `XOR` of the values in the `x` and `y` registers converted to the specified numeric type.
    Xor(BitwiseOperation),

    /// ```txt
    /// <result> = sh.l <numeric type> <value> by <amount>;
    /// ```
    /// Shifts the value in the `value` register converted to the specified integer type to the left by the amount in the
    /// `amount` register.
    ShL(BitwiseShiftOperation),
    /// ```txt
    /// <result> = sh.r <numeric type> <value> by <amount>;
    /// ```
    /// Shifts the value in the `value` register converted to the specified integer type to the right by the amount in the
    /// `amount` register, inserting a `0` bit if the numeric type is an unsigned integer type, or copying the sign bit if the
    /// type is a signed integer type.
    ShR(BitwiseShiftOperation),
    /// ```txt
    /// <result> = rot.l <numeric type> <value> by <amount>;
    /// ```
    /// Rotates the value in the specified `value` register converted to the specified numeric type left by the amount in the
    /// `amount` register.
    RotL(BitwiseShiftOperation),
    /// ```txt
    /// <result> = rot.r <numeric type> <value> by <amount>;
    /// ```
    /// Rotates the value in the specified `value` register converted to the specified numeric type right by the amount in the
    /// `amount` register.
    RotR(BitwiseShiftOperation),

    /// ```txt
    /// <result> = const.i <integer type> <value>;
    /// ```
    /// Returns an integer of the specified type.
    ConstI(IntegerConstant), // TODO: Allow indicating if integer constant is of a pointer type?
    /// ```txt
    /// break;
    /// ```
    /// Represents a breakpoint placed by a debugger.
    ///
    /// If no debugger is attached, or if a debugger is not supported, this instruction does nothing.
    Break,
}

impl Instruction {
    pub fn opcode(&self) -> Opcode {
        match self {
            Instruction::Nop => Opcode::Nop,
            Instruction::Ret(_) => Opcode::Ret,
            Instruction::Br(_, _) => Opcode::Br,
            Instruction::BrIf { .. } => Opcode::BrIf,
            Instruction::Call(_) => Opcode::Call,
            Instruction::Add(_) => Opcode::Add,
            Instruction::Sub(_) => Opcode::Sub,
            Instruction::Mul(_) => Opcode::Mul,
            Instruction::Div(_) => Opcode::Div,
            Instruction::And(_) => Opcode::And,
            Instruction::Or(_) => Opcode::Or,
            Instruction::Not { .. } => Opcode::Not,
            Instruction::Xor(_) => Opcode::Xor,
            Instruction::ShL(_) => Opcode::ShL,
            Instruction::ShR(_) => Opcode::ShR,
            Instruction::RotL(_) => Opcode::RotL,
            Instruction::RotR(_) => Opcode::RotR,
            Instruction::ConstI(_) => Opcode::ConstI,
            Instruction::Break => Opcode::Break,
        }
    }

    /// Calculates the number of temporary registers introduced after execution of the instruction.
    pub fn return_count<R: FnOnce(FunctionIndex) -> u8>(&self, function_return_count: R) -> u8 {
        match self {
            Instruction::Nop
            | Instruction::Ret(_)
            | Instruction::Br(_, _)
            | Instruction::BrIf { .. }
            | Instruction::Break => 0,
            Instruction::Call(CallInstruction { function, .. }) => function_return_count(*function),
            Instruction::Add(BasicArithmeticOperation { overflow, .. })
            | Instruction::Sub(BasicArithmeticOperation { overflow, .. })
            | Instruction::Mul(BasicArithmeticOperation { overflow, .. })
            | Instruction::Div(DivisionOperation { overflow, .. }) => match overflow {
                OverflowBehavior::Ignore | OverflowBehavior::Halt => 1,
                OverflowBehavior::Flag => 2,
            },
            Instruction::And(_)
            | Instruction::Or(_)
            | Instruction::Not { .. }
            | Instruction::Xor(_)
            | Instruction::ShL(_)
            | Instruction::ShR(_)
            | Instruction::RotL(_)
            | Instruction::RotR(_)
            | Instruction::ConstI(_) => 1,
        }
    }
}

impl TryFrom<u32> for Opcode {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        if value < Self::Continuation as u32 {
            Ok(unsafe { std::mem::transmute(value) })
        } else {
            Err(())
        }
    }
}

impl TryFrom<u8> for OverflowBehavior {
    type Error = ();

    fn try_from(tag: u8) -> Result<Self, Self::Error> {
        match tag {
            0 => Ok(OverflowBehavior::Ignore),
            1 => Ok(OverflowBehavior::Halt),
            2 => Ok(OverflowBehavior::Flag),
            _ => Err(()),
        }
    }
}

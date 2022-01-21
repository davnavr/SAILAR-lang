use crate::{
    format::{indices, type_system, LenVec},
    hashing::IntegerHashBuilder,
};
use bitflags::bitflags;
use std::collections::hash_map;

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

impl std::hash::Hash for IntegerConstant {
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        state.write_i128(self.value())
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
    Add = 16,
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
        const FLAG_ON_OVERFLOW = 0b0000_0001;
        const RETURN_VALUE_ON_DIVIDE_BY_ZERO = 0b0000_0100;
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum OverflowBehavior {
    Ignore,
    /// Introduces an extra temporary register containing a boolean value indicating if an overflow occured.
    Flag,
}

impl OverflowBehavior {
    pub fn flags(self) -> ArithmeticFlags {
        match self {
            Self::Ignore => ArithmeticFlags::NONE,
            Self::Flag => ArithmeticFlags::FLAG_ON_OVERFLOW,
        }
    }
}

impl From<ArithmeticFlags> for OverflowBehavior {
    fn from(flags: ArithmeticFlags) -> Self {
        if flags.contains(ArithmeticFlags::FLAG_ON_OVERFLOW) {
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

#[derive(Debug, PartialEq)]
pub struct BasicArithmeticOperation {
    pub overflow: OverflowBehavior,
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
#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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

/// # Structure
/// - [`function`]
/// - [`arguments`]
#[derive(Debug, PartialEq)]
pub struct CallInstruction {
    pub function: FunctionIndex,
    pub arguments: LenVec<RegisterIndex>,
}

/// Maps jump targets to the values returned by a `phi` instruction.Instruction
///
/// # Structure
/// - [`value_count()`] (uinteger)
/// - [`entry_count()`]
/// - block0
/// - value0
/// - value1
/// - ...
/// - block1
/// - value2
/// - value3
/// - ...
#[derive(Debug, PartialEq)]
pub struct PhiSelectionLookup {
    lookup: hash_map::HashMap<JumpTarget, Vec<RegisterIndex>, IntegerHashBuilder>,
    value_count: u8,
}

impl PhiSelectionLookup {
    pub fn with_capacity(value_count: u8, capacity: usize) -> Self {
        Self {
            lookup: hash_map::HashMap::with_capacity_and_hasher(
                capacity,
                IntegerHashBuilder::default(),
            ),
            value_count,
        }
    }

    /// Inserts an entry into the map.
    ///
    /// # Panics
    /// Panics if the number of registers is not equal to the `value_count`.
    #[must_use]
    pub fn insert(&mut self, target: JumpTarget, registers: Vec<RegisterIndex>) -> bool {
        if usize::from(self.value_count) != registers.len() {
            panic!(
                "expected {} values but got {}",
                self.value_count,
                registers.len()
            );
        }
        self.lookup.insert(target, registers).is_none()
    }

    pub fn iter(&self) -> impl std::iter::Iterator<Item = (JumpTarget, &'_ [RegisterIndex])> + '_ {
        self.lookup
            .iter()
            .map(|(target, registers)| (*target, registers.as_slice()))
    }

    pub fn get(&self, target: JumpTarget) -> Option<&[RegisterIndex]> {
        self.lookup.get(&target).map(Vec::as_slice)
    }

    pub fn entry_count(&self) -> usize {
        self.lookup.len()
    }

    pub fn value_count(&self) -> u8 {
        self.value_count
    }
}

/// Specifies the targets of a `switch` instruction.
///
/// # Structure
/// - [`len()`]
/// - value0
/// - target0
/// - value1
/// - target1
/// - ...
#[derive(Debug, PartialEq)]
pub struct SwitchLookupTable {
    lookup: hash_map::HashMap<IntegerConstant, JumpTarget, IntegerHashBuilder>,
}

impl SwitchLookupTable {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            lookup: hash_map::HashMap::with_capacity_and_hasher(
                capacity,
                IntegerHashBuilder::default(),
            ),
        }
    }

    #[must_use]
    pub fn insert(&mut self, value: IntegerConstant, target: JumpTarget) -> bool {
        match self.lookup.entry(value) {
            hash_map::Entry::Vacant(vacant) => {
                vacant.insert(target);
                true
            }
            hash_map::Entry::Occupied(_) => false,
        }
    }

    pub fn get(&self, value: &IntegerConstant) -> Option<JumpTarget> {
        self.lookup.get(value).copied()
    }

    pub fn len(&self) -> usize {
        self.lookup.len()
    }

    pub fn is_empty(&self) -> bool {
        self.lookup.is_empty()
    }

    pub fn iter(&self) -> impl std::iter::Iterator<Item = (&'_ IntegerConstant, JumpTarget)> + '_ {
        self.lookup.iter().map(|(value, target)| (value, *target))
    }
}

/// Represents an instruction consisting of an opcode and one or more operands.
#[derive(Debug, PartialEq)]
pub enum Instruction {
    /// ```txt
    /// nop;
    /// ```
    /// Does absolutely nothing.
    Nop,
    /// ```txt
    /// ret <value0>, <value1>, ...;
    /// ```
    /// Returns the values in the specified registers and transfers control back to the calling function.
    ///
    /// # Requirements
    /// A `ret` instruction should be the last instruction in a block.
    Ret(LenVec<RegisterIndex>),
    /// ```txt
    /// <result0>, <result1>, ... = phi <value0>, <value1>, ... when <block0> or <value2>, <value3>, ... when <block1> or ...;
    /// ```
    /// Selects values based on the previous block.
    ///
    /// # Requirements
    /// A `phi` instruction cannot appear in the entry block. Additionally, an entry must exist for all blocks that can transfer
    /// control to the block containing the instruction.
    Phi(PhiSelectionLookup),
    ///// ```txt
    ///// <result0>, <result1>, ... = select <condition> then <value0>, <value1>, ... else <value2>, <value3>, ...;
    ///// ```
    ///// Used to select one set of values or the other depending on whether the value in the `condition` register is truthy,
    ///// without requires blocks or branching.
    //Select {
    //    condition: RegisterIndex,
    //    true_registers: LenVec<RegisterIndex>, // NOTE: Length of vectors should be the same anyway, could optimize away length.
    //    false_registers: Vec<RegisterIndex>,
    //},
    /// ```txt
    /// switch <cmptype> <comparison> default <target0> or <value1> <target1> or <value2> <target2> or ...;
    /// ```
    /// Transfers control to one of several blocks depending on the value in the `comparison` register.
    ///
    /// # Requirements
    /// The type of the value in the `comparison` register must be a non-native integer type, and must be the same as `cmptype`.
    Switch {
        comparison: RegisterIndex,
        // TODO: Use enum for integer type.
        comparison_type: PrimitiveType,
        default_target: JumpTarget,
        target_lookup: SwitchLookupTable,
    },
    /// ```txt
    /// br <target>;
    /// br <target> with <input0>, <input1>, ...;
    /// ```
    /// Unconditionally transfers control flow to the `target` block providing the specified `input` values.
    Br {
        target: JumpTarget,
        input_registers: LenVec<RegisterIndex>,
    },
    /// ```txt
    /// br.if <condition> then <true> else <false>;
    /// br.if <condition> then <true> else <false> with <input0>, <input1>, ...;
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
    /// <result0>, <result1>, ... = call <function> <argument0>, <argument1>, ...;
    /// ```
    /// Calls the specified `function`, supplying the values in the arguments registers as inputs to its entry block.
    ///
    /// # Requirements
    /// The number of registers used as arguments must exactly match the number of arguments specified by the signature of the
    /// function. Additionally, the number of temporary registers introduced is equal to the number of return values in the
    /// function's signature.
    Call(CallInstruction),
    //CallIndr
    //CallRet
    /// ```txt
    /// <sum> = add <x> to <y>;
    /// <sum>, <overflowed> = add <x> to <y> ovf.flag;
    /// ```
    /// Returns the sum of the values in the `x` and `y` registers.
    Add(BasicArithmeticOperation),
    /// ```txt
    /// <result> = sub <x> from <y>;
    /// <result>, <overflowed> = sub <x> from <y> ovf.flag;
    /// ```
    /// Subtracts the value in the `x` register from the value in the `y` register, and returns
    /// the difference.
    Sub(BasicArithmeticOperation),
    /// ```txt
    /// <product> = mul <x> by <y>;
    /// <product>, <overflowed> = mul <x> by <y> ovf.flag;
    /// ```
    /// Returns the product of the values in the `x` and `y` registers.
    Mul(BasicArithmeticOperation),
    // /// ```txt
    // /// <quotient> = div <numeric type> <numerator> over <denominator> or <nan>;
    // /// <quotient> = div <numeric type> <numerator> over <denominator> or <nan> ovf.halt;
    // /// <quotient>, <overflowed> = div <numeric type> <numerator> over <denominator> or <nan> ovf.flag;
    // /// <quotient> = div <numeric type> <numerator> over <denominator> zeroed.halt;
    // /// <quotient> = div <numeric type> <numerator> over <denominator> zeroed.halt ovf.halt;
    // /// <quotient>, <overflowed> = div <numeric type> <numerator> over <denominator> zeroed.halt ovf.flag;
    // /// ```
    // /// Returns the result of dividing the values in the `numerator` and `denominator` registers converted to the specified type.
    // Div(DivisionOperation),
    // /// ```txt
    // /// <result> = and <numeric type> <x> <y>;
    // /// ```
    // /// Returns the bitwise `AND` of the values in the `x` and `y` registers converted to the specified numeric type.
    // And(BitwiseOperation),
    // /// ```txt
    // /// <result> = or <numeric type> <x> <y>;
    // /// ```
    // /// Returns the bitwise `OR` of the values in the `x` and `y` registers converted to the specified numeric type.
    // Or(BitwiseOperation),
    // /// ```txt
    // /// <result> = not <numeric type> <value>;
    // /// ```
    // /// Returns the bitwise `NOT` of the value in the specified register converted to the specified numeric type.
    // Not(NumericType, RegisterIndex),
    // /// ```txt
    // /// <result> = xor <numeric type> <x> <y>;
    // /// ```
    // /// Returns the bitwise `XOR` of the values in the `x` and `y` registers converted to the specified numeric type.
    // Xor(BitwiseOperation),

    // /// ```txt
    // /// <result> = sh.l <numeric type> <value> by <amount>;
    // /// ```
    // /// Shifts the value in the `value` register converted to the specified integer type to the left by the amount in the
    // /// `amount` register.
    // ShL(BitwiseShiftOperation),
    // /// ```txt
    // /// <result> = sh.r <numeric type> <value> by <amount>;
    // /// ```
    // /// Shifts the value in the `value` register converted to the specified integer type to the right by the amount in the
    // /// `amount` register, inserting a `0` bit if the numeric type is an unsigned integer type, or copying the sign bit if the
    // /// type is a signed integer type.
    // ShR(BitwiseShiftOperation),
    // /// ```txt
    // /// <result> = rot.l <numeric type> <value> by <amount>;
    // /// ```
    // /// Rotates the value in the specified `value` register converted to the specified numeric type left by the amount in the
    // /// `amount` register.
    // RotL(BitwiseShiftOperation),
    // /// ```txt
    // /// <result> = rot.r <numeric type> <value> by <amount>;
    // /// ```
    // /// Rotates the value in the specified `value` register converted to the specified numeric type right by the amount in the
    // /// `amount` register.
    // RotR(BitwiseShiftOperation),
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
            Instruction::Phi(_) => Opcode::Phi,
            Instruction::Switch { .. } => Opcode::Switch,
            Instruction::Br { .. } => Opcode::Br,
            Instruction::BrIf { .. } => Opcode::BrIf,
            Instruction::Call(_) => Opcode::Call,
            Instruction::Add(_) => Opcode::Add,
            Instruction::Sub(_) => Opcode::Sub,
            Instruction::Mul(_) => Opcode::Mul,
            // Instruction::Div(_) => Opcode::Div,
            // Instruction::And(_) => Opcode::And,
            // Instruction::Or(_) => Opcode::Or,
            // Instruction::Not { .. } => Opcode::Not,
            // Instruction::Xor(_) => Opcode::Xor,
            // Instruction::ShL(_) => Opcode::ShL,
            // Instruction::ShR(_) => Opcode::ShR,
            // Instruction::RotL(_) => Opcode::RotL,
            // Instruction::RotR(_) => Opcode::RotR,
            Instruction::ConstI(_) => Opcode::ConstI,
            Instruction::Break => Opcode::Break,
        }
    }

    /// Calculates the number of temporary registers introduced after execution of the instruction.
    pub fn return_count<R: FnOnce(FunctionIndex) -> u8>(&self, function_return_count: R) -> u8 {
        match self {
            Instruction::Nop
            | Instruction::Ret(_)
            | Instruction::Switch { .. }
            | Instruction::Br { .. }
            | Instruction::BrIf { .. }
            | Instruction::Break => 0,
            Instruction::Phi(lookup) => lookup.value_count,
            Instruction::Call(CallInstruction { function, .. }) => function_return_count(*function),
            Instruction::Add(BasicArithmeticOperation { overflow, .. })
            | Instruction::Sub(BasicArithmeticOperation { overflow, .. })
            | Instruction::Mul(BasicArithmeticOperation { overflow, .. })
            //| Instruction::Div(DivisionOperation { overflow, .. })
            => match overflow {
                OverflowBehavior::Ignore => 1,
                OverflowBehavior::Flag => 2,
            },
            // Instruction::And(_)
            // | Instruction::Or(_)
            // | Instruction::Not { .. }
            // | Instruction::Xor(_)
            // | Instruction::ShL(_)
            // | Instruction::ShR(_)
            // | Instruction::RotL(_)
            // | Instruction::RotR(_)
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

//! Representation of the SAILAR instruction set encoding.

use crate::index;
use crate::signature;
use std::borrow::{Borrow, BorrowMut};
use std::fmt::{Debug, Display, Formatter};

/// Represents a constant integer value stored in little-endian order. Whether or not the value is signed is inferred from
/// context.
#[derive(Copy, Clone, Eq, Hash, PartialEq)]
pub enum ConstantInteger {
    I8(u8),
    I16([u8; 2]),
    I32([u8; 4]),
    I64([u8; 8]),
}

impl ConstantInteger {
    pub fn bit_size(self) -> signature::IntegerSize {
        match self {
            Self::I8(_) => signature::IntegerSize::I8,
            Self::I16(_) => signature::IntegerSize::I16,
            Self::I32(_) => signature::IntegerSize::I32,
            Self::I64(_) => signature::IntegerSize::I64,
        }
    }

    pub fn bytes(&self) -> &[u8] {
        self.borrow()
    }

    fn to_comparable_value(self) -> impl std::cmp::Ord {
        match self {
            Self::I8(byte) => u64::from(byte),
            Self::I16(bytes) => u64::from(u16::from_le_bytes(bytes)),
            Self::I32(bytes) => u64::from(u32::from_le_bytes(bytes)),
            Self::I64(bytes) => u64::from_le_bytes(bytes),
        }
    }
}

impl std::cmp::Ord for ConstantInteger {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.to_comparable_value().cmp(&other.to_comparable_value())
    }
}

impl std::cmp::PartialOrd for ConstantInteger {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(std::cmp::Ord::cmp(&self, &other))
    }
}

impl std::ops::Deref for ConstantInteger {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.borrow()
    }
}

impl Borrow<[u8]> for ConstantInteger {
    fn borrow(&self) -> &[u8] {
        match self {
            Self::I8(b) => std::slice::from_ref(b),
            Self::I16(s) => s.as_slice(),
            Self::I32(i) => i.as_slice(),
            Self::I64(l) => l.as_slice(),
        }
    }
}

impl BorrowMut<[u8]> for ConstantInteger {
    fn borrow_mut(&mut self) -> &mut [u8] {
        match self {
            Self::I8(b) => std::slice::from_mut(b),
            Self::I16(s) => s.as_mut_slice(),
            Self::I32(i) => i.as_mut_slice(),
            Self::I64(l) => l.as_mut_slice(),
        }
    }
}

impl AsRef<[u8]> for ConstantInteger {
    fn as_ref(&self) -> &[u8] {
        self.borrow()
    }
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

impl Display for ConstantInteger {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::I8(value) => write!(f, "{:#02X}", value),
            Self::I16(value) => write!(f, "{:#04X}", u16::from_le_bytes(*value)),
            Self::I32(value) => write!(f, "{:#08X}", u32::from_le_bytes(*value)),
            Self::I64(value) => write!(f, "{:#016X}", u64::from_le_bytes(*value)),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Constant {
    Integer(ConstantInteger),
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Integer(integer) => Display::fmt(integer, f),
        }
    }
}

crate::enum_case_from_impl!(Constant, Integer, ConstantInteger);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Value {
    Constant(Constant),
    IndexedRegister(index::Register),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Constant(constant) => Display::fmt(constant, f),
            Self::IndexedRegister(register) => write!(f, "#{}", usize::from(*register)),
        }
    }
}

impl From<index::Register> for Value {
    #[inline]
    fn from(register: index::Register) -> Self {
        Self::IndexedRegister(register)
    }
}

crate::enum_case_from_impl!(Value, Constant, Constant);

impl From<ConstantInteger> for Value {
    fn from(value: ConstantInteger) -> Self {
        Constant::from(value).into()
    }
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
        const IS_REGISTER = 0;
        const IS_CONSTANT = 0b0000_0001;
        const IS_INTEGER = 0b0000_0010;
        const INTEGER_SIZE_MASK = 0b0000_1100;
        const INTEGER_SIZE_1 = 0;
        const INTEGER_SIZE_2 = 0b0000_0100;
        const INTEGER_SIZE_4 = 0b0000_1000;
        const INTEGER_SIZE_8 = 0b0000_1100;
        const INTEGER_IS_EMBEDDED = 0b0001_0000;
        const INTEGER_EMBEDDED_ONE = 0b0010_0000;
    }
}

impl Value {
    /// Gets a flags value describing this value. If the value contains an embedded integer vlaue of `0` or `1`, the flags
    /// contain the embedded value.
    pub fn flags(&self) -> ValueFlags {
        match self {
            Self::IndexedRegister(_) => ValueFlags::IS_REGISTER,
            Self::Constant(Constant::Integer(integer)) => {
                let mut integer_flags = ValueFlags::IS_CONSTANT | ValueFlags::IS_INTEGER;

                macro_rules! unsigned_integer_flag {
                    ($value: expr, $size: ident) => {{
                        integer_flags |= ValueFlags::$size;

                        match $value {
                            0 => integer_flags |= ValueFlags::INTEGER_IS_EMBEDDED,
                            1 => integer_flags |= ValueFlags::INTEGER_IS_EMBEDDED | ValueFlags::INTEGER_EMBEDDED_ONE,
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

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct ConditionalBranch {
    pub condition: Value,
    pub true_branch: BranchTarget,
    pub false_branch: BranchTarget,
}

impl ConditionalBranch {
    pub fn new(condition: Value, true_branch: BranchTarget, false_branch: BranchTarget) -> Self {
        Self {
            condition,
            true_branch,
            false_branch,
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct BranchTarget {
    pub target: index::CodeBlock,
    pub arguments: Box<[Value]>,
}

impl BranchTarget {
    pub fn new<A: Into<Box<[Value]>>>(target: index::CodeBlock, arguments: A) -> Self {
        Self {
            target,
            arguments: arguments.into(),
        }
    }
}

/// Represents the possible targets of a `switch lookup` instruction.
///
/// See the documentation for [`SwitchLookup`] for more information.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Default)]
pub struct SwitchLookupBranches {
    branches: std::collections::BTreeMap<ConstantInteger, BranchTarget>,
}

impl SwitchLookupBranches {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn len(&self) -> usize {
        self.branches.len()
    }

    pub fn try_insert(&mut self, value: ConstantInteger, target: BranchTarget) -> Result<(), index::CodeBlock> {
        match self.branches.insert(value, target) {
            None => Ok(()),
            Some(existing) => Err(existing.target),
        }
    }

    pub fn iter(&self) -> impl std::iter::ExactSizeIterator<Item = (&ConstantInteger, &BranchTarget)> {
        self.branches.iter()
    }
}

/// Describes a `switch lookup` instruction.
///
/// See the documentation for [`Instruction::SwitchLookup`] for more information.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct SwitchLookup {
    pub comparand_type: index::TypeSignature,
    pub comparand: Value,
    pub default_branch: index::CodeBlock,
    pub branches: SwitchLookupBranches,
}

/// Describes a `switch table` instruction.
///
/// See the documentation for [`Instruction::SwitchTable`] for more information.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct SwitchTable {
    pub index: Value,
    pub default_branch: index::CodeBlock,
    pub branches: Box<[BranchTarget]>,
}

/// Describes a `select` instruction.
///
/// See the documentation for [`Instruction::Select`] for more information.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct Selection {
    pub condition: Value,
    pub true_value: Value,
    pub false_value: Value,
}

impl Selection {
    pub fn new(condition: Value, true_value: Value, false_value: Value) -> Self {
        Self {
            condition,
            true_value,
            false_value,
        }
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
    /// Does absolutely nothing.
    ///
    /// ### Assembly Syntax
    /// ```text
    /// nop
    /// ```
    Nop = 0,
    /// Indicates that control flow cannot reach this particular location. Behavior is undefined if this instruction is actually
    /// reached somehow.
    ///
    /// ### Assembly Syntax
    /// ```text
    /// unreachable
    /// ```
    Unreachable = 1,
    /// Transfers control flow back to the calling function, providing the specified return value(s).
    ///
    /// ### Assembly Syntax
    /// ```text
    /// return <value0>, <value1>, ... ; Return multiple values
    /// return ; Return no values
    /// ```
    Return(_values: Box<[Value]>,) = 2,
    /// Given an integer value `comparand`, performs a lookup to determine where to transfer control to.
    ///
    /// Note that the comparison values are sorted in increasing order.
    ///
    /// ### Implementation
    /// The target to jump to can be easily determined using a binary search, taking `O(log n)` time.
    ///
    /// ### Assembly Syntax
    /// ```text
    /// switch lookup <comparand_type> <comparand> <value0> <target0>, <value1> <target1>, ... default <default_target>
    /// switch lookup <comparand_type> <comparand> <value0> <target0> (<argument0>, ...), ... default <default_target>
    /// ```
    SwitchLookup(_lookup: Box<SwitchLookup>,) = 3,
    /// Indexes a table to determine where to transfer control to.
    ///
    /// ### Implementation
    /// As an index is used into a table to determine the target, this has `O(1)` time complexity.
    ///
    /// ### Assembly Syntax
    /// ```text
    /// switch table <index_type> <index> <target0>, <target1>, ... default <default_target>
    /// switch table <index_type> <index> <target0> (<argument0>, <argument1>, ...), ... default <default_target>
    /// ```
    SwitchTable(_table: Box<SwitchTable>,) = 4,
    /// Unconditionally transfers control flow to the specified code block, using the specified values as inputs.
    ///
    /// ### Assembly Syntax
    /// ```text
    /// branch <target> ; No arguments
    /// branch <target> (<argument0>, <argument1>, ...)
    /// ```
    Branch(_target: BranchTarget,) = 5,
    /// Performs a conditional branch.
    ///
    /// If the `condition` (an `i1` value) is true, transfers control to the first block; otherwise, control is transferred to
    /// the second block.
    ///
    /// ### Assembly Syntax
    /// ```text
    /// branch if <condition> <true_branch> <false_branch> ; No arguments
    /// branch if <condition> <true_branch> (<argument0>, <argument1>, ...) <false_branch> (<argument0>, <argument1>, ...) ; With arguments
    /// ```
    ///
    /// ### Example
    /// ```text
    /// ; Note that the false block accepts a different number of arguments than the true block
    /// branch if %condition $true_block (1, $some_other_argument) $false_block (1, 2, 3)
    /// ```
    BranchIf(_branch: ConditionalBranch,) = 6,
    /// Transfers control flow to the specified `function`, providing the specified values as arguments.
    ///
    /// ### Assembly Syntax
    /// ```text
    /// <result0>, <result1>, ... = call <function> (<argument0>, <argument1>, ...) ; Call function with return values
    /// call <function> (<argument0>, <argument1>, ...) ; Call function with no return values
    /// ```
    Call(_callee: index::Function, _arguments: Box<[Value]>,) = 7,
    //CallIndr = 8,
    /// Selects one of two values of the same type based on a boolean condition value (an `i1`), without the need to use
    /// branching instructions.
    ///
    /// ### Assembly Syntax
    /// ```text
    /// <result> = select <condition> <value1> <value2>
    /// ```
    Select(_select: Selection,) = 0x10,
    /// Calculates the sum of two integer values.
    ///
    /// ### Assembly Syntax
    /// ```text
    /// <sum> = iadd <x> <y> ; Ignores any overflow/underflow
    /// <sum> = iadd sat <x> <y> ; Performs saturating addition
    /// <sum>, <overflowed> = iadd ovf <x> <y>
    /// ```
    IAdd(_op: Box<IntegerArithmetic>,) = 0x1A,
    /// Calculates the integer result of subtracting `y` from `x`.
    ///
    /// ### Assembly Syntax
    /// ```text
    /// <sum> = isub <x> <y> ; Calculates x - y, ignoring any overflow/underflow
    /// <sum> = isub sat <x> <y>
    /// <sum>, <overflowed> = isub ovf <x> <y>
    /// ```
    ISub(_op: Box<IntegerArithmetic>,) = 0x1B,
    // TODO: Could introduce muli overflow variant that returns the HIGH overflowing bits instead of just a single I overflow bool.
    //IMul(_op: Box<IntegerArithmetic>,) = 0x1C,
    //IDiv = 0x1D,
    //IRem,
    //IMod,
    //IDivRem,
    //FAdd,
    //FSub,
    //FMul,
    //FDiv,
    //FRem,
    //FNeg,
    //Not
    //And,
    //Or,
    //Xor,
    //Rotate,
    //Cmp,
    //BitCount,
    //Reverse,
}}

impl Instruction {
    /// Returns `true` if the instruction is a terminator instruction.
    ///
    /// # Examples
    ///
    /// ```
    /// # use sailar::instruction::Instruction;
    /// assert_eq!(Instruction::Nop.is_terminator(), false);
    /// assert_eq!(Instruction::Return(Default::default()).is_terminator(), true);
    /// ```
    pub fn is_terminator(&self) -> bool {
        matches!(
            self,
            Self::Return(_)
                | Self::Unreachable
                | Self::Branch(_)
                | Self::BranchIf(_)
                | Self::SwitchLookup(_)
                | Self::SwitchTable(_)
        )
    }
}

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
        let instruction_byte_size = std::mem::size_of::<Instruction>();
        dbg!(instruction_byte_size);
        assert!(instruction_byte_size <= 32);
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
            IS_CONSTANT,
            IS_INTEGER,
            INTEGER_SIZE_4,
            INTEGER_IS_EMBEDDED,
            INTEGER_EMBEDDED_ONE
        );

        assert_flags_eq!(0i16, IS_CONSTANT, IS_INTEGER, INTEGER_SIZE_2, INTEGER_IS_EMBEDDED);
        assert_flags_eq!(10u32, IS_CONSTANT, IS_INTEGER, INTEGER_SIZE_4);
        assert_flags_eq!(42u64, IS_CONSTANT, IS_INTEGER, INTEGER_SIZE_8);

        assert_flags_eq!(
            1u64,
            IS_CONSTANT,
            IS_INTEGER,
            INTEGER_SIZE_8,
            INTEGER_IS_EMBEDDED,
            INTEGER_EMBEDDED_ONE
        );
    }
}

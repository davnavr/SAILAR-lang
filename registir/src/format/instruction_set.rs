use crate::format::{indices, numeric, structures::LengthEncodedVector};

/// Specifies the target of a branch instruction, pointing to the block containing the instructions that will be executed next
/// if the target branch is taken, with `0` refering to the current block.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, PartialOrd)]
pub struct BlockOffset(pub numeric::SInteger);

/// An index into a code block's registers, starting with the input registers then temporary registers.
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd)]
pub enum RegisterIndex {
    Input(numeric::UInteger),
    Temporary(numeric::UInteger),
}

impl RegisterIndex {
    pub fn index(
        &self,
        numeric::UInteger(input_register_count): numeric::UInteger,
    ) -> numeric::UInteger {
        match self {
            // TODO: What if Input(index) is > input_register_count
            RegisterIndex::Input(index) => *index,
            RegisterIndex::Temporary(numeric::UInteger(index)) => {
                numeric::UInteger(index + input_register_count)
            }
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd)]
#[repr(u16)]
pub enum Opcode {
    Nop = 0,
    Ret = 1,
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
/// of temporary registers introduced is equal to the number of return values.
#[derive(Debug)]
pub enum Instruction {
    // NOTE: If efficiency is needed, could theoretically omit length integers from Ret and Call instructions
    /// ```txt
    /// nop
    /// ```
    /// Does absolutely nothing.
    Nop,
    /// ```txt
    /// ret (<values>)
    /// ```
    /// Returns the values in the specified registers and transfers control back to the calling method.
    ///
    /// Must be the last instruction in a block.
    Ret(LengthEncodedVector<RegisterIndex>),
}

impl Instruction {
    pub fn opcode(&self) -> Opcode {
        match self {
            Instruction::Nop => Opcode::Nop,
            Instruction::Ret(_) => Opcode::Ret,
        }
    }
}

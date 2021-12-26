use crate::format::{indices, numeric, structures::LengthEncodedVector};

/// Specifies the target of a branch instruction, pointing to the block containing the instructions that will be executed next
/// if the target branch is taken, with `0` refering to the current block.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, PartialOrd)]
pub struct BlockOffset(pub numeric::SInteger);

pub use indices::Register as RegisterIndex;

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd)]
#[repr(u32)]
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

impl TryFrom<u32> for Opcode {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        if value < Self::Ret as u32 {
            Ok(unsafe { std::mem::transmute(value) })
        } else {
            Err(())
        }
    }
}

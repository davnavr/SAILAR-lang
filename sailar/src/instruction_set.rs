//! Model of the SAILAR instruction set.

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Instruction {
    Nop,
    Break,
}

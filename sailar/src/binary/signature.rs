//! Contains types that model the structure of signatures in the SAILAR binary format.

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[repr(u8)]
#[non_exhaustive]
pub enum TypeCode {
    U8 = 1,
    U16 = 2,
    U32 = 4,
    U64 = 8,
    S8 = 0x11,
    S16 = 0x12,
    S32 = 0x14,
    S64 = 0x18,
    F32 = 0xF4,
    F64 = 0xF8,
}

//! Module for mainpulation of SAILAR runtime values.

use sailar::instruction::{Constant, ConstantInteger};
use std::borrow::{Borrow, BorrowMut};
use std::fmt::{Debug, Formatter};

#[derive(Clone)]
#[non_exhaustive]
pub enum Value {
    I8(u8),
    I16([u8; 2]),
    I24([u8; 3]),
    I32([u8; 4]),
    I64([u8; 8]),
    Boxed(Box<[u8]>),
}

impl Value {
    pub const fn from_constant(constant: Constant) -> Self {
        match constant {
            Constant::Integer(integer) => match integer {
                ConstantInteger::I8(b) => Self::I8(b),
                ConstantInteger::I16(s) => Self::I16(s),
                ConstantInteger::I32(i) => Self::I32(i),
                ConstantInteger::I64(l) => Self::I64(l),
            },
        }
    }
}

impl Borrow<[u8]> for Value {
    fn borrow(&self) -> &[u8] {
        match self {
            Self::I8(b) => std::slice::from_ref(b),
            Self::I16(s) => s.as_slice(),
            Self::I24(i24) => i24.as_slice(),
            Self::I32(i) => i.as_slice(),
            Self::I64(l) => l.as_slice(),
            Self::Boxed(bytes) => bytes,
        }
    }
}

impl BorrowMut<[u8]> for Value {
    fn borrow_mut(&mut self) -> &mut [u8] {
        match self {
            Self::I8(b) => std::slice::from_mut(b),
            Self::I16(s) => s.as_mut_slice(),
            Self::I24(i24) => i24.as_mut_slice(),
            Self::I32(i) => i.as_mut_slice(),
            Self::I64(l) => l.as_mut_slice(),
            Self::Boxed(ref mut bytes) => bytes,
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let bytes: &[u8] = self.borrow();
        Debug::fmt(bytes, f)
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::Boxed(Default::default())
    }
}

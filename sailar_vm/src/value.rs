//! Module for mainpulation of SAILAR runtime values.

use std::borrow::{Borrow, BorrowMut};
use std::fmt::{Debug, Formatter};

#[derive(Clone)]
#[non_exhaustive]
pub enum Value {
    One(u8),
    Two([u8; 2]),
    Four([u8; 4]),
    Eight([u8; 8]),
    Boxed(Box<[u8]>),
}

impl Borrow<[u8]> for Value {
    fn borrow(&self) -> &[u8] {
        match self {
            Self::One(b) => std::slice::from_ref(b),
            Self::Two(s) => s.as_slice(),
            Self::Four(i) => i.as_slice(),
            Self::Eight(l) => l.as_slice(),
            Self::Boxed(bytes) => &bytes,
        }
    }
}

impl BorrowMut<[u8]> for Value {
    fn borrow_mut(&mut self) -> &mut [u8] {
        match self {
            Self::One(b) => std::slice::from_mut(b),
            Self::Two(s) => s.as_mut_slice(),
            Self::Four(i) => i.as_mut_slice(),
            Self::Eight(l) => l.as_mut_slice(),
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

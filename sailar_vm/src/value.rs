//! Module for mainpulation of SAILAR runtime values.

use sailar::instruction::ConstantInteger;
use sailar_load::type_system::IntegerType;
use std::borrow::{Borrow, BorrowMut};
use std::fmt::{Debug, Formatter};

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Endianness {
    Little,
    Big,
}

impl Default for Endianness {
    fn default() -> Self {
        Self::Little
    }
}

#[derive(Clone)]
#[non_exhaustive]
pub enum Value {
    I8(u8),
    I16([u8; 2]),
    I24([u8; 3]),
    I32([u8; 4]),
    I40([u8; 5]),
    I48([u8; 6]),
    I56([u8; 7]),
    I64([u8; 8]),
    Boxed(Box<[u8]>),
}

impl Value {
    /// Creates a value from the specified constant, truncating, zero extending, or sign extending as necessary to ensure the
    /// value fits in the specified integer type.
    pub fn from_constant_integer(mut constant: ConstantInteger, integer_type: IntegerType, endianness: Endianness) -> Self {
        let mut value = match integer_type.size().byte_size().get() {
            1 => Value::I8(0),
            2 => Value::I16([0; 2]),
            3 => Value::I24([0; 3]),
            4 => Value::I32([0; 4]),
            5 => Value::I40([0; 5]),
            6 => Value::I48([0; 6]),
            7 => Value::I56([0; 7]),
            8 => Value::I64([0; 8]),
            size => Value::Boxed(vec![0u8; size.into()].into_boxed_slice()),
        };

        if integer_type.size().bit_size().get() % 8 != 0 {
            todo!("weird integer type sizes are not yet supported");
        }

        if endianness == Endianness::Big {
            BorrowMut::<[u8]>::borrow_mut(&mut constant).reverse();
        }

        BorrowMut::<[u8]>::borrow_mut(&mut value).copy_from_slice(Borrow::<[u8]>::borrow(&constant));
        value
    }
}

macro_rules! value_conversion_to_integer {
    ($name:ident, $destination:ty) => {
        impl Value {
            pub fn $name(self, endianness: Endianness) -> $destination {
                match <[u8; <$destination>::BITS as usize / 8]>::try_from(self.borrow()) {
                    Ok(bytes) => match endianness {
                        Endianness::Little => <$destination>::from_le_bytes(bytes),
                        Endianness::Big => <$destination>::from_be_bytes(bytes),
                    },
                    Err(e) => todo!("handle {:?}", e),
                }
            }
        }
    };
}

value_conversion_to_integer!(into_u8, u8);
value_conversion_to_integer!(into_u32, u32);
value_conversion_to_integer!(into_u64, u64);

impl Borrow<[u8]> for Value {
    fn borrow(&self) -> &[u8] {
        match self {
            Self::I8(b) => std::slice::from_ref(b),
            Self::I16(s) => s.as_slice(),
            Self::I24(i24) => i24.as_slice(),
            Self::I32(i) => i.as_slice(),
            Self::I40(i40) => i40.as_slice(),
            Self::I48(i48) => i48.as_slice(),
            Self::I56(i56) => i56.as_slice(),
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
            Self::I40(i40) => i40.as_mut_slice(),
            Self::I48(i48) => i48.as_mut_slice(),
            Self::I56(i56) => i56.as_mut_slice(),
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

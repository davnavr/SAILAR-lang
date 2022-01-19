/// Represents an unsigned integer in little-endian order.
#[derive(Clone, Copy, Default, Eq, Hash, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct UInteger(pub u32);

/// Represents a signed integer in little-endian order.
#[derive(Clone, Copy, Default, Eq, Hash, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct SInteger(pub i32);

/// Specifies the size of integers and indices in the module file.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum IntegerSize {
    I1 = 0,
    I2,
    I4,
}

macro_rules! integer_traits {
    ($t: ty, $backing_type: ty) => {
        impl<T> From<T> for $t
        where
            $backing_type: From<T>,
        {
            fn from(value: T) -> Self {
                Self(<$backing_type>::from(value))
            }
        }

        impl TryFrom<$t> for usize {
            type Error = std::num::TryFromIntError;

            fn try_from(value: $t) -> Result<Self, Self::Error> {
                usize::try_from(value.0)
            }
        }

        impl std::fmt::Debug for $t {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                std::fmt::Debug::fmt(&self.0, f)
            }
        }

        impl std::fmt::Display for $t {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                std::fmt::Display::fmt(&self.0, f)
            }
        }

        impl std::fmt::UpperHex for $t {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                std::fmt::UpperHex::fmt(&self.0, f)
            }
        }
    };
}

integer_traits!(UInteger, u32);
integer_traits!(SInteger, i32);

impl IntegerSize {
    pub fn size(self) -> u8 {
        match self {
            Self::I1 => 1,
            Self::I2 => 2,
            Self::I4 => 4,
        }
    }
}

impl TryFrom<u8> for IntegerSize {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::I1),
            1 => Ok(Self::I2),
            2 => Ok(Self::I4),
            _ => Err(()),
        }
    }
}

impl std::fmt::Display for IntegerSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::I1 => "1",
            Self::I2 => "2",
            Self::I4 => "4",
        })
    }
}

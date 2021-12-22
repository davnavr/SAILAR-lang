/// Represents an unsigned integer.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq, PartialOrd)]
pub struct UInteger(pub u32);

/// Represents a signed integer.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq, PartialOrd)]
pub struct SInteger(pub i32);

/// Specifies the size of integers and indices in the module file.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum IntegerSize {
    I1 = 0,
    I2,
    I4,
}

macro_rules! integer_conversions {
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
    };
}

integer_conversions!(UInteger, u32);
integer_conversions!(SInteger, i32);

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

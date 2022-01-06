use bitflags::bitflags;

bitflags! {
    #[derive(Default)]
    #[repr(transparent)]
    pub struct CodeBlock: u8 {
        const NO_EXCEPTION_HANDLING = 0;
        const EXCEPTION_HANDLER_IGNORES_EXCEPTION = 0b0000_0001;
        const EXCEPTION_HANDLER_STORES_EXCEPTION = 0b0000_0010;
    }
}

bitflags! {
    #[derive(Default)]
    #[repr(transparent)]
    pub struct Namespace: u8 {
        const NONE = 0;
        const HAS_PARENT = 0b0000_0011;
    }
}

bitflags! {
    #[derive(Default)]
    #[repr(transparent)]
    pub struct Struct: u8 {
        const NONE = 0;
        const IS_EXPORT = 0b0000_0011;
    }
}

bitflags! {
    /// Describes the properties of a field or global.
    #[derive(Default)]
    #[repr(transparent)]
    pub struct Field: u8 {
        const NONE = 0;
        const IS_EXPORT = 0b0000_0001;
        const MUTABLE = 0b0000_0010;
    }
}

bitflags! {
    #[derive(Default)]
    #[repr(transparent)]
    pub struct Function: u8 {
        const NONE = 0;
        const IS_EXPORT = 0b0000_0001;
        const IS_EXTERNAL = 0b0000_0010;
    }
}

macro_rules! numeric_enum_conversion {
    ($flag_type: ty, $integer_type: ty, $minimum_value: ident, $maximum_value: ident) => {
        impl TryFrom<$integer_type> for $flag_type {
            type Error = $integer_type;

            fn try_from(value: $integer_type) -> Result<Self, Self::Error> {
                if value >= <$flag_type>::$minimum_value as $integer_type
                    && value <= <$flag_type>::$maximum_value as $integer_type
                {
                    Ok(unsafe { std::mem::transmute(value) })
                } else {
                    Err(value)
                }
            }
        }
    };
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum TypeLayout {
    /// The runtime or compiler is free to decide how the fields of the type are laid out.
    Unspecified = 0,
    /// The fields of the type are laid out sequentially, and the size of the type is calculated automatically.
    Sequential,
    /// The size and offset of fields is specified manually.
    ExplicitOffsets,
    /// The fields of the type are laid out sequentially, but the size of the type is specified manually.
    ExplicitSize,
}

numeric_enum_conversion!(TypeLayout, u8, Unspecified, ExplicitSize);

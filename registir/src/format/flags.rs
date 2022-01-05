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

// TODO: Since some bits in flags are unused, insert visibility bits into Type, Field, and Method flags.
/// Indicates whether or not a type, field, or method can be imported by another module.
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum Visibility {
    /// Compiler decides whether or not it can be used.
    Unspecified = 0,
    /// Can be used as an import by another module.
    Public,
    /// Can only be used within the current module.
    Private,
}

impl Default for Visibility {
    fn default() -> Self {
        Visibility::Unspecified
    }
}

impl TryFrom<u8> for Visibility {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value < Visibility::Private as u8 {
            Ok(unsafe { std::mem::transmute(value) })
        } else {
            Err(())
        }
    }
}

bitflags! {
    #[repr(transparent)]
    pub struct Field: u8 {
        const READ_ONLY = 0;
        const MUTABLE = 0b0000_0001;
        const STATIC = 0b0000_0010;
        const VALID_MASK = 0b0000_0011;
    }
}

bitflags! {
    #[derive(Default)]
    #[repr(transparent)]
    pub struct Method: u8 {
        const FINAL = 0;
        const INSTANCE = 0b0000_0001;
        const CONSTRUCTOR_OR_INITIALIZER = 0b0000_0010;
        const CONSTRUCTOR = Self::CONSTRUCTOR_OR_INITIALIZER.bits | Self::INSTANCE.bits;
        const INITIALIZER = Self::CONSTRUCTOR_OR_INITIALIZER.bits;
        const VIRTUAL = 0b0000_0100;
    }
}

bitflags! {
    #[derive(Default)]
    #[repr(transparent)]
    pub struct Type: u8 {
        const FINAL = 0;
        /// The type can be inherited from.
        const NOT_FINAL = 0b0000_0001;
        /// Instances of this type cannot be created.
        const ABSTRACT = 0b0000_0010;
    }
}

bitflags! {
    #[derive(Default)]
    #[repr(transparent)]
    pub struct MethodBody: u8 {
        const DEFINED = 0;
        /// The method body is not defined.
        const NONE = 0b0000_0001;
        const EXTERNAL = 0b0000_0010;
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum TypeLayout {
    /// The runtime or compiler is free to decide how the fields of the type are laid out.
    Unspecified = 0,
    /// The fields of the type are laid out sequentially, and the size of the type is calculated automatically.
    Sequential = 1,
    /// The size and offset of fields is specified manually.
    ExplicitOffsets = 2,
    /// The fields of the type are laid out sequentially, but the size of the type is specified manually.
    ExplicitSize = 3,
}

impl TryFrom<u8> for TypeLayout {
    // TODO: Have macro for some enum TryFrom conversions.
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value <= Self::ExplicitSize as u8 {
            Ok(unsafe { std::mem::transmute(value) })
        } else {
            Err(())
        }
    }
}

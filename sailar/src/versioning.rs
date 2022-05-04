/// Specifies the version of a SAILAR module file.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
#[non_exhaustive]
pub struct Format {
    /// The major version number, incremented when backwards incompatible changes are made to the format.
    pub major: u8,
    pub minor: u8,
}

impl Format {
    /// The minimum version of the format supported by this API.
    pub const MINIMUM_SUPPORTED: &'static Self = &Self { major: 0, minor: 12 };

    pub const CURRENT: &'static Self = Self::MINIMUM_SUPPORTED;
}

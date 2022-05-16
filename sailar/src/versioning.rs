//! Types to model version numbers in SAILAR modules.

/// Specifies the version of a SAILAR module file.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[non_exhaustive]
pub struct Format {
    /// The major version number, incremented when backwards incompatible changes are made to the format.
    pub major: u8,
    pub minor: u8,
}

impl Format {
    pub const SUPPORTED_RANGE: std::ops::RangeInclusive<Format> =
        std::ops::RangeInclusive::new(SupportedFormat::MINIMUM.0, SupportedFormat::MAXIMUM.0);

    pub const fn new(major: u8, minor: u8) -> Self {
        Self { major, minor }
    }

    #[inline]
    pub fn is_supported(&self) -> bool {
        Self::SUPPORTED_RANGE.contains(self)
    }
}

/// Represents a SAILAR format version that is supported by this version of the API.
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct SupportedFormat(Format);

impl SupportedFormat {
    /// The minimum version of the format supported by this API.
    pub const MINIMUM: Self = Self(Format::new(0, 18));
    /// The maximum version of the format that is supported.
    pub const MAXIMUM: Self = Self::MINIMUM;
    pub const CURRENT: Self = Self::MINIMUM;

    #[inline]
    pub fn new(major: u8, minor: u8) -> Result<Self, UnsupportedFormatError> {
        Self::try_from(Format::new(major, minor))
    }
}

impl std::ops::Deref for SupportedFormat {
    type Target = Format;

    #[inline]
    fn deref(&self) -> &Format {
        &self.0
    }
}

impl TryFrom<Format> for SupportedFormat {
    type Error = UnsupportedFormatError;

    fn try_from(version: Format) -> Result<Self, Self::Error> {
        if version.is_supported() {
            Ok(Self(version))
        } else {
            Err(UnsupportedFormatError::new(version))
        }
    }
}

/// Error used when a format version is not supported.
#[derive(Clone, Debug, thiserror::Error)]
pub struct UnsupportedFormatError {
    version: Format,
}

impl UnsupportedFormatError {
    pub(crate) fn new(version: Format) -> Self {
        Self { version }
    }

    #[inline]
    pub fn version(&self) -> &Format {
        &self.version
    }
}

impl std::fmt::Display for UnsupportedFormatError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "the format version {}.{} is not supported",
            self.version.major, self.version.minor
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::versioning::Format;

    #[test]
    fn version_with_greater_major_number_is_greater() {
        let bigger = Format { major: 2, minor: 0 };
        let smaller = Format { major: 1, minor: 9 };
        assert!(bigger > smaller);
    }
}

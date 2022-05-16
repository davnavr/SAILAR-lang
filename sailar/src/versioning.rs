//! Types to model version numbers in SAILAR modules.

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

    pub const fn new(major: u8, minor: u8) -> Self {
        Self { major, minor }
    }

    #[inline]
    pub fn is_supported(&self) -> bool {
        self >= Self::MINIMUM_SUPPORTED
    }
}

//pub struct ValidFormat(FormatVersion);

/// Error used when a format version is not supported.
#[derive(Debug, thiserror::Error)]
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

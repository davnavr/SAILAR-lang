use crate::format::{numeric::UInteger, LenVec};

/// A length-encoded array of variable-length unsigned integers used to indicate a version.
#[derive(Clone, Debug, Default, Eq, PartialEq, Hash)]
pub struct Numbers(pub LenVec<UInteger>);

impl Numbers {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

/// Specifies what version of the module format is being used, placed after the module's integer size field.
#[derive(Clone, Debug, Eq, PartialEq, PartialOrd)]
pub struct Format {
    pub major: UInteger,
    pub minor: UInteger,
}

static MINIMUM_FORMAT_VERSION: Format = Format {
    major: UInteger(0),
    minor: UInteger(5),
};

impl Format {
    pub fn minimum_supported_version() -> &'static Self {
        &MINIMUM_FORMAT_VERSION
    }

    /// Indicates if this version is greater than or equal to the [`Format::minimum_supported_version()`].
    pub fn is_supported(&self) -> bool {
        self >= Self::minimum_supported_version()
    }
}

impl std::default::Default for Format {
    fn default() -> Self {
        MINIMUM_FORMAT_VERSION.clone()
    }
}

impl<T: Into<UInteger>> std::iter::FromIterator<T> for Numbers {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut numbers = Vec::new();
        for i in iter {
            numbers.push(i.into())
        }
        Self(LenVec(numbers))
    }
}

impl From<&[u32]> for Numbers {
    fn from(numbers: &[u32]) -> Self {
        Self(LenVec(numbers.iter().cloned().map(UInteger).collect()))
    }
}

#[cfg(test)]
mod tests {
    use crate::format::{numeric::UInteger, FormatVersion};

    #[test]
    fn larger_format_major_version_is_greater() {
        assert!(
            FormatVersion {
                major: UInteger(2),
                minor: UInteger(3)
            } > FormatVersion {
                major: UInteger(1),
                minor: UInteger(6)
            }
        )
    }
}

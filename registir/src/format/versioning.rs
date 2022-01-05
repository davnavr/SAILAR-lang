use crate::format::{numeric::UInteger, LenVec};

/// A length-encoded array of variable-length unsigned integers used to indicate a version.
#[derive(Clone, Debug, Default, Eq, PartialEq, Hash)]
pub struct Numbers(pub LenVec<UInteger>);

impl<T: Into<UInteger>> std::iter::FromIterator<T> for Numbers {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut numbers = Vec::new();
        for i in iter {
            numbers.push(i.into())
        }
        Self(LenVec(numbers))
    }
}

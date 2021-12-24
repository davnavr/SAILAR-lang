use std::ops::Deref;

/// Represents data that is preceded by an unsigned integer indicating the byte length of the following data.
#[derive(Debug, Default, Eq, PartialEq, PartialOrd)]
pub struct ByteLengthEncoded<T>(pub T);

/// Represents an array preceded by an unsigned integer indicating the number of items.
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq, PartialOrd)]
pub struct LengthEncodedVector<T>(pub Vec<T>);

pub type DoubleLengthEncodedVector<T> = ByteLengthEncoded<LengthEncodedVector<T>>;

impl<T> ByteLengthEncoded<T> {
    pub fn data(&self) -> &T {
        &self.0
    }

    pub fn as_ref(&self) -> ByteLengthEncoded<&T> {
        ByteLengthEncoded(&self.0)
    }
}

impl<T> LengthEncodedVector<T> {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<T> Deref for LengthEncodedVector<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> Deref for ByteLengthEncoded<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

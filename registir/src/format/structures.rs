use std::{
    fmt::{Debug, Formatter},
    ops::Deref,
};

/// Represents data that is preceded by an unsigned integer indicating the byte length of the following data.
#[derive(Default, Eq, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct ByteLengthEncoded<T>(pub T);

/// Represents an array preceded by an unsigned integer indicating the number of items.
#[derive(Clone, Default, Eq, Hash, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct LengthEncodedVector<T>(pub Vec<T>);

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

impl<T> From<T> for ByteLengthEncoded<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}

impl<T> From<Vec<T>> for LengthEncodedVector<T> {
    fn from(values: Vec<T>) -> Self {
        Self(values)
    }
}

impl<T> From<Vec<T>> for ByteLengthEncoded<LengthEncodedVector<T>> {
    fn from(values: Vec<T>) -> Self {
        Self(LengthEncodedVector(values))
    }
}

impl<T: Debug> Debug for ByteLengthEncoded<T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl<T: Debug> Debug for LengthEncodedVector<T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

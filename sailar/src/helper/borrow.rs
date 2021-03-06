//! Contains code for working with borrowed data.

use std::borrow::Borrow;
use std::cmp::{Eq, PartialEq};
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

/// Trait implemented by objects that can be boxed.
pub trait ToBox {
    fn to_box(&self) -> Box<Self>;
}

impl<T: Clone> ToBox for T {
    fn to_box(&self) -> Box<Self> {
        Box::new(self.clone())
    }
}

impl<T: Clone> ToBox for [T] {
    fn to_box(&self) -> Box<Self> {
        std::borrow::ToOwned::to_owned(self).into_boxed_slice()
    }
}

impl ToBox for str {
    fn to_box(&self) -> Box<str> {
        Box::from(self)
    }
}

/// Version of [`std::borrow::Cow`] whose owned form is `Box<T>`.
pub enum CowBox<'a, B: ?Sized> {
    Borrowed(&'a B),
    Boxed(Box<B>),
}

impl<'a, B: ?Sized + ToBox> CowBox<'a, B> {
    pub fn into_boxed(self) -> Box<B> {
        match self {
            Self::Boxed(b) => b,
            Self::Borrowed(b) => ToBox::to_box(b),
        }
    }
}

impl<'a, B: ?Sized> From<&'a B> for CowBox<'a, B> {
    fn from(borrowed: &'a B) -> Self {
        Self::Borrowed(borrowed)
    }
}

impl<B: ?Sized> From<Box<B>> for CowBox<'_, B> {
    fn from(owned: Box<B>) -> Self {
        Self::Boxed(owned)
    }
}

impl<T> From<Vec<T>> for CowBox<'_, [T]> {
    fn from(v: Vec<T>) -> Self {
        Self::Boxed(v.into_boxed_slice())
    }
}

impl<'a, B: ?Sized + ToBox> CowBox<'a, B> {
    pub fn to_mut(&mut self) -> &mut B {
        match self {
            Self::Borrowed(borrowed) => {
                *self = Self::Boxed(borrowed.to_box());
                if let Self::Boxed(ref mut owned) = self {
                    owned
                } else {
                    unreachable!()
                }
            }
            Self::Boxed(ref mut owned) => owned,
        }
    }

    pub fn into_box(self) -> Box<B> {
        match self {
            Self::Borrowed(borrowed) => borrowed.to_box(),
            Self::Boxed(owned) => owned,
        }
    }
}

impl<B: ?Sized> Borrow<B> for CowBox<'_, B> {
    fn borrow(&self) -> &B {
        match self {
            Self::Borrowed(borrowed) => borrowed,
            Self::Boxed(owned) => owned,
        }
    }
}

impl<B: ?Sized> std::convert::AsRef<B> for CowBox<'_, B> {
    #[inline]
    fn as_ref(&self) -> &B {
        self.borrow()
    }
}

impl<B: ?Sized> std::ops::Deref for CowBox<'_, B> {
    type Target = B;

    #[inline]
    fn deref(&self) -> &B {
        self.borrow()
    }
}

impl<B: ?Sized + ToBox> Clone for CowBox<'_, B> {
    fn clone(&self) -> Self {
        let borrowed: &B = self.borrow();
        Self::Boxed(borrowed.to_box())
    }
}

impl<B: ?Sized + Debug> Debug for CowBox<'_, B> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Borrowed(borrowed) => Debug::fmt(borrowed, f),
            Self::Boxed(boxed) => Debug::fmt(boxed, f),
        }
    }
}

impl<B: ?Sized + Display> Display for CowBox<'_, B> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Borrowed(borrowed) => Display::fmt(borrowed, f),
            Self::Boxed(boxed) => Display::fmt(boxed, f),
        }
    }
}

impl<B: ?Sized + PartialEq> PartialEq for CowBox<'_, B> {
    fn eq(&self, other: &Self) -> bool {
        B::eq(self.borrow(), other.borrow())
    }
}

impl<B: ?Sized + Eq> Eq for CowBox<'_, B> {}

impl<B: ?Sized + Hash> Hash for CowBox<'_, B> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

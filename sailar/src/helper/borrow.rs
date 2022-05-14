//! Contains code for working with borrowed data.

use std::borrow::Borrow;
use std::fmt::{Debug, Display, Formatter};

/// Trait implemented by objects that can be boxed.
pub trait ToBox {
    fn to_box(&self) -> Box<Self>;
}

impl<C: Clone> ToBox for C {
    fn to_box(&self) -> Box<Self> {
        Box::new(self.clone())
    }
}

impl<T: Copy> ToBox for [T] {
    fn to_box(&self) -> Box<Self> {
        Box::from(self)
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
    fn as_ref(&self) -> &B {
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

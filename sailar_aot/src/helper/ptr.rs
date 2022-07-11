//! Module to help with manipulation of pointer types.

use std::cmp::{Eq, PartialEq};
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::sync::Arc as ArcPtr;

/// Wrapper for [`Arc`] that uses [`Arc::ptr_eq`] for equality.
///
/// [`Arc`]: ArcPtr
/// [`Arc::ptr_eq`]: ArcPtr::ptr_eq
#[derive(Clone)]
#[repr(transparent)]
pub struct ArcEq<T: ?Sized>(ArcPtr<T>);

impl<T: ?Sized> From<ArcPtr<T>> for ArcEq<T> {
    fn from(arc: ArcPtr<T>) -> Self {
        Self(arc)
    }
}

impl<T: ?Sized> Deref for ArcEq<T> {
    type Target = ArcPtr<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: ?Sized> PartialEq for ArcEq<T> {
    fn eq(&self, other: &Self) -> bool {
        ArcPtr::ptr_eq(&self.0, &other.0)
    }
}

impl<T: ?Sized> Eq for ArcEq<T> {}

impl<T: ?Sized> Hash for ArcEq<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ArcPtr::as_ptr(&self.0).hash(state)
    }
}

impl<T: Debug + ?Sized> Debug for ArcEq<T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

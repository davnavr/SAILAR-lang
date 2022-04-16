//! Helper types for operating with references.

use std::sync::Arc;

/// A smart pointer type that allows for single ownership or multiple ownership with thread-safe reference counting.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Ref<T>
where
    T: ?Sized,
{
    Arc(Arc<T>),
    Box(Box<T>),
}

impl<T> Ref<T> {
    pub fn into_arc(this: Self) -> Arc<T> {
        match this {
            Self::Arc(arc) => arc,
            Self::Box(boxed) => Arc::new(*boxed),
        }
    }
}

impl<T: ?Sized> From<Box<T>> for Ref<T> {
    #[inline]
    fn from(boxed: Box<T>) -> Self {
        Self::Box(boxed)
    }
}

impl<T: ?Sized> From<Arc<T>> for Ref<T> {
    #[inline]
    fn from(arc: Arc<T>) -> Self {
        Self::Arc(arc)
    }
}

impl<T> From<Ref<T>> for Arc<T> {
    #[inline]
    fn from(reference: Ref<T>) -> Self {
        Ref::into_arc(reference)
    }
}

impl<T: ?Sized> std::ops::Deref for Ref<T> {
    type Target = T;

    fn deref(&self) -> &T {
        match self {
            Self::Arc(arc) => &arc,
            Self::Box(boxed) => &boxed,
        }
    }
}

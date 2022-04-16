//! Helper types for operating with references and smart pointers.

use std::sync::Arc;

/// A smart pointer type that allows for single ownership or multiple ownership with thread-safe reference counting.
/// 'Orc' stands for 'Optionally (and atomically) Reference Counted'.
///
/// This attempts to avoid any overhead associated with `Arc<T>` where it is not necessary.
#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Orc<T: ?Sized> {
    Arc(Arc<T>),
    Box(Box<T>),
}

impl<T> Orc<T> {
    pub fn into_arc(this: Self) -> Arc<T> {
        match this {
            Self::Arc(arc) => arc,
            Self::Box(boxed) => Arc::new(*boxed),
        }
    }
}

impl<T: ?Sized> From<Box<T>> for Orc<T> {
    #[inline]
    fn from(boxed: Box<T>) -> Self {
        Self::Box(boxed)
    }
}

impl<T: ?Sized> From<Arc<T>> for Orc<T> {
    #[inline]
    fn from(arc: Arc<T>) -> Self {
        Self::Arc(arc)
    }
}

impl<T> From<Orc<T>> for Arc<T> {
    #[inline]
    fn from(reference: Orc<T>) -> Self {
        Orc::into_arc(reference)
    }
}

impl<T: ?Sized> std::ops::Deref for Orc<T> {
    type Target = T;

    fn deref(&self) -> &T {
        match self {
            Self::Arc(arc) => &arc,
            Self::Box(boxed) => &boxed,
        }
    }
}

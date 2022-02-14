use std::cell::UnsafeCell;

#[derive(Debug)]
pub struct Once<T> {
    value: UnsafeCell<Option<T>>,
}

impl<T> Once<T> {
    pub fn new() -> Self {
        Self {
            value: std::cell::UnsafeCell::new(None),
        }
    }

    pub fn get_or_insert_fallible<'a, E, F: FnOnce() -> Result<T, E>>(
        &'a self,
        initializer: F,
    ) -> Result<&'a T, E> {
        // Value is never null, and only one reference to the value exists at a time.
        let value: &'a mut Option<_> = unsafe { &mut *self.value.get() };
        if let Some(existing) = value {
            Ok(existing)
        } else {
            Ok(value.insert(initializer()?))
        }
    }

    pub fn get_or_insert<'a, F: FnOnce() -> T>(&'a self, initializer: F) -> &'a T {
        self.get_or_insert_fallible(|| {
            std::result::Result::<T, std::convert::Infallible>::Ok(initializer())
        })
        .unwrap()
    }
}

impl<T> Default for Once<T> {
    fn default() -> Self {
        Self::new()
    }
}

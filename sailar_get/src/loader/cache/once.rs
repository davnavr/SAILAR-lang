use std::cell::UnsafeCell;

#[derive(Default, Debug)]
pub struct Once<T> {
    value: UnsafeCell<Option<T>>,
}

impl<T> Once<T> {
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
}

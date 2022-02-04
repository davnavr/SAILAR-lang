pub struct Cell<T> {
    next: std::cell::Cell<u32>,
    phantom: std::marker::PhantomData<T>,
}

impl<T> Cell<T> {
    pub fn new() -> Self {
        Self::with_start_value(0)
    }

    pub fn with_start_value(value: u32) -> Self {
        Self {
            next: std::cell::Cell::new(value),
            phantom: std::marker::PhantomData,
        }
    }
}

impl<T> Default for Cell<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: From<u32>> Cell<T> {
    pub fn next(&self) -> T {
        let value = T::from(self.next.get());
        self.next.set(self.next.get() + 1);
        value
    }
}

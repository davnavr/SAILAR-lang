#[derive(Debug)]
pub struct Cell<T> {
    next: std::cell::Cell<u32>,
    phantom: std::marker::PhantomData<T>,
}

impl<T> Cell<T> {
    pub fn with_start_value(start: u32) -> Self {
        Self {
            next: std::cell::Cell::new(start),
            phantom: std::marker::PhantomData,
        }
    }

    pub fn new() -> Self {
        Self::with_start_value(0)
    }
}

impl<T: From<u32>> Cell<T> {
    pub fn next(&self) -> T {
        let value = T::from(self.next.get());
        self.next.set(self.next.get() + 1);
        value
    }
}

#[derive(Debug)]
pub struct Counter<T> {
    next: u32,
    phantom: std::marker::PhantomData<T>,
}

impl<T> Counter<T> {
    pub fn with_start_value(start: u32) -> Self {
        Self {
            next: start,
            phantom: std::marker::PhantomData,
        }
    }

    pub fn new() -> Self {
        Self::with_start_value(0)
    }
}

impl<T: From<u32>> Counter<T> {
    pub fn next(&mut self) -> T {
        let value = T::from(self.next);
        self.next += 1;
        value
    }
}

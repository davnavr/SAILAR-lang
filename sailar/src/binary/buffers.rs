use std::cell::RefCell;

#[derive(Debug)]
pub struct BufferPool {
    buffers: RefCell<Vec<Vec<u8>>>,
}

#[derive(Debug)]
pub struct RentedBuffer<'a> {
    pool: &'a BufferPool,
    buffer: Vec<u8>,
}

impl BufferPool {
    pub fn new() -> Self {
        Self {
            buffers: RefCell::new(Vec::new()),
        }
    }

    pub fn rent(&self) -> RentedBuffer<'_> {
        RentedBuffer {
            pool: self,
            buffer: match self.buffers.borrow_mut().pop() {
                Some(mut buffer) => {
                    buffer.clear();
                    buffer
                }
                None => Vec::new(),
            },
        }
    }

    pub fn rent_with_capacity(&self, capacity: usize) -> RentedBuffer<'_> {
        let mut buffer = self.rent();
        if buffer.capacity() < capacity {
            // The buffer is empty, so reserve will ensure the capacity is at least as many bytes as requested.
            buffer.reserve(capacity);
        }
        buffer
    }

    pub fn rent_with_length(&self, length: usize) -> RentedBuffer<'_> {
        let mut buffer = self.rent_with_capacity(length);
        // Buffer was already cleared, so changing length should be safe.
        unsafe { buffer.set_len(length) }
        buffer
    }
}

impl<'a> std::ops::Deref for RentedBuffer<'a> {
    type Target = Vec<u8>;

    fn deref(&self) -> &Vec<u8> {
        &self.buffer
    }
}

impl<'a> std::ops::DerefMut for RentedBuffer<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.buffer
    }
}

impl<'a> Drop for RentedBuffer<'a> {
    fn drop(&mut self) {
        self.pool
            .buffers
            .borrow_mut()
            .push(std::mem::take(&mut self.buffer));
    }
}

#[cfg(test)]
mod tests {
    use crate::buffers;

    #[test]
    fn buffer_rent_test() {
        let pool = buffers::BufferPool::new();

        {
            let mut buffer = pool.rent();
            buffer.push(1);
        }

        assert_eq!(pool.rent().len(), 0);
    }
}

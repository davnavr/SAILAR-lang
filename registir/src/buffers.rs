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
    fn buffer_rent_twice_test() {
        let pool = buffers::BufferPool::new();

        {
            let mut buffer_1 = pool.rent();
            buffer_1.push(1);
            let mut buffer_2 = pool.rent();
            buffer_2.push(2);
        }

        assert_eq!(pool.rent().len(), 0);
        assert_eq!(pool.buffers.borrow().len(), 2);
    }
}

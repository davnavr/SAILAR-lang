use std::ptr::NonNull;

pub use std::num::NonZeroUsize as Capacity;

pub static DEFAULT_CAPACITY: Capacity = unsafe { Capacity::new_unchecked(0xFFFFF) };

pub struct Stack {
    memory: Box<[u8]>,
    allocations: Vec<usize>,
    allocated: usize,
}

impl Stack {
    pub fn new(capacity: Capacity) -> Self {
        Self {
            memory: vec![0u8; capacity.get()].into_boxed_slice(),
            allocations: Vec::new(),
            allocated: 0,
        }
    }

    pub fn capacity(&self) -> usize {
        self.memory.len()
    }

    pub fn remaining(&self) -> usize {
        self.capacity() - self.allocated
    }

    /// Allocates an object of the specified `size` on the stack.
    ///
    /// # Safety
    /// The caller must ensure that the stack outlives the pointer returned by this function.
    pub unsafe fn allocate(&mut self, size: usize) -> Option<NonNull<u8>> {
        let actual_size = size + (self.allocated % size);
        if actual_size <= self.remaining() {
            // Memory safety handled by caller, and memory pointer is guaranteed to not be null.
            let address =
                NonNull::new_unchecked((self.memory.as_mut_ptr() as *mut u8).add(self.allocated));
            self.allocated += actual_size;
            Some(address)
        } else {
            None
        }
    }

    pub fn save(&mut self) {
        self.allocations.push(self.allocated)
    }

    /// Frees the previously allocated objects.
    ///
    /// # Panics
    /// Panics if no allocations have been saved.
    pub fn free(&mut self) {
        self.allocated = self.allocations.pop().expect("value stack underflow");
    }
}

impl std::fmt::Debug for Stack {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let address = self.memory.as_ptr() as usize;
        for (index, chunk) in self.memory.chunks(16).enumerate() {
            write!(f, "{:#08X}", address + (index * 16))?;
            for &value in chunk {
                write!(f, " {:#02}", value)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

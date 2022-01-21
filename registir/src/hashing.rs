#[derive(Clone, Debug, Default)]
pub struct IntegerHasher {
    value: u64,
}

pub type IntegerHashBuilder = std::hash::BuildHasherDefault<IntegerHasher>;

impl IntegerHasher {
    fn builder() -> IntegerHashBuilder {
        IntegerHashBuilder::default()
    }
}

impl std::hash::Hasher for IntegerHasher {
    fn finish(&self) -> u64 {
        self.value
    }

    fn write(&mut self, bytes: &[u8]) {
        for value in bytes {
            self.value <<= 8u8;
            self.value |= *value as u64;
        }
    }
}

//! Manipulation of SAILAR module data.

use std::sync::Arc;

/// Representation of SAILAR module data, which is used for literal strings and other constant-like values.
#[derive(Clone, Eq, Hash, PartialEq)]
pub struct Data {
    contents: Vec<u8>,
}

impl Data {
    pub fn new(contents: Vec<u8>) -> Arc<Self> {
        Arc::new(Self { contents })
    }

    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        &self.contents
    }
}

impl std::fmt::Debug for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(&crate::binary::buffer::ByteDebug::from(self.as_bytes()), f)
    }
}

impl std::ops::Deref for Data {
    type Target = [u8];

    #[inline]
    fn deref(&self) -> &[u8] {
        self.as_bytes()
    }
}

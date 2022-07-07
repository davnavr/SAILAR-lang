//! Module for interacting with SAILAR types.

use crate::module;
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, Weak};

pub struct Signature {
    module: Weak<module::Module>,
    signature: sailar::signature::Type,
}

impl Signature {
    pub(crate) fn new(signature: sailar::signature::Type, module: Weak<module::Module>) -> Arc<Self> {
        Arc::new(Self { module, signature })
    }

    pub fn module(&self) -> &Weak<module::Module> {
        &self.module
    }

    pub fn signature(&self) -> &sailar::signature::Type {
        &self.signature
    }
}

impl Debug for Signature {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Signature").field("signature", &self.signature).finish()
    }
}

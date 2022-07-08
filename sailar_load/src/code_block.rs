//! Module for interacting with SAILAR code blocks.

use crate::error;
use crate::module;
use crate::type_system;
use sailar::record;
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, Weak};

type Record = record::CodeBlock<'static>;

pub struct Code {
    record: Box<Record>,
    register_types: lazy_init::Lazy<Result<Box<[Arc<type_system::Signature>]>, error::LoaderError>>,
    module: Weak<module::Module>,
}

impl Code {
    pub(crate) fn new(record: Box<Record>, module: Weak<module::Module>) -> Arc<Self> {
        Arc::new(Self {
            record,
            register_types: Default::default(),
            module,
        })
    }

    pub fn record(&self) -> &Record {
        &self.record
    }

    pub fn module(&self) -> &Weak<module::Module> {
        &self.module
    }
}

impl Debug for Code {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Code")
            .field("record", &self.record)
            .field("register_types", &self.register_types)
            .finish()
    }
}

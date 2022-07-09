//! Module for interacting with SAILAR types.

use crate::error;
use crate::module;
use sailar::index;
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, Weak};

#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum Type {}

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

#[derive(Default)]
pub(crate) struct LazySignatureList(lazy_init::Lazy<Result<Box<[Arc<Signature>]>, error::LoaderError>>);

impl LazySignatureList {
    pub(crate) fn get_or_initialize<T>(
        &self,
        module: &Weak<module::Module>,
        types: T,
    ) -> Result<&[Arc<Signature>], error::LoaderError>
    where
        T: IntoIterator<Item = index::TypeSignature>,
        T::IntoIter: std::iter::ExactSizeIterator,
    {
        self.0
            .get_or_create(|| {
                let module = module::Module::upgrade_weak(module)?;
                let iterator = types.into_iter();
                let mut loaded = Vec::with_capacity(iterator.len());
                for index in iterator {
                    loaded.push(module.get_type_signature(index)?.clone());
                }
                Ok(loaded.into_boxed_slice())
            })
            .as_ref()
            .map(std::borrow::Borrow::borrow)
            .map_err(Clone::clone)
    }
}

impl Debug for LazySignatureList {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

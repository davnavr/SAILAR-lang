//! Module for interacting with SAILAR types.

use crate::error;
use crate::module;
use sailar::index;
use sailar::signature;
use std::cmp::PartialEq;
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, Weak};

pub use signature::{IntegerSign, IntegerSize, IntegerType};

#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum Type {
    FixedInteger(IntegerType),
    UAddr,
    SAddr,
    F32,
    F64,
    RawPtr(Option<Arc<Signature>>), // TODO: Make a validation error for recursive RawPtr types.
    FuncPtr(Arc<crate::function::Signature>), // TODO: Make a validation error for recursive FuncPtr types.
    Signature(Arc<Signature>),
}

impl Type {
    fn try_from_signature(signature: &signature::Type, module: &Weak<module::Module>) -> Result<Self, error::LoaderError> {
        Ok(match signature {
            signature::Type::FixedInteger(ty) => Type::FixedInteger(*ty),
            signature::Type::UAddr => Type::UAddr,
            signature::Type::SAddr => Type::SAddr,
            signature::Type::F32 => Type::F32,
            signature::Type::F64 => Type::F64,
            signature::Type::RawPtr(None) => Type::RawPtr(None),
            signature::Type::RawPtr(Some(pointee)) => Self::RawPtr(Some(
                module::Module::upgrade_weak(module)?.get_type_signature(*pointee)?.clone(),
            )),
            signature::Type::FuncPtr(signature) => Self::FuncPtr(
                module::Module::upgrade_weak(module)?
                    .get_function_signature(*signature)?
                    .clone(),
            ),
        })
    }
}

// PartialEq only since resolution
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::FixedInteger(x), Self::FixedInteger(y)) => x == y,
            (Self::UAddr, Self::UAddr) | (Self::F32, Self::F32) | (Self::F64, Self::F64) => true,
            (Self::RawPtr(x), Self::RawPtr(y)) => x == y,
            (Self::FuncPtr(x), Self::FuncPtr(y)) => x == y,
            (Self::Signature(x), Self::Signature(y)) => x == y,
            _ => false,
        }
    }
}

// If Type will contain Weak reference, make it PartialEq only
impl std::cmp::Eq for Type {}

pub struct Signature {
    module: Weak<module::Module>,
    record: signature::Type,
    signature: lazy_init::Lazy<Result<Type, error::LoaderError>>,
}

impl Signature {
    pub(crate) fn new(signature: signature::Type, module: Weak<module::Module>) -> Arc<Self> {
        Arc::new(Self {
            module,
            record: signature,
            signature: Default::default(),
        })
    }

    pub fn module(&self) -> &Weak<module::Module> {
        &self.module
    }

    pub fn record(&self) -> &signature::Type {
        &self.record
    }

    pub fn signature(&self) -> Result<&Type, error::LoaderError> {
        self.signature
            .get_or_create(|| Type::try_from_signature(&self.record, &self.module))
            .as_ref()
            .map_err(Clone::clone)
    }
}

impl Debug for Signature {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Signature")
            .field("record", &self.signature)
            .field("signature", &self.signature)
            .finish()
    }
}

// PartialEq only since resolution of the signature may result in an error
impl PartialEq for Signature {
    fn eq(&self, other: &Self) -> bool {
        if let (Ok(x), Ok(y)) = (self.signature(), other.signature()) {
            x == y
        } else {
            false
        }
    }
}

impl From<IntegerType> for Type {
    fn from(ty: IntegerType) -> Self {
        Self::FixedInteger(ty)
    }
}

impl From<Arc<Signature>> for Type {
    fn from(signature: Arc<Signature>) -> Self {
        Self::Signature(signature)
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

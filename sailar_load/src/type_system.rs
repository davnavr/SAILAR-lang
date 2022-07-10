//! Module for interacting with SAILAR types.

use crate::error;
use crate::module;
use sailar::index;
use sailar::signature;
use std::cmp::PartialEq;
use std::fmt::{Debug, Display, Formatter};
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

impl From<IntegerType> for Type {
    fn from(ty: IntegerType) -> Self {
        Self::FixedInteger(ty)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::FixedInteger(ty) => Display::fmt(ty, f),
            Self::UAddr => f.write_str("uaddr"),
            Self::SAddr => f.write_str("saddr"),
            Self::F32 => f.write_str("f32"),
            Self::F64 => f.write_str("f64"),
            Self::RawPtr(None) => f.write_str("voidptr"),
            Self::RawPtr(Some(pointee)) => write!(f, "rawptr({})", pointee),
            Self::FuncPtr(signature) => write!(f, "funcptr({})", signature),
        }
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
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeOrSignature {
    Type(Type),
    Signature(Arc<Signature>),
}

impl TypeOrSignature {
    pub fn try_get_type(&self) -> Result<&Type, error::LoaderError> {
        Ok(match self {
            Self::Type(ty) => ty,
            Self::Signature(signature) => signature.signature()?,
        })
    }
}

impl From<Type> for TypeOrSignature {
    fn from(ty: Type) -> Self {
        Self::Type(ty)
    }
}

impl Display for TypeOrSignature {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Type(ty) => Display::fmt(ty, f),
            Self::Signature(signature) => Display::fmt(signature, f),
        }
    }
}

impl From<Arc<Signature>> for TypeOrSignature {
    fn from(signature: Arc<Signature>) -> Self {
        Self::Signature(signature)
    }
}

impl<'a> TryFrom<&'a TypeOrSignature> for &'a Type {
    type Error = error::LoaderError;

    fn try_from(ty: &'a TypeOrSignature) -> Result<&'a Type, error::LoaderError> {
        ty.try_get_type()
    }
}

pub struct Signature {
    module: Weak<module::Module>,
    record: signature::Type,
    index: index::TypeSignature,
    signature: lazy_init::Lazy<Result<Type, error::LoaderError>>,
}

impl Signature {
    pub(crate) fn new(signature: signature::Type, index: index::TypeSignature, module: Weak<module::Module>) -> Arc<Self> {
        Arc::new(Self {
            module,
            record: signature,
            index,
            signature: Default::default(),
        })
    }

    pub fn module(&self) -> &Weak<module::Module> {
        &self.module
    }

    pub fn record(&self) -> &signature::Type {
        &self.record
    }

    pub fn index(&self) -> index::TypeSignature {
        self.index
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

impl Display for Signature {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self.signature.get() {
            Some(Ok(ty)) => Display::fmt(ty, f),
            None | Some(Err(_)) => write!(f, "#{}", usize::from(self.index)),
        }
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

type SignatureIndices = Box<[index::TypeSignature]>;
type ResolvedSignatureList = Result<Box<[Arc<Signature>]>, error::LoaderError>;

pub(crate) struct LazySignatureList(lazy_init::LazyTransform<SignatureIndices, ResolvedSignatureList>);

impl LazySignatureList {
    pub(crate) fn new(types: SignatureIndices) -> Self {
        Self(lazy_init::LazyTransform::new(types))
    }

    pub(crate) fn get_or_initialize(&self, module: &Weak<module::Module>) -> Result<&[Arc<Signature>], error::LoaderError> {
        self.0
            .get_or_create(|types| {
                let module = module::Module::upgrade_weak(module)?;
                let mut loaded = Vec::with_capacity(types.len());
                for index in types.iter().copied() {
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
        f.debug_tuple("LazySignatureList").field(&self.0.get()).finish()
    }
}

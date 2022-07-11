//! Module to translate SAILAR types and function signatures into LLVM types.

use crate::helper::ptr::ArcEq;
use inkwell::types::{BasicTypeEnum as LlvmBasicType, FunctionType as LlvmFunctonType};
use sailar_load::type_system::Type;
use std::cell::RefCell;
use std::cmp::PartialEq;
use std::collections::hash_map;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

#[derive(Clone, Debug)]
struct TypeEq(Type);

impl PartialEq for TypeEq {
    fn eq(&self, other: &Self) -> bool {
        match (self.0, other.0) {
            (Type::FixedInteger(x), Type::FixedInteger(y)) => x == y,
            (Type::F32, Type::F32)
            | (Type::F64, Type::F64)
            | (Type::UAddr, Type::UAddr)
            | (Type::SAddr, Type::SAddr)
            | (Type::RawPtr(None), Type::RawPtr(None)) => true,
            (Type::RawPtr(Some(x)), Type::RawPtr(Some(y))) => Arc::ptr_eq(&x, &y),
            (Type::FuncPtr(x), Type::FuncPtr(y)) => Arc::ptr_eq(&x, &y),
            _ => false,
        }
    }
}

impl std::cmp::Eq for TypeEq {}

/// Maps SAILAR type and function signatures to LLVM types.
#[derive(Debug)]
pub struct Cache<'module, 'context> {
    module: &'module inkwell::module::Module<'context>,
    basic_types: RefCell<rustc_hash::FxHashMap<TypeEq, LlvmBasicType<'context>>>,
    function_types: RefCell<rustc_hash::FxHashMap<ArcEq<sailar_load::function::Signature>, LlvmFunctonType<'context>>>,
}

impl<'module, 'context> Cache<'module, 'context> {
    pub fn new(module: &'module inkwell::module::Module<'context>) -> Self {
        Self {
            module,
            basic_types: Default::default(),
            function_types: Default::default(),
        }
    }

    pub fn module(&self) -> &'module inkwell::module::Module<'context> {
        &self.module
    }

    pub fn get_basic_type(&self, ty: Type) -> LlvmBasicType<'context> {
        match self.basic_types.borrow_mut().entry(TypeEq(ty)) {
            hash_map::Entry::Occupied(occupied) => *occupied.get(),
            hash_map::Entry::Vacant(vacant) => {
                todo!("insert the type")
            }
        }
    }
}

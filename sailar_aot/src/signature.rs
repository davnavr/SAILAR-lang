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
            (Type::FixedInteger(x), Type::FixedInteger(y)) => x.size() == y.size(),
            (Type::F32, Type::F32)
            | (Type::F64, Type::F64)
            | (Type::UAddr | Type::SAddr, Type::UAddr | Type::SAddr)
            | (Type::RawPtr(None), Type::RawPtr(None)) => true,
            (Type::RawPtr(Some(x)), Type::RawPtr(Some(y))) => Arc::ptr_eq(&x, &y),
            (Type::FuncPtr(x), Type::FuncPtr(y)) => Arc::ptr_eq(&x, &y),
            _ => false,
        }
    }
}

impl std::cmp::Eq for TypeEq {}

impl Hash for TypeEq {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self.0 {
            Type::FixedInteger(itype) => itype.size().hash(state),
            Type::UAddr | Type::SAddr => state.write_u8(0xAA),
            Type::F32 => state.write_u8(0xF4),
            Type::F64 => state.write_u8(0xF8),
            Type::RawPtr(None) => state.write_u8(0xCA),
            Type::RawPtr(Some(raw)) => Arc::as_ptr(&raw).hash(state),
            Type::FuncPtr(func) => Arc::as_ptr(&func).hash(state),
        }
    }
}

/// Maps SAILAR type and function signatures to LLVM types.
#[derive(Debug)]
pub struct Cache<'context> {
    module: &'module inkwell::module::Module<'context>,
    target_data: &'module inkwell::targets::TargetData,
    basic_types: RefCell<rustc_hash::FxHashMap<TypeEq, LlvmBasicType<'context>>>,
    function_types: RefCell<rustc_hash::FxHashMap<ArcEq<sailar_load::function::Signature>, LlvmFunctonType<'context>>>,
}

impl<'module, 'context> Cache<'module, 'context> {
    pub fn new(module: &'module inkwell::module::Module, target_data: &'module inkwell::targets::TargetData) -> Self {
        Self {
            module,
            target_data,
            basic_types: Default::default(),
            function_types: Default::default(),
        }
    }

    pub fn module(&self) -> &'module inkwell::module::Module<'context> {
        &self.module
    }

    //pub fn get_function_type

    pub fn get_basic_type(&self, ty: Type) -> LlvmBasicType<'context> {
        match self.basic_types.borrow_mut().entry(TypeEq(ty)) {
            hash_map::Entry::Occupied(occupied) => *occupied.get(),
            hash_map::Entry::Vacant(vacant) => {
                let context = self.module.get_context();
                *vacant.insert(match vacant.key().0 {
                    Type::FixedInteger(integer_type) => context
                        .custom_width_int_type(integer_type.size().bit_size().get().into())
                        .into(),
                    Type::F32 => context.f32_type().into(),
                    Type::F64 => context.f64_type().into(),
                    Type::UAddr | Type::SAddr => context.ptr_sized_int_type(self.target_data, None).into(),
                    Type::RawPtr(None) => context.i8_type().ptr_type(None),
                    bad => todo!("add support for {:?}", bad),
                })
            }
        }
    }
}

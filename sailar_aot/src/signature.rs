//! Module to translate SAILAR types and function signatures into LLVM types.

use crate::compilation::Result;
use crate::helper::ptr::ArcEq;
use inkwell::types::{
    BasicMetadataTypeEnum as LlvmMetadataType, BasicTypeEnum as LlvmBasicType, FunctionType as LlvmFunctionType,
};
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
        match (&self.0, &other.0) {
            (Type::FixedInteger(x), Type::FixedInteger(y)) => x.size() == y.size(),
            (Type::F32, Type::F32)
            | (Type::F64, Type::F64)
            | (Type::UAddr | Type::SAddr, Type::UAddr | Type::SAddr)
            | (Type::RawPtr(None), Type::RawPtr(None)) => true,
            (Type::RawPtr(Some(x)), Type::RawPtr(Some(y))) => Arc::ptr_eq(x, y),
            (Type::FuncPtr(x), Type::FuncPtr(y)) => Arc::ptr_eq(x, y),
            _ => false,
        }
    }
}

impl std::cmp::Eq for TypeEq {}

impl Hash for TypeEq {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self.0 {
            Type::FixedInteger(itype) => itype.size().hash(state),
            Type::UAddr | Type::SAddr => state.write_u8(0xAA),
            Type::F32 => state.write_u8(0xF4),
            Type::F64 => state.write_u8(0xF8),
            Type::RawPtr(None) => state.write_u8(0xCA),
            Type::RawPtr(Some(raw)) => Arc::as_ptr(raw).hash(state),
            Type::FuncPtr(func) => Arc::as_ptr(func).hash(state),
        }
    }
}

/// Maps SAILAR type and function signatures to LLVM types.
pub struct Cache<'module, 'context> {
    context: &'context inkwell::context::Context,
    target_data: &'module inkwell::targets::TargetData,
    basic_types: RefCell<rustc_hash::FxHashMap<TypeEq, LlvmBasicType<'context>>>,
    function_types: RefCell<rustc_hash::FxHashMap<ArcEq<sailar_load::function::Signature>, LlvmFunctionType<'context>>>,
}

impl<'module, 'context> Cache<'module, 'context> {
    pub fn new(context: &'context inkwell::context::Context, target_data: &'module inkwell::targets::TargetData) -> Self {
        Self {
            context,
            target_data,
            basic_types: Default::default(),
            function_types: Default::default(),
        }
    }

    pub fn get_function_type(&self, signature: Arc<sailar_load::function::Signature>) -> Result<LlvmFunctionType<'context>> {
        Ok(match self.function_types.borrow_mut().entry(ArcEq::from(signature)) {
            hash_map::Entry::Occupied(occupied) => *occupied.get(),
            hash_map::Entry::Vacant(vacant) => {
                let signature = vacant.key();
                let argument_types = signature.parameter_types()?;

                let mut parameter_types = Vec::with_capacity(argument_types.len());

                for ty in argument_types {
                    parameter_types.push(LlvmMetadataType::from(self.get_basic_type(ty.signature()?.clone())?));
                }

                fn construct_function_type<'c>(
                    return_type: LlvmBasicType<'c>,
                    parameter_types: &[LlvmMetadataType<'c>],
                ) -> LlvmFunctionType<'c> {
                    match return_type {
                        LlvmBasicType::ArrayType(arr) => arr.fn_type(parameter_types, false),
                        LlvmBasicType::FloatType(ftype) => ftype.fn_type(parameter_types, false),
                        LlvmBasicType::IntType(itype) => itype.fn_type(parameter_types, false),
                        LlvmBasicType::PointerType(ptype) => ptype.fn_type(parameter_types, false),
                        LlvmBasicType::StructType(stype) => stype.fn_type(parameter_types, false),
                        LlvmBasicType::VectorType(vtype) => vtype.fn_type(parameter_types, false),
                    }
                }

                match signature.return_types()? {
                    [] => self.context.void_type().fn_type(&parameter_types, false),
                    [return_type] => {
                        construct_function_type(self.get_basic_type(return_type.signature()?.clone())?, &parameter_types)
                    }
                    _ => todo!("multiple return types not yet supported (need to make a struct)"),
                }
            }
        })
    }

    pub fn get_basic_type(&self, ty: Type) -> Result<LlvmBasicType<'context>> {
        Ok(match self.basic_types.borrow_mut().entry(TypeEq(ty)) {
            hash_map::Entry::Occupied(occupied) => *occupied.get(),
            hash_map::Entry::Vacant(vacant) => {
                let ty = match &vacant.key().0 {
                    Type::FixedInteger(integer_type) => self
                        .context
                        .custom_width_int_type(integer_type.size().bit_size().get().into())
                        .into(),
                    Type::F32 => self.context.f32_type().into(),
                    Type::F64 => self.context.f64_type().into(),
                    Type::UAddr | Type::SAddr => self.context.ptr_sized_int_type(self.target_data, None).into(),
                    Type::RawPtr(None) => self.context.i8_type().ptr_type(inkwell::AddressSpace::Generic).into(),
                    // Note: RawPtr and FuncPtr may require recursive calls, will conflict with borrowing of RefCell.
                    bad => todo!("add support for {:?}", bad),
                };

                *vacant.insert(ty)
            }
        })
    }
}

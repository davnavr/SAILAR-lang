//! Module for interacting with SAILAR code blocks.

use crate::error;
use crate::module;
use crate::type_system;
use sailar::instruction::{self, Instruction};
use std::fmt::{Debug, Formatter};
use std::sync::{Arc, Weak};

#[derive(Clone, Debug)]
pub struct TypedValue {
    value_type: type_system::Type,
    value: instruction::Value,
}

impl TypedValue {
    fn new(value_type: type_system::Type, value: instruction::Value) -> Self {
        Self { value_type, value }
    }

    pub fn value_type(&self) -> &type_system::Type {
        &self.value_type
    }

    pub fn raw_value(&self) -> &instruction::Value {
        &self.value
    }
}

/// Represents a SAILAR instruction with type information.
#[derive(Clone, Debug)]
pub enum TypedInstruction {
    Nop,
    Break,
    Ret(Box<[TypedValue]>),
}

pub struct Code {
    input_count: usize,
    result_count: usize,
    register_types: type_system::LazySignatureList,
    untyped_instructions: Box<[Instruction]>,
    typed_instructions: lazy_init::Lazy<Result<Box<[TypedInstruction]>, error::LoaderError>>,
    index: sailar::index::CodeBlock,
    module: Weak<module::Module>,
}

impl Code {
    pub(crate) fn new(
        code: sailar::record::CodeBlock<'static>,
        index: sailar::index::CodeBlock,
        module: Weak<module::Module>,
    ) -> Arc<Self> {
        Arc::new(Self {
            input_count: code.input_count,
            result_count: code.result_count,
            register_types: type_system::LazySignatureList::new(code.register_types.into_boxed()),
            untyped_instructions: code.instructions.into_boxed(),
            typed_instructions: Default::default(),
            index,
            module,
        })
    }

    pub fn module(&self) -> &Weak<module::Module> {
        &self.module
    }

    pub fn index(&self) -> sailar::index::CodeBlock {
        self.index
    }

    /// The types of all input registers, results, and temporary registers in that order.
    pub fn all_types(&self) -> Result<&[Arc<type_system::Signature>], error::LoaderError> {
        self.register_types.get_or_initialize(&self.module)
    }

    pub fn input_types(&self) -> Result<&[Arc<type_system::Signature>], error::LoaderError> {
        self.all_types().map(|types| &types[0..self.input_count])
    }

    pub fn result_types(&self) -> Result<&[Arc<type_system::Signature>], error::LoaderError> {
        self.all_types()
            .map(|types| &types[self.input_count..self.input_count + self.result_count])
    }

    pub fn temporary_types(&self) -> Result<&[Arc<type_system::Signature>], error::LoaderError> {
        self.all_types().map(|types| &types[self.input_count + self.result_count..])
    }

    pub fn untyped_instructions(&self) -> &[Instruction] {
        &self.untyped_instructions
    }

    pub fn typed_instructions(&self) -> Result<&[TypedInstruction], error::LoaderError> {
        self.typed_instructions
            .get_or_create(|| {
                let module = module::Module::upgrade_weak(&self.module)?;
                let result_types = self.result_types()?;
                let mut typed_instructions = Vec::with_capacity(self.untyped_instructions.len());

                for instruction in self.untyped_instructions.iter() {
                    typed_instructions.push(match instruction {
                        Instruction::Nop => TypedInstruction::Nop,
                        Instruction::Break => TypedInstruction::Break,
                        Instruction::Ret(values) => {
                            assert!(values.len() == result_types.len());

                            let mut return_values = Vec::with_capacity(values.len());

                            for (value, return_type) in values.iter().zip(result_types) {
                                return_values.push(TypedValue::new(return_type.signature()?.clone(), value.clone()));
                            }

                            TypedInstruction::Ret(return_values.into_boxed_slice())
                        }
                        bad => todo!("translate {:?}", bad),
                    })
                }

                Ok(typed_instructions.into_boxed_slice())
            })
            .as_ref()
            .map(AsRef::as_ref)
            .map_err(Clone::clone)
    }
}

impl Debug for Code {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Code")
            .field("index", &self.index)
            .field("register_types", &self.register_types)
            .field("untyped_instructions", &self.untyped_instructions)
            .finish()
    }
}

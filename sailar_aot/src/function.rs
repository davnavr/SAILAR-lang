//! Module for generating LLVM function definitions corresponding to SAILAR function instantiations.

use inkwell::values::FunctionValue as LlvmFunction;
use sailar_load::function;
use sailar_load::helper::ptr::ArcEq;
use std::sync::Arc;

/// Maps LLVM function definitions to SAILAR function instantiations.
#[derive(Debug)]
pub struct Lookup<'module, 'context> {
    module: &'module inkwell::module::Module<'context>,
    functions: rustc_hash::FxHashMap<ArcEq<function::Instantiation>, LlvmFunction>,
    //undefined_functions,
}

impl<'module, 'context> Lookup<'module, 'context> {
    pub fn new(module: &'module inkwell::module::Module<'context>) -> Self {
        Self {
            module,
            functions: Default::default(),
        }
    }

    pub fn get_or_define(&mut self, instantiation: Arc<function::Instantiation>) -> LlvmFunction {
        todo!()
    }
}

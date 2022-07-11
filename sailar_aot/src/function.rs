//! Module for generating LLVM function definitions corresponding to SAILAR function instantiations.

use inkwell::values::FunctionValue as LlvmFunction;
use sailar_load::function;
use std::sync::Arc;

/// Maps LLVM function definitions to SAILAR function instantiations.
pub struct Lookup<'module, 'context> {
    module: &'module inkwell::module::Module<'context>,
    //functions: rustc_hash::FxHashMap
    //undefined_functions,
}

impl<'module, 'context> Lookup<'module, 'context> {
    pub fn new(module: &'module inkwell::module::Module<'context>) -> Self {
        Self { module }
    }

    pub fn get_or_define(&mut self, instantiation: Arc<function::Instantiation>) -> LlvmFunction {
        todo!()
    }
}

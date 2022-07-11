//! Module for generating LLVM function definitions corresponding to SAILAR function instantiations.

use crate::helper::ptr::ArcEq;
use inkwell::values::FunctionValue as LlvmFunction;
use sailar_load::function;
use std::cell::RefCell;
use std::collections::hash_map;
use std::sync::Arc;

/// Maps SAILAR function instantiations to LLVM function definitions.
#[derive(Debug)]
pub struct Cache<'module, 'context> {
    module: &'module inkwell::module::Module<'context>,
    functions: RefCell<rustc_hash::FxHashMap<ArcEq<function::Instantiation>, LlvmFunction<'context>>>,
    undefined_functions: Vec<(Arc<function::Instantiation>, LlvmFunction<'context>)>,
}

impl<'module, 'context> Cache<'module, 'context> {
    pub fn new(module: &'module inkwell::module::Module<'context>) -> Self {
        Self {
            module,
            functions: Default::default(),
            undefined_functions: Vec::new(),
        }
    }

    pub fn get_or_define(&mut self, instantiation: Arc<function::Instantiation>) -> LlvmFunction<'context> {
        match self.functions.borrow_mut().entry(ArcEq::from(instantiation)) {
            hash_map::Entry::Occupied(occupied) => *occupied.get(),
            hash_map::Entry::Vacant(vacant) => {
                todo!("insert the function")
            }
        }
    }
}

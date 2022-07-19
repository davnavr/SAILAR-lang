//! Module for generating LLVM function definitions corresponding to SAILAR function instantiations.

use crate::compilation::Result;
use crate::helper::ptr::ArcEq;
use inkwell::values::FunctionValue as LlvmFunction;
use sailar_load::function;
use std::cell::RefCell;
use std::collections::hash_map;
use std::sync::Arc;

/// Maps SAILAR function instantiations to LLVM function definitions.
pub struct Cache<'types, 'module, 'context> {
    module: &'module inkwell::module::Module<'context>,
    type_cache: &'types crate::signature::Cache<'module, 'context>,
    functions: RefCell<rustc_hash::FxHashMap<ArcEq<function::Function>, LlvmFunction<'context>>>,
    undefined_functions: RefCell<Vec<(Arc<function::Function>, LlvmFunction<'context>)>>,
}

impl<'types, 'module, 'context> Cache<'types, 'module, 'context> {
    pub fn new(
        module: &'module inkwell::module::Module<'context>,
        type_cache: &'types crate::signature::Cache<'module, 'context>,
    ) -> Self {
        Self {
            module,
            type_cache,
            functions: Default::default(),
            undefined_functions: Default::default(),
        }
    }

    pub fn get_or_define(&self, instantiation: Arc<function::Function>) -> Result<LlvmFunction<'context>> {
        Ok(match self.functions.borrow_mut().entry(ArcEq::from(instantiation)) {
            hash_map::Entry::Occupied(occupied) => *occupied.get(),
            hash_map::Entry::Vacant(vacant) => {
                let instantiation = vacant.key();
                let template = vacant.key().template()?.as_definition()?;
                let signature = self.type_cache.get_function_type(template.signature()?.clone())?;

                let linkage = if template.export().kind() == sailar::record::ExportKind::Export {
                    inkwell::module::Linkage::External
                } else { 
                    inkwell::module::Linkage::Private
                };

                let function = self.module.add_function(
                    &crate::name_mangling::mangle(instantiation.as_ref())?,
                    signature,
                    Some(linkage),
                );

                self.undefined_functions
                    .borrow_mut()
                    .push((Arc::from(ArcEq::clone(vacant.key())), function));

                *vacant.insert(function)
            }
        })
    }

    pub fn next_undefined(&self) -> Option<(Arc<function::Function>, LlvmFunction<'context>)> {
        self.undefined_functions.borrow_mut().pop()
    }
}

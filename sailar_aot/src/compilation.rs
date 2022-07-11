//! Module for translating from SAILAR modules to LLVM modules.

use crate::error::{CompilationError, CompilationErrorKind};
use inkwell::context::Context as LlvmContext;
use inkwell::module::Module as LlvmModule;
use inkwell::targets::{Target, TargetMachine};
use sailar_load::module::Module;
use std::sync::Arc;

pub type Result<T> = std::result::Result<T, CompilationError>;

/// Specifies how the name of the output LLVM module is determined.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum OutputName {
    /// The name of the first SAILAR module used as input is used.
    Inferred,
    Specified(String),
}

impl Default for OutputName {
    fn default() -> Self {
        Self::Inferred
    }
}

#[derive(Debug)]
pub struct Inputs {
    target_machine: Option<TargetMachine>,
    modules: Vec<Arc<Module>>,
    output_name: OutputName,
}

impl Inputs {
    pub fn new() -> Self {
        Self {
            target_machine: None,
            modules: Default::default(),
            output_name: Default::default(),
        }
    }

    /// Includes the specified `modules` in this compilation.
    pub fn with_modules<M: IntoIterator<Item = Arc<Module>>>(mut self, modules: M) -> Self {
        self.modules.extend(modules);
        self
    }

    /// Sets the target machine for the compilation.
    pub fn target_machine(self, target_machine: TargetMachine) -> Self {
        Self {
            target_machine: Some(target_machine),
            ..self
        }
    }

    pub fn compile(mut self, context: &LlvmContext) -> Result<Compilation<'_>> {
        let target_machine = match self.target_machine {
            Some(machine) => machine,
            None => {
                let host_triple = TargetMachine::get_default_triple();
                Target::create_target_machine(
                    &Target::from_triple(&host_triple).map_err(|s| CompilationErrorKind::InvalidTargetTriple(s.to_string()))?,
                    &host_triple,
                    &TargetMachine::get_host_cpu_name().to_string(),
                    &TargetMachine::get_host_cpu_features().to_string(),
                    inkwell::OptimizationLevel::Default,
                    inkwell::targets::RelocMode::Default,
                    inkwell::targets::CodeModel::Default,
                )
                .unwrap()
            }
        };

        let output_module_name;
        let output_module = context.create_module(match self.output_name {
            OutputName::Inferred => self
                .modules
                .first()
                .and_then(|module| module.module_identifier())
                .map(|id| id.name().as_str())
                .unwrap_or("<unnamed>"),
            OutputName::Specified(name) => {
                output_module_name = name;
                &output_module_name
            }
        });

        todo!("hey");

        Ok(Compilation {
            output_module,
            input_modules: self.modules,
        })
    }
}

impl Default for Inputs {
    fn default() -> Self {
        Self::new()
    }
}

/// Represents an LLVM module containing LLVM IR corresponding to one or more SAILAR modules.
#[derive(Debug)]
pub struct Compilation<'ctx> {
    output_module: LlvmModule<'ctx>,
    input_modules: Vec<Arc<Module>>,
}

impl<'ctx> Compilation<'ctx> {
    pub fn with_inputs(inputs: Inputs, context: &'ctx LlvmContext) -> Result<Self> {
        inputs.compile(context)
    }

    pub fn into_llvm_module(self) -> LlvmModule<'ctx> {
        self.output_module
    }
}

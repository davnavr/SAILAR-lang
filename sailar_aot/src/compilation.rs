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

pub struct Inputs {
    target_machine: Option<TargetMachine>,
    sources: Vec<sailar_load::source::BoxedSource>,
    resolver: Option<sailar_load::resolver::BoxedResolver>,
    output_name: OutputName,
}

impl Inputs {
    pub fn new() -> Self {
        Self {
            target_machine: None,
            sources: Default::default(),
            resolver: None,
            output_name: Default::default(),
        }
    }

    /// Explicitly includes the specified modules in this compilation.
    pub fn with_modules<S, M>(mut self, modules: M) -> Self
    where
        S: sailar_load::source::Source + 'static,
        S::Error: std::error::Error + 'static,
        M: IntoIterator<Item = S>,
    {
        self.sources.extend(modules.map(sailar_load::source::boxed));
        self
    }

    pub fn module_resolver<R>(self, resolver: R) -> Self
    where
        R: sailar_load::Resolver + Send + 'static,
        R::Error: std::error::Error,
    {
        Self {
            resolver: Some(resolver),
            ..self
        }
    }

    /// Sets the target machine for the compilation.
    pub fn target_machine(self, target_machine: TargetMachine) -> Self {
        Self {
            target_machine: Some(target_machine),
            ..self
        }
    }

    /// Given the specified input, compiles the SAILAR code into an LLVM module.
    pub fn compile(mut self, context: &LlvmContext) -> Result<Compilation<'_>> {
        let target_triple;
        let target_machine;

        match self.target_machine {
            Some(machine) => {
                target_machine = machine;
                target_triple = target_machine.get_triple();
            }
            None => {
                target_triple = TargetMachine::get_default_triple();
                target_machine = Target::create_target_machine(
                    &Target::from_triple(&target_triple).map_err(|s| CompilationErrorKind::InvalidTargetTriple(s.to_string()))?,
                    &target_triple,
                    &TargetMachine::get_host_cpu_name().to_string(),
                    &TargetMachine::get_host_cpu_features().to_string(),
                    inkwell::OptimizationLevel::Default,
                    inkwell::targets::RelocMode::Default,
                    inkwell::targets::CodeModel::Default,
                )
                .unwrap();
            }
        }

        let target_data = target_machine.get_target_data();

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

        output_module.set_triple(&target_triple);

        let state = sailar_load::state::Builder::new()
            .resolver(self.resolver)
            .address_size(sailar_load::state::AddressSize::with_byte_size(&target_data))
            .create();

        let input_modules = {
            let mut modules = Vec::with_capacity(self.sources.len());
            for source in self.sources {
                match state.force_load_module(source)? {
                    Some(module) => modules.push(module),
                    None => todo!("make error for duplicate module"),
                }
            }
            modules.into_boxed_slice()
        };

        // TODO: Have a hashmap that initially contains all function exports, but will then be gradually filled by all referenced functions
        // This should also help keep track of which functions have yet to be translated
        todo!("hey");

        Ok(Compilation {
            output_module,
            input_modules,
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
    input_modules: Box<[Arc<Module>]>,
}

impl<'ctx> Compilation<'ctx> {
    /// Compiles the specified `inputs`. Alias for [`Inputs::compile`].
    #[inline]
    pub fn with_inputs(inputs: Inputs, context: &'ctx LlvmContext) -> Result<Self> {
        inputs.compile(context)
    }

    pub fn into_llvm_module(self) -> LlvmModule<'ctx> {
        self.output_module
    }
}

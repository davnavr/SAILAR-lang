//! Module for mangling names.

use sailar_load::error::LoaderError;
use sailar_load::module::{Export, Module};
use std::fmt::Write;
use std::sync::Arc;

fn mangle_module_name(module: &Arc<Module>, buffer: &mut String) {
    match module.module_identifiers().iter().next() {
        Some(identifier) => {
            buffer.push_str(identifier.name().as_str());
            for n in identifier.version().iter() {
                write!(buffer, "_{}", n).unwrap();
            }
        }
        None => {
            // TODO: Use a hash of the module's contents instead if no name is available to be mangled.
            write!(buffer, "anonymous{:p}", module).unwrap();
        }
    }
}

pub trait Definition {
    fn index(&self) -> usize;

    fn export(&self) -> Result<&Export, LoaderError>;

    fn module(&self) -> &std::sync::Weak<Module>;

    fn anonymous_prefix() -> &'static str;
}

impl Definition for sailar_load::function::Function {
    fn index(&self) -> usize {
        self.index().into()
    }

    fn export(&self) -> Result<&Export, LoaderError> {
        Ok(self.template()?.as_definition()?.export())
    }

    fn module(&self) -> &std::sync::Weak<Module> {
        self.module()
    }

    fn anonymous_prefix() -> &'static str {
        "F"
    }
}

pub fn mangle<D: Definition>(definition: &D) -> Result<String, LoaderError> {
    let mut buffer = String::new();
    let module = Module::upgrade_weak(definition.module())?;
    mangle_module_name(&module, &mut buffer);

    buffer.push('_');

    if let Some(symbol) = definition.export()?.symbol() {
        buffer.push_str(symbol.as_str());
    } else {
        buffer.push_str(D::anonymous_prefix());
        write!(&mut buffer, "{}", definition.index()).unwrap();
    }

    Ok(buffer)
}

use super::*;

pub struct Function<'a> {
    source: &'a format::Function,
    module: &'a Module<'a>,
}

#[derive(PartialEq)]
pub struct Signature<'a> {
    return_types: Vec<&'a format::TypeSignature>,
    parameter_types: Vec<&'a format::TypeSignature>,
}

impl<'a> Signature<'a> {
    pub(crate) fn new(
        return_types: Vec<&'a format::TypeSignature>,
        parameter_types: Vec<&'a format::TypeSignature>,
    ) -> Self {
        Self {
            return_types,
            parameter_types,
        }
    }

    pub fn return_types(&self) -> &[&'a format::TypeSignature] {
        &self.return_types
    }

    pub fn parameter_types(&self) -> &[&'a format::TypeSignature] {
        &self.parameter_types
    }
}

impl<'a> Function<'a> {
    pub(crate) fn new(module: &'a Module<'a>, source: &'a format::Function) -> Self {
        Self { source, module }
    }

    pub fn is_export(&'a self) -> bool {
        self.source.is_export
    }

    pub fn symbol(&'a self) -> Result<&'a Identifier> {
        self.module.load_identifier_raw(self.source.symbol)
    }

    pub fn declaring_module(&'a self) -> &'a Module<'a> {
        self.module
    }

    pub fn raw_body(&'a self) -> &'a format::FunctionBody {
        &self.source.body
    }

    pub fn raw_signature(&'a self) -> Result<&'a format::FunctionSignature> {
        self.module
            .load_function_signature_raw(self.source.signature)
    }

    pub fn signature(&'a self) -> Result<&'a Signature<'a>> {
        self.module.load_function_signature(self.source.signature)
    }
}

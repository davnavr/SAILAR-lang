use crate::loader::{self, cache, Result};
use sailar::format;

pub struct Function<'a> {
    module: &'a loader::Module<'a>,
    source: &'a format::Function,
    index: format::indices::FunctionDefinition,
    code: cache::Once<Option<&'a loader::Code<'a>>>,
}

#[derive(Eq, PartialEq)]
pub struct Signature<'a> {
    return_types: Vec<&'a loader::TypeSignature<'a>>,
    parameter_types: Vec<&'a loader::TypeSignature<'a>>,
}

impl<'a> Signature<'a> {
    pub(crate) fn new(
        return_types: Vec<&'a loader::TypeSignature<'a>>,
        parameter_types: Vec<&'a loader::TypeSignature<'a>>,
    ) -> Self {
        Self {
            return_types,
            parameter_types,
        }
    }

    pub fn return_types(&'a self) -> &[&'a loader::TypeSignature<'a>] {
        &self.return_types
    }

    pub fn parameter_types(&'a self) -> &[&'a loader::TypeSignature<'a>] {
        &self.parameter_types
    }

    pub fn into_raw(&'a self) -> format::FunctionSignature {
        fn map_raw<'a>(
            types: &[&'a loader::TypeSignature<'a>],
        ) -> format::LenVec<format::indices::TypeSignature> {
            format::LenVec(types.iter().map(|signature| signature.index()).collect())
        }

        format::FunctionSignature {
            return_types: map_raw(self.return_types()),
            parameter_types: map_raw(self.parameter_types()),
        }
    }
}

impl<'a> Function<'a> {
    pub(super) fn new(
        module: &'a loader::Module<'a>,
        source: &'a format::Function,
        index: format::indices::FunctionDefinition,
    ) -> Self {
        Self {
            source,
            module,
            index,
            code: cache::Once::default(),
        }
    }

    pub fn index(&'a self) -> format::indices::FunctionDefinition {
        self.index
    }

    pub fn is_export(&'a self) -> bool {
        self.source.is_export
    }

    pub fn symbol(&'a self) -> Result<&'a loader::Identifier> {
        self.module.load_identifier_raw(self.source.symbol)
    }

    pub fn full_symbol(&'a self) -> Result<loader::FunctionSymbol<'a>> {
        Ok(loader::FunctionSymbol::new(
            self.module.full_symbol(),
            loader::Symbol::Borrowed(self.symbol()?),
        ))
    }

    pub fn declaring_module(&'a self) -> &'a loader::Module<'a> {
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

    /// Returns the code associated with the function, if its body is defined in the declaring module.
    pub fn code(&'a self) -> Result<Option<&'a loader::Code<'a>>> {
        self.code
            .get_or_insert_fallible(|| match self.raw_body() {
                format::FunctionBody::Defined(index) => {
                    // TODO: Check that the parameter types match the code's types.
                    Ok(Some(self.module.load_code_raw(*index)?))
                }
                format::FunctionBody::External { .. } => Ok(None),
            })
            .map(|result| *result)
    }
}

impl<'a> std::fmt::Debug for &'a Function<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("is_export", &self.is_export())
            .field("symbol", &self.symbol().ok())
            .finish()
    }
}

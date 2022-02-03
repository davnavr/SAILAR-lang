use crate::builder;
use crate::format;
use std::rc::Rc;

pub struct Imports {
    imports: std::cell::RefCell<Vec<Rc<Function>>>,
    index: builder::counter::Cell<format::indices::FunctionImport>,
}

impl Imports {
    pub(super) fn new() -> Self {
        Self {
            imports: std::cell::RefCell::new(Vec::new()),
            index: builder::counter::Cell::new(),
        }
    }

    pub fn import(
        &self,
        module: Rc<builder::ModuleImport>,
        symbol: format::Identifier,
        signature: Rc<builder::FunctionSig>,
    ) -> Rc<Function> {
        let function = Rc::new(Function {
            index: self.index.next(),
            module,
            symbol,
            signature,
        });

        self.imports.borrow_mut().push(function.clone());
        function
    }

    pub(super) fn build(
        &self,
        identifiers: &mut builder::identifiers::Identifiers,
    ) -> Vec<format::FunctionImport> {
        self.imports
            .borrow()
            .iter()
            .map(|import| format::FunctionImport {
                module: import.module.index(),
                symbol: identifiers.insert_or_get(import.symbol.clone()),
                signature: import.signature.index(),
            })
            .collect()
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub struct Function {
    index: format::indices::FunctionImport,
    module: Rc<builder::ModuleImport>,
    symbol: format::Identifier,
    signature: Rc<builder::FunctionSig>,
}

impl Function {
    pub fn index(&self) -> format::indices::FunctionImport {
        self.index
    }

    pub fn signature(&self) -> &builder::FunctionSig {
        &self.signature
    }
}

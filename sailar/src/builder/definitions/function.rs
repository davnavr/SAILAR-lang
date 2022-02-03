use crate::builder;
use crate::format;
use std::rc::Rc;

pub struct Definitions {
    definitions: std::cell::RefCell<Vec<Rc<Function>>>,
    index: builder::counter::Cell<format::indices::FunctionDefinition>,
}

impl Definitions {
    pub fn new() -> Self {
        Self {
            definitions: std::cell::RefCell::new(Vec::new()),
            index: builder::counter::Cell::new(),
        }
    }

    pub fn define(
        &self,
        symbol: format::Identifier,
        signature: Rc<builder::FunctionSig>,
        body: Body,
    ) -> Rc<Function> {
        let function = Rc::new(Function::new(self.index.next(), symbol, signature, body));
        self.definitions.borrow_mut().push(function.clone());
        function
    }

    pub(super) fn build(
        &self,
        identifiers: &mut builder::identifiers::Identifiers,
    ) -> Vec<format::Function> {
        self.definitions
            .borrow()
            .iter()
            .map(|function| function.build(identifiers))
            .collect()
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub struct Function {
    index: format::indices::FunctionDefinition,
    symbol: format::Identifier,
    // TODO Combine name and is_export into one struct so it can be wrapped in a RefCell.
    name: Option<format::Identifier>,
    signature: Rc<builder::FunctionSig>,
    body: Body,
    // Could be useful to store references to parameter and return types here, to help with type checking in block builder
    is_export: std::cell::Cell<bool>,
}

#[derive(Debug)]
#[non_exhaustive]
pub enum Body {
    Defined(Rc<builder::Code>),
}

impl Body {
    fn build(&self) -> format::FunctionBody {
        match self {
            Self::Defined(code) => format::FunctionBody::Defined(code.index()),
        }
    }
}

impl Function {
    fn new(
        index: format::indices::FunctionDefinition,
        symbol: format::Identifier,
        signature: Rc<builder::FunctionSig>,
        body: Body,
    ) -> Self {
        Self {
            index,
            symbol,
            name: None,
            signature,
            body,
            is_export: std::cell::Cell::new(false),
        }
    }

    pub fn index(&self) -> format::indices::FunctionDefinition {
        self.index
    }

    pub fn signature(&self) -> &builder::FunctionSig {
        &self.signature
    }

    pub fn is_export(&self, exported: bool) {
        self.is_export.set(exported)
    }

    //pub fn set_name(&self, name: )

    pub fn build(&self, identifiers: &mut builder::identifiers::Identifiers) -> format::Function {
        let symbol = identifiers.insert_or_get(self.symbol.clone());

        format::Function {
            name: self
                .name
                .as_ref()
                .map(|name| identifiers.insert_or_get(name.clone()))
                .unwrap_or(symbol),
            signature: self.signature.index(),
            is_export: self.is_export.get(),
            symbol,
            body: self.body.build(),
        }
    }
}

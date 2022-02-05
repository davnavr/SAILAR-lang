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
        // TODO: Check that return count of body is correct.
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

#[derive(Clone, Debug)]
pub struct ExternalFunction {
    library: Rc<format::Identifier>,
    symbol: format::Identifier,
}

impl ExternalFunction {
    pub fn new(library: Rc<format::Identifier>, symbol: format::Identifier) -> Self {
        Self { library, symbol }
    }

    pub fn library(&self) -> &Rc<format::Identifier> {
        &self.library
    }

    pub fn symbol(&self) -> &format::Identifier {
        &self.symbol
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub enum Body {
    Defined(Rc<builder::Code>),
    External(Box<ExternalFunction>),
}

impl From<ExternalFunction> for Body {
    fn from(body: ExternalFunction) -> Self {
        Self::External(Box::new(body))
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
            body: match &self.body {
                Body::Defined(code) => format::FunctionBody::Defined(code.index()),
                Body::External(external) => format::FunctionBody::External {
                    library: identifiers.insert_or_get((*external.library).clone()),
                    entry_point_name: identifiers.insert_or_get(external.symbol.clone()),
                },
            },
        }
    }
}

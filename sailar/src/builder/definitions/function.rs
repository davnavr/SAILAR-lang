use crate::builder;
use crate::format;

pub struct Definitions {
    definitions: typed_arena::Arena<Function>,
    index: builder::counter::Cell<format::indices::FunctionDefinition>,
}

impl Definitions {
    pub fn new() -> Self {
        Self {
            definitions: typed_arena::Arena::new(),
            index: builder::counter::Cell::new(),
        }
    }

    pub fn build(&mut self) -> Vec<format::Function> {
        self.definitions.iter_mut().map(Function::build).collect()
    }
}

pub struct Function {
    index: format::indices::FunctionDefinition,
    symbol: format::Identifier,
    name: Option<format::Identifier>,
    body: format::FunctionBody,
    // Could be useful to store references to parameter and return types here, to help with type checking in block builder
}

#[non_exhaustive]
pub enum Body<'a> {
    Defined(&'a builder::Code),
}

impl Body<'_> {
    fn build(&self) -> format::FunctionBody {
        match self {
            Self::Defined(code) => format::FunctionBody::Defined(code.index()),
        }
    }
}

impl<'a, 'b> builder::definitions::Functions<'a>
where
    'a: 'b,
{
    // TODO: How to specify body in a way that allows self-referential functions?
    pub fn define(&'b mut self, symbol: format::Identifier, body: &Body<'a>) -> Builder<'b> {
        let function = self.functions.definitions.alloc(Function {
            index: self.functions.index.next(),
            symbol,
            name: None,
            body: body.build(),
        });

        Builder {
            builder: self.builder,
            function,
            type_signatures: self.type_signatures,
        }
    }
}

impl Function {
    pub fn new(
        index: format::indices::FunctionDefinition,
        symbol: format::Identifier,
        body: format::FunctionBody,
    ) -> Self {
        Self {
            index,
            symbol,
            name: None,
            body,
        }
    }

    pub fn build(&mut self) -> format::Function {
        format::Function {
            name: todo!(), //self.name.unwrap_or(self.symbol),
            signature: todo!(),
            is_export: todo!(),
            symbol: todo!(), //self.symbol,
            body: self.body,
        }
    }
}

pub struct Builder<'a> {
    builder: &'a (),
    function: &'a mut Function,
    type_signatures: &'a mut builder::type_signatures::Signatures,
}

impl<'a, 'b> Builder<'a>
where
    'a: 'b,
{
    fn new(
        builder: &'a (),
        function: &'a mut Function,
        type_signatures: &'a mut builder::type_signatures::Signatures,
    ) -> Self {
        Self {
            builder,
            function,
            type_signatures,
        }
    }

    //pub fn set_name

    pub fn finish(self) -> &'a Function {
        self.function
    }
}

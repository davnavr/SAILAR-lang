use crate::ast;

#[derive(thiserror::Error, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum Kind {
    #[error("missing directive {0}")]
    MissingDirective(&'static str),
    #[error("duplicate directive")]
    DuplicateDirective,
    #[error("invalid format version")]
    InvalidFormatVersion,
    #[error("a register with the symbol %{0} was not defined")]
    UndefinedRegister(ast::Identifier),
    #[error("expected integer type, but got {0}")]
    InvalidIntegerType(ast::PrimitiveType),
    #[error("{0} is not a valid constant integer type, pointer-sized integers are not allowed")]
    InvalidConstantIntegerType(ast::PrimitiveType),
    #[error("{1} is not a valid value for constant integers of type {0}")]
    ConstantIntegerOutOfRange(ast::PrimitiveType, i128),
    #[error("expected {expected} return registers but got {actual}")]
    InvalidReturnRegisterCount { expected: usize, actual: usize },
    #[error("a register with the name %{name} already exists")]
    DuplicateRegister {
        name: ast::Identifier,
        original: ast::Position,
    },
    #[error("a code block with the name ${name} already exists")]
    DuplicateBlock {
        name: ast::Identifier,
        original: ast::Position,
    },
    #[error("a set of registers to use for this phi instruction already exists for block ${0}")]
    DuplicatePhiTargetBlock(ast::Identifier),
    #[error("a branch already exists for value {0}")]
    DuplicateSwitchBranch(i128),
    #[error("a {kind} with the name @{name} already exists")]
    DuplicateDeclaration {
        kind: &'static str,
        name: ast::Identifier,
        original: ast::Position,
    },
    #[error("unable to find declaration corresponding to the symbol @{0}")]
    UndefinedGlobal(ast::Identifier),
    #[error("a block with the name ${0} was not defined")]
    UndefinedBlock(ast::Identifier),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Error {
    kind: Kind,
    location: Option<ast::Position>,
}

impl Error {
    pub(crate) fn new(kind: Kind, location: Option<ast::Position>) -> Self {
        Self { kind, location }
    }

    pub(crate) fn with_location(kind: Kind, location: ast::Position) -> Self {
        Self::new(kind, Some(location))
    }

    pub fn kind(&self) -> &Kind {
        &self.kind
    }

    pub fn location(&self) -> Option<&ast::Position> {
        self.location.as_ref()
    }
}

impl From<Kind> for Error {
    fn from(kind: Kind) -> Self {
        Self::new(kind, None)
    }
}

pub(crate) struct Builder {
    errors: Vec<Error>,
}

impl Builder {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn push(&mut self, error: Error) {
        self.errors.push(error)
    }

    pub fn push_with_location(&mut self, kind: Kind, location: ast::Position) {
        self.push(Error::with_location(kind, location))
    }

    pub fn drain_to_vec(&mut self) -> Vec<Error> {
        std::mem::take(&mut self.errors)
    }

    pub fn add_result<T>(&mut self, value: std::result::Result<T, Error>) -> Option<T> {
        match value {
            Ok(value) => Some(value),
            Err(error) => {
                self.push(error);
                None
            }
        }
    }
}

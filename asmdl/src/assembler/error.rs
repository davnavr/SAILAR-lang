use crate::ast;

#[derive(Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum ErrorKind {
    MissingDirective(&'static str),
    DuplicateDirective,
    InvalidFormatVersion,
    UndefinedRegister(ast::Identifier),
    InvalidConstantIntegerType(ast::PrimitiveType),
    ConstantIntegerOutOfRange(ast::PrimitiveType, i128),
    InvalidReturnRegisterCount {
        expected: usize,
        actual: usize,
    },
    DuplicateRegister {
        name: ast::Identifier,
        original: ast::Position,
    },
    DuplicateBlock {
        name: ast::Identifier,
        original: ast::Position,
    },
    DuplicateDeclaration {
        kind: &'static str,
        name: ast::Identifier,
        original: ast::Position,
    },
    DuplicateSymbol {
        symbol: ast::Identifier,
        original: ast::Position,
    },
    UndefinedGlobal(ast::Identifier),
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingDirective(description) => {
                f.write_str("missing directive: ")?;
                f.write_str(description)
            }
            Self::DuplicateDirective => f.write_str("duplicate directive"),
            Self::InvalidFormatVersion => f.write_str("invalid format version"),
            Self::UndefinedRegister(name) => {
                write!(f, "a register with the symbol %{} was not defined", name)
            }
            Self::InvalidConstantIntegerType(kind) => {
                write!(f, "{} is not a valid constant integer type", kind)
            }
            Self::ConstantIntegerOutOfRange(integer_type, value) => {
                write!(
                    f,
                    "{} is not a valid value for constant integers of type {}",
                    value, integer_type
                )
            }
            Self::InvalidReturnRegisterCount { expected, actual } => write!(
                f,
                "expected {} return registers but got {}",
                expected, actual
            ),
            Self::DuplicateRegister { name, .. } => {
                write!(f, "a register with the name %{} already exists", name)
            }
            Self::DuplicateBlock { name, .. } => {
                write!(f, "a code block with the name ${} already exists", name)
            }
            Self::DuplicateDeclaration { name, kind, .. } => {
                write!(f, "a {} with the name @{} already exists", kind, name)
            }
            Self::DuplicateSymbol { symbol, .. } => write!(
                f,
                "a declaration with the symbol \"{}\" already exists",
                symbol
            ),
            Self::UndefinedGlobal(symbol) => write!(
                f,
                "unable to find declaration corresponding to the symbol @{}",
                symbol
            ),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Error {
    kind: ErrorKind,
    location: Option<ast::Position>,
}

impl Error {
    pub(crate) fn new(kind: ErrorKind, location: Option<ast::Position>) -> Self {
        Self { kind, location }
    }

    pub(crate) fn with_location(kind: ErrorKind, location: ast::Position) -> Self {
        Self::new(kind, Some(location))
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn location(&self) -> Option<&ast::Position> {
        self.location.as_ref()
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Self {
        Self::new(kind, None)
    }
}

pub(crate) struct Builder {
    errors: Vec<Error>,
}

impl Builder {
    pub(crate) fn new() -> Self {
        Self { errors: Vec::new() }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub(crate) fn push(&mut self, error: Error) {
        self.errors.push(error)
    }

    pub(crate) fn push_with_location(&mut self, kind: ErrorKind, location: ast::Position) {
        self.push(Error::with_location(kind, location))
    }

    pub(crate) fn into_vec(&mut self) -> Vec<Error> {
        std::mem::replace(&mut self.errors, Vec::new())
    }
}

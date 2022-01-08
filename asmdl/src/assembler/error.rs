use crate::ast;

#[derive(Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum ErrorKind {
    MissingDirective(&'static str),
    DuplicateDirective,
    InvalidFormatVersion,
    InvalidConstantIntegerType(ast::PrimitiveType),
    DuplicateRegister {
        name: ast::Identifier,
        original: ast::Position,
    },
    DuplicateBlock {
        name: ast::Identifier,
        original: ast::Position,
    },
    DuplicateCode {
        name: ast::Identifier,
        original: ast::Position,
    },
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
            Self::InvalidConstantIntegerType(kind) => {
                write!(f, "{} is not a valid constant integer type", kind)
            }
            Self::DuplicateRegister { name, .. } => {
                write!(f, "a register with the name %{} already exists", name)
            }
            Self::DuplicateBlock { name, .. } => {
                write!(f, "a code block with the name ${} already exists", name)
            }
            Self::DuplicateCode { name, .. } => {
                write!(
                    f,
                    "a code declaration with the name @{} already exists",
                    name
                )
            }
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

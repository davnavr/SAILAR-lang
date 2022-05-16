//! Library for parsing and assembling the SAILAR text format.
//!
//! For a quick overview of the basic syntax and available directives, see [`ast::Directive`].

pub mod assembler;
pub mod ast;
pub mod lexer;
pub mod parser;

#[derive(Clone, Debug, thiserror::Error)]
pub enum AnyErrorKind {
    #[error(transparent)]
    Parser(#[from] parser::ErrorKind),
    #[error(transparent)]
    Assembler(#[from] assembler::ErrorKind),
}

/// Represents an error that occured at any point during assembly.
#[derive(Clone, Debug, thiserror::Error)]
#[error("{kind}")]
pub struct AnyError {
    kind: AnyErrorKind,
    location: Option<ast::LocationRange>,
}

impl AnyError {
    #[inline]
    pub fn kind(&self) -> &AnyErrorKind {
        &self.kind
    }

    /// The location, if any, where this error occured in the source code.
    pub fn location(&self) -> Option<&ast::LocationRange> {
        self.location.as_ref()
    }
}

impl From<&parser::Error> for AnyError {
    fn from(error: &parser::Error) -> Self {
        Self {
            kind: error.kind().clone().into(),
            location: Some(error.location().clone()),
        }
    }
}

impl From<&assembler::Error> for AnyError {
    fn from(error: &assembler::Error) -> Self {
        Self {
            kind: error.kind().clone().into(),
            location: error.location().cloned(),
        }
    }
}

fn extend_errors_from_slice<'e, E>(errors: &mut Vec<AnyError>, other: &'e [E])
where
    AnyError: From<&'e E>,
{
    errors.reserve_exact(other.len());
    for e in other.iter() {
        errors.push(e.into());
    }
}

pub fn assemble(input: &str) -> Result<sailar::binary::Builder, Vec<AnyError>> {
    let mut errors = Vec::default();
    let tokens = lexer::tokenize(input);
    let tree = parser::parse(&tokens);

    extend_errors_from_slice(&mut errors, tree.errors());

    match assembler::assemble(&tree) {
        Ok(module) if errors.is_empty() => return Ok(module),
        Ok(_) => (),
        Err(e) => extend_errors_from_slice(&mut errors, e.as_slice()),
    }

    errors.sort_by_key(|e| e.location().cloned());
    Err(errors)
}

//! Library for parsing and assembling the SAILAR text format.
//!
//! For a quick overview of the basic syntax and available directives, see [`ast::Directive`].

use std::collections::BTreeSet;
use std::ops::Range;

pub mod ast;
pub mod lexer;
pub mod parser;

#[derive(Clone, Debug, thiserror::Error)]
pub enum AnyErrorKind {
    #[error(transparent)]
    Parser(#[from] parser::ErrorKind),
    //Assembler(assembler::error),
}

/// Represents an error that occured at any point during assembly.
#[derive(Clone, Debug)]
pub struct AnyError {
    kind: Box<AnyErrorKind>,
    location: Range<ast::Location>,
}

impl AnyError {
    #[inline]
    pub fn kind(&self) -> &AnyErrorKind {
        &self.kind
    }

    #[inline]
    pub fn location(&self) -> &Range<ast::Location> {
        &self.location
    }
}

impl std::fmt::Display for AnyError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        ast::fmt_location_range(&self.location, f)?;
        f.write_str(": ")?;
        std::fmt::Display::fmt(&self.kind, f)
    }
}

impl From<&parser::Error> for AnyError {
    fn from(error: &parser::Error) -> Self {
        Self {
            kind: Box::new(error.kind().clone().into()),
            location: error.location().clone(),
        }
    }
}

fn extend_errors_from_slice<'e, E>(errors: &mut Vec<AnyError>, other: &'e [E]) where AnyError: From<&'e E> {
    errors.reserve_exact(other.len());
    for e in other.iter() {
        errors.push(e.into());
    }
}

pub fn assemble(input: &str) -> Result<(), Vec<AnyError>> {
    let mut errors = Vec::default();
    let tokens = lexer::tokenize(input);
    let tree = parser::parse(&tokens);

    extend_errors_from_slice(&mut errors, tree.errors());

    if errors.is_empty() {
        todo!("do the assembly")
    } else {
        errors.sort_by_key(|e| e.location());
        Err(errors)
    }
}

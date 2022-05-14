//! Transfers a sequence of tokens into an abstract syntax tree.

use crate::ast;
use crate::lexer::{self, Token};
use std::fmt::{Display, Formatter};
use std::ops::Range;

#[derive(Clone, Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("unknown token")]
    UnknownToken,
}

#[derive(Clone, Debug, thiserror::Error)]
pub struct Error {
    kind: Box<ErrorKind>,
    location: Range<ast::Location>,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Display::fmt(&self.location.start, f)?;

        if self.location.end > self.location.start {
            f.write_str(" - ")?;
            Display::fmt(&self.location.end, f)?;
        }

        f.write_str(": ")?;
        Display::fmt(&self.kind, f)
    }
}

impl Error {
    pub fn new<K: Into<ErrorKind>>(kind: K, location: Range<ast::Location>) -> Self {
        Self {
            kind: Box::new(kind.into()),
            location,
        }
    }

    #[inline]
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    #[inline]
    pub fn location(&self) -> &Range<ast::Location> {
        &self.location
    }
}

#[derive(Debug)]
struct Input<'o, 's, S> {
    source: S,
    locations: &'o lexer::OffsetMap<'s>,
}

impl<'o, 's, S: std::iter::Iterator<Item = &'o (Token<'s>, Range<usize>)>> Input<'o, 's, S> {
    fn next_token(&mut self) -> Option<(&'o Token<'s>, Range<ast::Location>)> {
        let (token, offsets) = self.source.next()?;
        Some((
            token,
            Range {
                start: self.locations.get_location(offsets.start).unwrap(),
                end: self.locations.get_location(offsets.end).unwrap(),
            },
        ))
    }
}

#[derive(Debug)]
pub struct Output<'s> {
    tree: Vec<ast::Located<ast::Directive<'s>>>,
    errors: Vec<Error>,
}

impl<'s> Output<'s> {
    #[inline]
    pub fn tree(&self) -> &[ast::Located<ast::Directive<'s>>] {
        &self.tree
    }

    #[inline]
    pub fn errors(&self) -> &[Error] {
        &self.errors
    }
}

pub fn parse<'s>(input: &lexer::Output<'s>) -> Output<'s> {
    let mut input = Input {
        source: input.tokens().into_iter(),
        locations: input.locations(),
    };

    let mut tree = Vec::default();
    let mut errors = Vec::default();

    while let Some((token, location)) = input.next_token() {
        match token {
            Token::FormatDirective => {
                todo!("parse")
            }
            Token::Unknown => errors.push(Error::new(ErrorKind::UnknownToken, location.clone())),
        }
    }

    Output { tree, errors }
}

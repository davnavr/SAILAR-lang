//! Transfers a sequence of tokens into an abstract syntax tree.

use crate::ast;
use crate::lexer::{self, Token};
use std::ops::Range;

#[derive(Debug)]
struct Input<'o, 's, S> {
    source: S,
    locations: &'o lexer::OffsetMap<'s>,
}

impl<'s, S: std::iter::Iterator<Item = (Token<'s>, Range<usize>)>> Input<'_, 's, S> {
    fn next_token(&mut self) -> Option<(Token<'s>, Range<ast::Location>)> {
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

pub type Result<T> = std::result::Result<T, ()>;

pub fn parse<'s>(input: &lexer::Output<'s>) -> Result<Vec<ast::Located<ast::Directive<'s>>>> {
    let mut input = Input {
        source: input.tokens().into_iter(),
        locations: input.locations(),
    };
    let mut directives = Vec::default();
    todo!("parse the nodes");
    Ok(directives)
}

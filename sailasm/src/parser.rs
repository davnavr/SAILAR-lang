//! Transfers a sequence of tokens into an abstract syntax tree.

use crate::ast;
use crate::lexer::{self, Token};
use std::ops::Range;

#[derive(Debug)]
struct Input<'s, S> {
    source: S,
    offset_map: lexer::OffsetMap<'s>,
}

impl<'s, S: std::iter::Iterator<Item = (Token<'s>, Range<usize>)>> Input<'s, S> {
    fn next_token(&mut self) -> Option<(Token<'s>, Range<ast::Location>)> {
        let (token, offsets) = self.source.next()?;
        Some((
            token,
            Range {
                start: self.offset_map.get_location(offsets.start).unwrap(),
                end: self.offset_map.get_location(offsets.end).unwrap(),
            },
        ))
    }
}

pub type Result<T> = std::result::Result<T, ()>;

pub fn parse<'s, T: IntoIterator<Item = (Token<'s>, Range<usize>)>>(
    tokens: T,
    offset_map: lexer::OffsetMap<'s>,
) -> Result<Vec<ast::Located<ast::Directive<'s>>>> {
    let mut input = Input {
        source: tokens.into_iter(),
        offset_map,
    };
    let mut directives = Vec::default();
    todo!("parse the nodes");
    Ok(directives)
}

//! Transfers a sequence of tokens into an abstract syntax tree.

use crate::ast;
use crate::lexer::{self, Token};
use std::fmt::{Display, Formatter};
use std::ops::Range;

#[derive(Clone, Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("unknown token")]
    UnknownToken,
    #[error("{0} is not a valid format version kind")]
    InvalidFormatVersionKind(String),
    #[error("expected format version kind")]
    ExpectedFormatVersionKind,
    #[error("expected integer format version")]
    ExpectedFormatVersion,
    #[error("invalid format version: {0}")]
    InvalidFormatVersion(std::num::ParseIntError),
}

#[derive(Clone, Debug, thiserror::Error)]
pub struct Error {
    kind: Box<ErrorKind>,
    location: Range<ast::Location>,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        ast::fmt_location_range(&self.location, f)?;
        f.write_str(": ")?;
        Display::fmt(&self.kind, f)
    }
}

impl Error {
    pub fn new<K: Into<ErrorKind>, L: Into<Range<ast::Location>>>(kind: K, location: L) -> Self {
        Self {
            kind: Box::new(kind.into()),
            location: location.into(),
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

macro_rules! fail {
    ($errors: expr, $kind: expr, $location: expr) => {{
        $errors.push(Error::new($kind, $location));
        continue;
    }};
}

macro_rules! result {
    ($errors: expr, $value: expr, $location: expr) => {
        match $value {
            Ok(v) => v,
            Err(e) => fail!($errors, ErrorKind::from(e), $location),
        }
    };
}

macro_rules! match_exhausted {
    ($errors: expr, $kind: expr, $token: expr, $input: expr) => {
        fail!(
            $errors,
            $kind,
            if let Some((_, location)) = $token {
                location
            } else {
                $input.locations.get_last().unwrap().into()
            }
        )
    };
}

pub fn parse<'s>(input: &lexer::Output<'s>) -> Output<'s> {
    let mut input = Input {
        source: input.tokens().into_iter(),
        locations: input.locations(),
    };

    let mut tree = Vec::default();
    let mut errors = Vec::default();

    while let Some((token, location)) = input.next_token() {
        let start_location = location.start.clone();
        let end_location;

        match token {
            Token::ArrayDirective => tree.push(ast::Located::new(ast::Directive::Array, start_location, location.end)),
            Token::FormatDirective => {
                let format_kind = match input.next_token() {
                    Some((Token::Word("major"), _)) => ast::FormatVersionKind::Major,
                    Some((Token::Word("minor"), _)) => ast::FormatVersionKind::Minor,
                    Some((Token::Word(bad), location)) => {
                        fail!(errors, ErrorKind::InvalidFormatVersionKind(bad.to_string()), location)
                    }
                    bad => match_exhausted!(errors, ErrorKind::ExpectedFormatVersionKind, bad, input),
                };

                let format_version = match input.next_token() {
                    Some((Token::LiteralInteger(digits), location)) => {
                        end_location = location.end.clone();
                        result!(
                            errors,
                            u8::try_from(digits).map_err(ErrorKind::InvalidFormatVersion),
                            location
                        )
                    }
                    bad => match_exhausted!(errors, ErrorKind::ExpectedFormatVersion, bad, input),
                };

                // TODO: Have helper/macro that checks for newline or EOF

                tree.push(ast::Located::new(
                    ast::Directive::Format(format_kind, format_version),
                    start_location,
                    end_location,
                ));
            }
            Token::Unknown => fail!(errors, ErrorKind::UnknownToken, location),
            Token::Newline => (),
            bad => todo!("handle {:?}", bad),
        }
    }

    Output { tree, errors }
}

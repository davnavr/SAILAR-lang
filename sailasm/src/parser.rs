//! Transfers a sequence of tokens into an abstract syntax tree.

use crate::ast;
use crate::lexer::{self, Token};
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
#[error("{location}: {kind}")]
pub struct Error {
    kind: Box<ErrorKind>,
    location: ast::LocationRange,
}

impl Error {
    pub fn new<K: Into<ErrorKind>, L: Into<ast::LocationRange>>(kind: K, location: L) -> Self {
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
    pub fn location(&self) -> &ast::LocationRange {
        &self.location
    }
}

#[derive(Debug)]
struct Input<'o, 's, S> {
    source: S,
    locations: &'o lexer::OffsetMap<'s>,
}

impl<'o, 's, S: std::iter::Iterator<Item = &'o (Token<'s>, Range<usize>)>> Input<'o, 's, S> {
    fn next_token(&mut self) -> Option<(&'o Token<'s>, ast::LocationRange)> {
        let (token, offsets) = self.source.next()?;
        Some((
            token,
            ast::LocationRange::new(
                self.locations.get_location(offsets.start).unwrap(),
                self.locations.get_location(offsets.end).unwrap(),
            ),
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
macro_rules! push_error {
    ($errors: expr, $kind: expr, $location: expr) => {
        $errors.push(Error::new($kind, $location))
    };
}

macro_rules! fail_continue {
    ($errors: expr, $kind: expr, $location: expr) => {{
        push_error!($errors, $kind, $location);
        continue;
    }};
}

macro_rules! fail_skip_line {
    ($errors: expr, $kind: expr, $location: expr, $input: expr) => {{
        push_error!($errors, $kind, $location);

        loop {
            match $input.next_token() {
                Some((Token::Newline, _)) | None => break,
                Some(_) => (),
            }
        }

        continue;
    }};
}

macro_rules! match_exhausted {
    ($errors: expr, $kind: expr, $token: expr, $input: expr) => {
        fail_continue!(
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
        source: input.tokens().iter(),
        locations: input.locations(),
    };

    let mut tree = Vec::default();
    let mut errors = Vec::default();

    while let Some((token, location)) = input.next_token() {
        let start_location = location.start().clone();
        let end_location;

        match token {
            Token::ArrayDirective => tree.push(ast::Located::new(
                ast::Directive::Array,
                start_location,
                location.end().clone(),
            )),
            Token::FormatDirective => {
                let format_kind = match input.next_token() {
                    Some((Token::Word("major"), _)) => ast::FormatVersionKind::Major,
                    Some((Token::Word("minor"), _)) => ast::FormatVersionKind::Minor,
                    Some((Token::Word(bad), location)) => {
                        fail_skip_line!(errors, ErrorKind::InvalidFormatVersionKind(bad.to_string()), location, input)
                    }
                    bad => match_exhausted!(errors, ErrorKind::ExpectedFormatVersionKind, bad, input),
                };

                let format_version = match input.next_token() {
                    Some((Token::LiteralInteger(digits), location)) => {
                        end_location = location.end().clone();
                        match u8::try_from(digits) {
                            Ok(version) => version,
                            Err(e) => fail_skip_line!(errors, ErrorKind::InvalidFormatVersion(e), location, input),
                        }
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
            Token::Unknown => fail_continue!(errors, ErrorKind::UnknownToken, location),
            Token::Newline => (),
            bad => todo!("handle {:?}, {:?}", bad, &errors),
        }
    }

    Output { tree, errors }
}

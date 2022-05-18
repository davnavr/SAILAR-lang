//! Provides functions for parsing SAILAR assembly.

use crate::ast;
use crate::lexer::{self, Token};
use std::iter::Iterator;
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
    #[error("expected end of line or file")]
    ExpectedNewLineOrEndOfFile,
    #[error("expected literal contents of identifier")]
    ExpectedIdentifierLiteral,
    #[error("\"\\{0}\" is not a valid escape sequence")]
    InvalidEscapeSequence(Box<str>),
    #[error(transparent)]
    InvalidIdentifierLiteral(#[from] sailar::identifier::InvalidError),
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("{kind}")]
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

struct Input<'o, 's, S: Iterator> {
    source: std::iter::Peekable<S>,
    locations: &'o lexer::OffsetMap<'s>,
}

type SliceInput<'o, 's> = Input<'o, 's, std::slice::Iter<'o, (Token<'s>, Range<usize>)>>;

type LocatedToken<'o, 's> = (&'o (Token<'s>, Range<usize>), ast::LocationRange);

impl<'o, 's, S: Iterator<Item = &'o (Token<'s>, Range<usize>)>> Input<'o, 's, S> {
    fn new<I: std::iter::IntoIterator<IntoIter = S>>(source: I, locations: &'o lexer::OffsetMap<'s>) -> Self {
        Self {
            source: source.into_iter().peekable(),
            locations,
        }
    }

    fn token_from_offsets(token: Option<&S::Item>, locations: &'o lexer::OffsetMap<'s>) -> Option<LocatedToken<'o, 's>> {
        token.map(|lexer_token| {
            let (_, offsets) = lexer_token;
            (
                *lexer_token,
                ast::LocationRange::new(
                    locations.get_location(offsets.start).unwrap(),
                    locations.get_location(offsets.end).unwrap(),
                ),
            )
        })
    }

    fn next_token(&mut self) -> Option<LocatedToken<'o, 's>> {
        Self::token_from_offsets(self.source.next().as_ref(), self.locations)
    }

    fn next_token_if<F: FnOnce(&'o Token<'s>) -> bool>(&mut self, condition: F) -> Option<LocatedToken<'o, 's>> {
        Self::token_from_offsets(self.source.next_if(|(token, _)| condition(token)).as_ref(), self.locations)
    }

    fn peek_next_token(&mut self) -> Option<LocatedToken<'o, 's>> {
        Self::token_from_offsets(self.source.peek(), self.locations)
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
                Some(((Token::Newline, _), _)) | None => break,
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

macro_rules! expect_new_line_or_end {
    ($errors: expr, $input: expr) => {
        match $input.peek_next_token() {
            Some(((Token::Newline, _), _)) | None => (),
            Some((_, location)) => {
                let start_location = location.start();
                let mut end_location = location.end().clone();

                while let Some((_, location)) = $input.next_token_if(|next| !matches!(next, Token::Newline)) {
                    end_location = location.end().clone();
                }

                fail_continue!(
                    $errors,
                    ErrorKind::ExpectedNewLineOrEndOfFile,
                    ast::LocationRange::new(start_location.clone(), end_location)
                );
            }
        }
    };
}

/// Transfers a sequence of tokens into an abstract syntax tree.
pub fn parse<'s>(input: &lexer::Output<'s>) -> Output<'s> {
    let mut input = SliceInput::new(input.tokens(), input.locations());
    let mut character_buffer = String::default();
    let mut tree = Vec::default();
    let mut errors = Vec::default();

    while let Some(((token, _), location)) = input.next_token() {
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
                    Some(((Token::Word("major"), _), _)) => ast::FormatVersionKind::Major,
                    Some(((Token::Word("minor"), _), _)) => ast::FormatVersionKind::Minor,
                    Some(((Token::Word(bad), _), location)) => {
                        fail_skip_line!(errors, ErrorKind::InvalidFormatVersionKind(bad.to_string()), location, input)
                    }
                    bad => match_exhausted!(errors, ErrorKind::ExpectedFormatVersionKind, bad, input),
                };

                let format_version = match input.next_token() {
                    Some(((Token::LiteralInteger(digits), _), location)) => {
                        end_location = location.end().clone();
                        match u8::try_from(digits) {
                            Ok(version) => version,
                            Err(e) => fail_skip_line!(errors, ErrorKind::InvalidFormatVersion(e), location, input),
                        }
                    }
                    bad => match_exhausted!(errors, ErrorKind::ExpectedFormatVersion, bad, input),
                };

                expect_new_line_or_end!(errors, input);

                tree.push(ast::Located::new(
                    ast::Directive::Format(format_kind, format_version),
                    start_location,
                    end_location,
                ));
            }
            Token::IdentifierDirective => {
                let symbol = None;

                let literal_start_location;
                let literal = match input.next_token() {
                    Some(((Token::LiteralString(contents), literal_offset), location)) => {
                        match ast::LiteralString::with_escape_sequences(contents, &mut character_buffer) {
                            Ok(literal) => {
                                literal_start_location = location.start();
                                end_location = location.end().clone();
                                literal
                            }
                            Err(e) => {
                                let escape_start_location = literal_offset.start + 1 + e.byte_offset();
                                let escape_end_location = escape_start_location + e.sequence().len() + 1;
                                fail_skip_line!(
                                    errors,
                                    ErrorKind::InvalidEscapeSequence(e.take_sequence()),
                                    ast::LocationRange::new(
                                        input.locations.get_location(escape_start_location).unwrap(),
                                        input.locations.get_location(escape_end_location).unwrap()
                                    ),
                                    input
                                );
                            }
                        }
                    }
                    bad => match_exhausted!(errors, ErrorKind::ExpectedIdentifierLiteral, bad, input),
                };

                expect_new_line_or_end!(errors, input);

                let identifier = match literal.try_into_identifier() {
                    Ok(identifier) => identifier,
                    Err(e) => fail_skip_line!(
                        errors,
                        e,
                        ast::LocationRange::new(literal_start_location.clone(), end_location),
                        input
                    ),
                };

                tree.push(ast::Located::new(
                    ast::Directive::Identifier(symbol, identifier),
                    start_location,
                    end_location,
                ));
            }
            Token::Newline => (),
            Token::Unknown | _ => fail_continue!(errors, ErrorKind::UnknownToken, location),
        }
    }

    Output { tree, errors }
}
